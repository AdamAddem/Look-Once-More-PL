#include "tollvm.hpp"
#include "parsing/ast.hpp"
#include "build_system/build.hpp"
#include "error.hpp"
#include "peepir/peepir.hpp"
#include "settings.hpp"

#include <filesystem>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>
#include <chrono>
using namespace LOM;

namespace {

struct TU : Backend {
  llvm::LLVMContext context;
  llvm::Module module;

  explicit TU(std::string_view file_path)
  : module(file_path, context) {}

  void printModule(llvm::raw_ostream& out = llvm::outs()) const
  { module.print(out, nullptr); }

  [[nodiscard]] std::filesystem::path
  createASMFile(std::filesystem::path const& output_path) override {
    static constexpr std::string_view asm_folder = "build/asm/";
    std::filesystem::path asm_path = asm_folder;
    asm_path.append(output_path.string());
    std::filesystem::create_directories(asm_path.parent_path());
#ifdef _WIN32
    asm_path.replace_extension(".asm");
#else
    asm_path.replace_extension(".s");
#endif

    createFile(asm_path, llvm::CodeGenFileType::AssemblyFile);
    return asm_path;
  }

  [[nodiscard]] std::filesystem::path
  createIRFile(std::filesystem::path const& output_path) override {
    static constexpr std::string_view ir_path = "build/llvm_ir/";

    std::filesystem::path file_path = ir_path;
    file_path.append(output_path.string());
    std::filesystem::create_directories(file_path.parent_path());
    file_path.replace_extension(".ll");

    std::error_code EC;
    llvm::raw_fd_ostream dest(file_path.string(), EC, llvm::sys::fs::OF_None);
    if (EC) {
      llvm::errs() << "Could not open file: " << file_path.string() << " | " << EC.message() << '\n';
      std::quick_exit(1);
    }

    printModule(dest);
    return file_path;
  }

  [[nodiscard]] std::filesystem::path
  createObjectFile(std::filesystem::path const& output_path) override {
    static constexpr std::string_view object_folder = "build/obj/";

    std::filesystem::path obj_path = object_folder;
    obj_path.append(output_path.string());
    std::filesystem::create_directories(obj_path.parent_path());
#ifdef _WIN32
    obj_path.replace_extension(".obj");
#else
    obj_path.replace_extension(".o");
#endif

    createFile(obj_path, llvm::CodeGenFileType::ObjectFile);
    return obj_path;
  }

  void createFile(std::filesystem::path const& output_path, llvm::CodeGenFileType filetype) {
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    static const llvm::Triple target_triple{llvm::sys::getDefaultTargetTriple()};
    module.setTargetTriple(target_triple);
    std::string err;
    auto const target = llvm::TargetRegistry::lookupTarget(module.getTargetTriple(), err);
    if (not target) {
      llvm::errs() << err;
      std::quick_exit(1);
    }

    static constexpr std::string_view CPU = "generic";
    static constexpr std::string_view Features = "";
    static const llvm::TargetOptions opt;
    auto const target_machine = target->createTargetMachine(
      target_triple, CPU, Features, opt, llvm::Reloc::PIC_);
    module.setDataLayout(target_machine->createDataLayout());

    std::error_code EC;
    llvm::raw_fd_ostream dest(output_path.string(), EC, llvm::sys::fs::OF_None);
    if (EC) {
      llvm::errs() << "Could not open file: " << output_path.string() << " | " << EC.message() << '\n';
      assert(false);
    }

    llvm::legacy::PassManager pass;
    if (target_machine->addPassesToEmitFile(pass, dest, nullptr, filetype)) {
      llvm::errs() << "Target machine can't emit a file of this type";
      assert(false);
    }

    pass.run(module);
    dest.flush();
    delete target_machine;
  }
};

class Lowerer final {
  friend struct Function;
  TU* tu;

  llvm::IntegerType* i1;
  llvm::IntegerType* i8; llvm::Value* bool_true; llvm::Value* bool_false;
  llvm::IntegerType* i16; llvm::IntegerType* i32; llvm::IntegerType* i64;
  llvm::Type* f32; llvm::Type* f64; llvm::Type* devoid; llvm::PointerType* ptr;

  std::unordered_map<Type const*, llvm::Type*> type_map;
  std::unordered_map<SymbolTable::Function const*, llvm::Value*> imports;

  /* Variables used when lowering a function */
  llvm::Type* return_type;
  llvm::Function* llvmfunc;
  llvm::IRBuilder<> builder;
  std::vector<llvm::AllocaInst*> locals;

  std::vector<PeepMIR::Instruction> instructions; sz_t instruction_idx{};
  std::vector<PeepMIR::Block> mir_blocks;
  std::vector<llvm::BasicBlock*> llvm_blocks;
  /* Variables used when lowering a function */

  [[nodiscard]] llvm::Value*
  getFunctionImport(StabilizedModule imported_module, u16_t function_id) {
    auto const function = imported_module.getFunction(function_id); assume_assert(function);
    auto const element = imports.find(function);
    if (element not_eq imports.end()) return element->second;

    auto const module_name = imported_module.nameof();
    std::string function_name;
    if (module_name not_eq "__C") {
      function_name.append(module_name);
      function_name.push_back('.');
    }
    function_name.append(function->nameof());

    auto const function_type = translateFunctionType(function->type);

    auto const res = tu->module.getOrInsertFunction(function_name, function_type).getCallee();
    imports.emplace(function, res);
    return res;
  }

  [[nodiscard]] llvm::FunctionType*
  translateFunctionType(FunctionType const* func_type) noexcept {
    auto const element = type_map.find(func_type);
    if (element not_eq type_map.end()) return llvm::cast<llvm::FunctionType>(element->second);

    llvm::Type* arg_types[Settings::MAX_FUNCTION_PARAMETERS];
    auto num_params{0uz};
    for (auto const param_type : func_type->parameterTypes()) {
      assert(type_map.contains(param_type));
      arg_types[num_params] = type_map[param_type];
      ++num_params;
    }

    // hack
    auto const lom_return_type = func_type->returnType(); assert(type_map.contains(lom_return_type));
    auto const llvm_return_type =
      lom_return_type->isBool() ?
      i1 :
      type_map[lom_return_type];

    auto const res = llvm::FunctionType::get(llvm_return_type, {arg_types, num_params}, func_type->isVariadic());
    type_map.emplace(func_type, res);
    return res;
  }

  [[nodiscard]] llvm::Type*
  translateType(Type const* type) noexcept {
    auto const element = type_map.find(type);
    if (element not_eq type_map.end()) return element->second;

    assert(type->isCustom());
    auto const custom_type = type->castToCustom();
    auto const member_table = custom_type->member_table();
    member_table->orderVariableList();
    auto const& members = member_table->getVariableList();
    llvm::Type* member_types[Settings::MAX_STRUCT_MEMBER_VARIABLES];
    auto i{0uz};
    for (auto const& member : members) {
      auto* const member_type = member.type.type;
      if (member_type->isCustom()) member_types[i] = translateType(member_type->castToCustom());
      else member_types[i] = type_map[member_type];

      ++i;
    }

    auto const res = llvm::StructType::create(tu->context, llvm::ArrayRef(member_types, i), custom_type->nameof());
    type_map.emplace(type, res);
    return res;
  }

   [[nodiscard]] llvm::Constant*
  fpConstant(llvm::Type* t, double value) const noexcept
  {return llvm::ConstantFP::get(t, value);}

  [[nodiscard]] llvm::Constant*
  signedConstant(llvm::Type* t, i64_t value) const noexcept
  {return llvm::ConstantInt::get(llvm::cast<llvm::IntegerType>(t), value, true);}

  [[nodiscard]] llvm::Constant*
  unsignedConstant(llvm::Type* t, u64_t value) const noexcept
  {return llvm::ConstantInt::get(llvm::cast<llvm::IntegerType>(t), value);}

  [[nodiscard]] llvm::Value*
  genUnary(PeepMIR::Instruction::Type type) noexcept {
    llvm::Value* res{};
    switch (type) {
    using enum PeepMIR::Instruction::Type;
    case PRE_INC: {
      auto const var = llvm::cast<llvm::AllocaInst>(genRefExpression());
      auto const var_type = var->getAllocatedType();
      res = builder.CreateAdd(
        builder.CreateLoad(var_type, var),
        unsignedConstant(var_type, 1));
      builder.CreateStore(res, var);
      return var;
    }
    case FPRE_INC: {
      auto const var = llvm::cast<llvm::AllocaInst>(genRefExpression());
      auto const var_type = var->getAllocatedType();
      res = builder.CreateFAdd(
        builder.CreateLoad(var_type, var),
        fpConstant(var_type, 1));
      builder.CreateStore(res, var);
      return var;
    }
    case PRE_DEC: {
      auto const var = llvm::cast<llvm::AllocaInst>(genRefExpression());
      auto const var_type = var->getAllocatedType();
      res = builder.CreateSub(
        builder.CreateLoad(var_type, var),
        unsignedConstant(var_type, 1));
      builder.CreateStore(res, var);
      return var;
    }
    case FPRE_DEC: {
      auto const var = llvm::cast<llvm::AllocaInst>(genRefExpression());
      auto const var_type = var->getAllocatedType();
      res = builder.CreateFSub(
        builder.CreateLoad(var_type, var),
        fpConstant(var_type, 1));
      builder.CreateStore(res, var);
      return var;
    }
    case ADDRESS_OF:
      return genRefExpression();
    case NEGATE:
      return builder.CreateNeg(genValueExpression());
    case FNEGATE:
      return builder.CreateFNeg(genValueExpression());
    case BITNOT:
      return builder.CreateNot(genValueExpression());
    case POST_INC: {
      auto const var = llvm::cast<llvm::AllocaInst>(genRefExpression());
      auto const var_type = var->getAllocatedType();
      auto load = builder.CreateLoad(var_type, var);
      res = builder.CreateAdd(load, unsignedConstant(var_type, 1));
      builder.CreateStore(res, var);
      return load;
    }
    case FPOST_INC: {
      auto const var = llvm::cast<llvm::AllocaInst>(genRefExpression());
      auto const var_type = var->getAllocatedType();
      auto load = builder.CreateLoad(var_type, var);
      res = builder.CreateFAdd(load, fpConstant(var_type, 1));
      builder.CreateStore(res, var);
      return load;
    }
    case POST_DEC: {
      auto const var = llvm::cast<llvm::AllocaInst>(genRefExpression());
      auto const var_type = var->getAllocatedType();
      auto load = builder.CreateLoad(var_type, var);
      res = builder.CreateSub(load, unsignedConstant(var_type, 1));
      builder.CreateStore(res, var);
      return load;
    }
    case FPOST_DEC: {
      auto const var = llvm::cast<llvm::AllocaInst>(genRefExpression());
      auto const var_type = var->getAllocatedType();
      auto load = builder.CreateLoad(var_type, var);
      res = builder.CreateFSub(load, fpConstant(var_type, 1));
      builder.CreateStore(res, var);
      return load;
    }
    default:
      eden_unreachable("Invalid unary peep instruction type.");
    }
  }

  [[nodiscard]] llvm::Value*
  genBinary(PeepMIR::Instruction::Type type) noexcept {
    auto const left = genValueExpression();
    auto const right = genValueExpression();

    switch (type) { using enum PeepMIR::Instruction::Type;
    case ADD: return builder.CreateAdd(left, right);
    case FADD: return builder.CreateFAdd(left, right);
    case SUB: return builder.CreateSub(left, right);
    case FSUB: return builder.CreateFSub(left, right);
    case MULT: return builder.CreateMul(left, right);
    case FMULT: return builder.CreateFMul(left, right);

    case UDIV: return builder.CreateUDiv(left, right);
    case SDIV: return builder.CreateSDiv(left, right);
    case FDIV: return builder.CreateFDiv(left, right);
    case UMOD: return builder.CreateURem(left, right);
    case SMOD: return builder.CreateSRem(left, right);
    case FMOD: return builder.CreateFRem(left, right);

    case ASSIGN:
      eden_unreachable("Assign shouldn't be called here.");

    case ULESS: return builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_ULT, left, right);
    case SLESS: return builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_SLT, left, right);
    case FLESS: return builder.CreateCmp(llvm::CmpInst::Predicate::FCMP_OLT, left, right);
    case UGTR: return builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_UGT, left, right);
    case SGTR: return builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_SGT, left, right);
    case FGTR: return builder.CreateCmp(llvm::CmpInst::Predicate::FCMP_OGT, left, right);
    case ULEQ: return builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_ULE, left, right);
    case SLEQ: return builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_SLE, left, right);
    case FLEQ: return builder.CreateCmp(llvm::CmpInst::Predicate::FCMP_OLE, left, right);
    case UGEQ: return builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_UGE, left, right);
    case SGEQ: return builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_SGE, left, right);
    case FGEQ: return builder.CreateCmp(llvm::CmpInst::Predicate::FCMP_OGE, left, right);

    case AND: return builder.CreateLogicalAnd(builder.CreateTrunc(left, i1), builder.CreateTrunc(right, i1));
    case OR: return builder.CreateLogicalOr(builder.CreateTrunc(left, i1), builder.CreateTrunc(right, i1));
    case BITAND: return builder.CreateAnd(left, right);
    case BITOR: return builder.CreateOr(left, right);
    case BITXOR: return builder.CreateXor(left, right);
    case EQ:
      return builder.CreateCmp(
      left->getType()->isFloatingPointTy()
      ? llvm::CmpInst::Predicate::FCMP_OEQ
      : llvm::CmpInst::Predicate::ICMP_EQ,
      left, right);
    case NEQ:
      return builder.CreateCmp(
      left->getType()->isFloatingPointTy()
      ? llvm::CmpInst::Predicate::FCMP_ONE
      : llvm::CmpInst::Predicate::ICMP_NE,
      left, right);
    default:
      eden_unreachable("Invalid binary peep instruction type.");
    }
  }

  [[nodiscard]] llvm::Value*
  genAssign(PeepMIR::Instruction::Type type, u64_t bitwidth = 0) noexcept {
    auto const left = genRefExpression();
    llvm::Value* right;
    switch (type) {
      using enum PeepMIR::Instruction::Type;
    case ASSIGN:
      right = genValueExpression();
      break;
    case UCAST_ASSIGN:
      right = builder.CreateZExt(genValueExpression(), llvm::IntegerType::get(tu->context, bitwidth));
      break;
    case SCAST_ASSIGN:
      right = builder.CreateSExt(genValueExpression(), llvm::IntegerType::get(tu->context, bitwidth));
      break;

    default:
      eden_unreachable("Invalid assign peep instruction type.");
    }

    return builder.CreateStore(right, left);
  }

  [[nodiscard]] llvm::Value*
  genValueExpression() noexcept { // only relevant for that which can be used by value, with the addition of functions
    auto const& instruction = instructions[instruction_idx++];
    switch (instruction.type) {
      using enum PeepMIR::Instruction::Type;
    case NOOP:
      return nullptr;
    case FUNCTION: {
      auto const function = tu->module.getFunction(instruction.function_name()); assert(function);
      return function;
    }
    case LOCAL: {
      assert(instruction.local_idx() < locals.size());
      auto const local = locals[instruction.local_idx()];
      auto const local_type = local->getAllocatedType();
      return builder.CreateLoad(local_type, local);
    }

    case TYPE_VARIABLE: {
      auto const type = instruction.custom_type();
      auto const id = instruction.type_variable_id();
      auto const& member = type->member_table()->getVariable(id);
      auto const member_ptr = builder.CreateStructGEP(translateType(type), genRefExpression(), id);
      return builder.CreateLoad(translateType(member->type.type), member_ptr);
    }
    case MODULE_FUNCTION:
      return getFunctionImport(instruction.module(), instruction.module_function_id());

    case I8_LITERAL:
      return llvm::ConstantInt::get(i8, instruction.value, true);
    case I16_LITERAL:
      return llvm::ConstantInt::get(i16, instruction.value, true);
    case I32_LITERAL:
      return llvm::ConstantInt::get(i32, instruction.value, true);
    case I64_LITERAL:
      return llvm::ConstantInt::get(i64, instruction.value, true);
    case U8_LITERAL:
      return llvm::ConstantInt::get(i8, instruction.value);
    case U16_LITERAL:
      return llvm::ConstantInt::get(i16, instruction.value);
    case U32_LITERAL:
      return llvm::ConstantInt::get(i32, instruction.value);
    case U64_LITERAL:
      return llvm::ConstantInt::get(i64, instruction.value);
    case FLOAT_LITERAL:
      return llvm::ConstantFP::get(f32, instruction.float_value());
    case DOUBLE_LITERAL:
      return llvm::ConstantFP::get(f64, instruction.double_value());
    case BOOL_LITERAL:
      return instruction.bool_value() ? bool_true : bool_false;
    case CHAR_LITERAL:
      return llvm::ConstantInt::get(i8, instruction.char_value());
    case STRING_LITERAL:
      return builder.CreateGlobalString(instruction.string_value());
    case ESCAPED_STRING_LITERAL:
      return builder.CreateGlobalString(instruction.escaped_string_value());

    case ADD: case FADD:
    case SUB: case FSUB:
    case MULT: case FMULT:
    case UDIV: case SDIV: case FDIV:
    case UMOD: case SMOD: case FMOD:
    case ULESS: case SLESS: case FLESS:
    case UGTR: case SGTR: case FGTR:
    case ULEQ: case SLEQ: case FLEQ:
    case UGEQ: case SGEQ: case FGEQ:
    case EQ:
    case NEQ:
    case AND:
    case OR:
    case BITAND:
    case BITOR:
    case BITXOR:
      return genBinary(instruction.type);

    case ASSIGN:
      return genAssign(instruction.type);
    case UCAST_ASSIGN: case SCAST_ASSIGN:
      return genAssign(instruction.type, instruction.cast_assign_bitwidth());

    case PRE_INC:
    case FPRE_INC:
    case PRE_DEC:
    case FPRE_DEC: {
      auto const var = llvm::cast<llvm::AllocaInst>(genUnary(instruction.type));
      return builder.CreateLoad(var->getAllocatedType(), var);
    }

    case ADDRESS_OF:
    case NEGATE: case FNEGATE:
    case BITNOT:
    case POST_INC: case FPOST_INC:
    case POST_DEC: case FPOST_DEC:
      return genUnary(instruction.type);
    case DEREFERENCE:
      return builder.CreateLoad(
        translateType(instruction.dereference_type()),
        genValueExpression());

    case UCAST: {
      auto const dest_type = instruction.cast_type();
      auto const llvm_dest_type = translateType(dest_type);
      if (dest_type->isFloating())
        return builder.CreateUIToFP(genValueExpression(), llvm_dest_type);
      return builder.CreateZExtOrTrunc(genValueExpression(), llvm_dest_type);
    }
    case SCAST: {
      auto const dest_type = instruction.cast_type();
      auto const llvm_dest_type = translateType(dest_type);
      if (dest_type->isFloating())
        return builder.CreateSIToFP(genValueExpression(), llvm_dest_type);
      return builder.CreateSExtOrTrunc(genValueExpression(), llvm_dest_type);
    }
    case FCAST: {
      auto const dest_type = instruction.cast_type();
      auto const llvm_dest_type = translateType(dest_type);
      if (dest_type->isIntegral()) {
        auto const primitive_dest_type = dest_type->castToPrimitive();
        if (primitive_dest_type->isSignedIntegral())
          return builder.CreateFPToSI(genValueExpression(), llvm_dest_type);
        return builder.CreateSIToFP(genValueExpression(), llvm_dest_type);
      }
      return builder.CreateFPCast(genValueExpression(), llvm_dest_type);
    }
    case PCAST:
      return genValueExpression();

    case CALL: {
      auto const fn = genRefExpression();
      llvm::Value* parameters[Settings::MAX_FUNCTION_PARAMETERS];
      for (auto i{0uz}; i<instruction.num_params(); ++i)
        parameters[i] = genValueExpression();

      return builder.CreateCall(
        llvm::cast<llvm::Function>(fn),
        {parameters, instruction.num_params()}
        );
    }

    default:
      eden_unreachable("Invalid peep instruction type.");
    }
  }

  [[nodiscard]] llvm::Value*
  genRefExpression() noexcept { // only relevant for that which can be referenced
    auto const& instruction = instructions[instruction_idx++];
    switch (instruction.type) {
    using enum PeepMIR::Instruction::Type;
    case FUNCTION: {
      auto const function = tu->module.getFunction(instruction.function_name()); assert(function);
      return function;
    }
    case MODULE_FUNCTION:
      return getFunctionImport(instruction.module(), instruction.module_function_id());

    case LOCAL:
      assert(instruction.local_idx() < locals.size());
      return locals[instruction.local_idx()];


    case TYPE_VARIABLE: {
      auto const type = instruction.custom_type();
      auto const id = instruction.type_variable_id();
      return builder.CreateStructGEP(translateType(type), genRefExpression(), id);
    }


    case PRE_INC: case FPRE_INC:
    case PRE_DEC: case FPRE_DEC:
      return genUnary(instruction.type);
    case ASSIGN: {
      auto const left = genRefExpression();
      auto const right = genValueExpression();
      return builder.CreateStore(right, left);
    }
    case UCAST_ASSIGN: {
      auto const left = genRefExpression();
      auto const right = builder.CreateZExt(genValueExpression(), llvm::IntegerType::get(tu->context, instruction.cast_assign_bitwidth()));
      return builder.CreateStore(right, left);
    }
    case SCAST_ASSIGN: {
      auto const left = genRefExpression();
      auto const right = builder.CreateSExt(genValueExpression(), llvm::IntegerType::get(tu->context, instruction.cast_assign_bitwidth()));
      return builder.CreateStore(right, left);
    }


    case DEREFERENCE:
      return builder.CreateLoad(ptr, genRefExpression());

    default:
      eden_unreachable("Invalid peep instruction type.");
    }
  }

  void genBlock(u32_t instruction_cut_off, sz_t block_idx) {
    llvm::Value* branch_value{};
    while (instruction_idx < instruction_cut_off)
      branch_value = genValueExpression();

    auto const& block = mir_blocks[block_idx];
    switch (block.terminator_type) {
      using enum PeepMIR::Block::Terminator;
    case BR:
      builder.CreateBr(
        llvm_blocks[block.br.next_block_idx]);
      break;
    case BRC:
      builder.CreateCondBr(
        builder.CreateTrunc(branch_value, i1),
        llvm_blocks[block.brc.true_block_idx],
        llvm_blocks[block.brc.false_block_idx]);
      break;
    case RET:
      builder.CreateRet(branch_value);
      break;
    default:
      eden_unreachable("Invalid block terminator type.");
    }
  }

  llvm::Function* compileFunction(PeepMIR::Function& func) {
    // reset state
    {
      instruction_idx = 0;
      locals.clear();
      llvm_blocks.clear();
      instructions = std::move(func.instructions);
      mir_blocks = std::move(func.blocks);
    }

    // initialize state for new function
    {
      auto const func_type = translateFunctionType(func.type);
      return_type = func_type->getReturnType();

      auto const num_params = func_type->getNumParams();
      auto const arg_types = func_type->param_begin();

      auto const linkage = func.is_public ? llvm::Function::ExternalLinkage : llvm::Function::InternalLinkage;
      llvmfunc = llvm::Function::Create(
        func_type, linkage, 0,
        func.nameof(),
        &tu->module);

      auto const entry = llvm::BasicBlock::Create(tu->context, "", llvmfunc);
      builder.SetInsertPoint(entry);
      llvm_blocks.emplace_back(entry);
      for (auto i{1uz}; i<mir_blocks.size(); ++i) {
        llvm_blocks.emplace_back(
          llvm::BasicBlock::Create(tu->context, "", llvmfunc));
      }

      auto arg = llvmfunc->arg_begin();
      locals.reserve(func.locals.size() + 1);
      locals.emplace_back(
        (return_type->isVoidTy()) ? nullptr
        : builder.CreateAlloca(return_type, nullptr) );

      for (auto i{0uz}; i<num_params; ++i) {
        llvm::AllocaInst* param_alloca = builder.CreateAlloca(arg_types[i], nullptr);
        builder.CreateStore(arg, param_alloca);
        locals.emplace_back(param_alloca);
        ++arg;
      }

      auto const num_locals = func.locals.size();
      for (auto i{num_params + 1}; i<num_locals; ++i) {
        locals.emplace_back(
          builder.CreateAlloca(translateType(func.locals[i]), nullptr)
          );
      }


    }

    // codegen
    {
      auto const num_blocks = mir_blocks.size();
      auto block_idx{0uz};
      while (block_idx < (num_blocks - 1)) {
        genBlock(mir_blocks[block_idx+1].first_instruction_idx, block_idx);
        ++block_idx;
        builder.SetInsertPoint(llvm_blocks[block_idx]);
      }
      builder.SetInsertPoint(&llvmfunc->back());

      return_type->isVoidTy()
      ? builder.CreateRetVoid()
      : builder.CreateRet( builder.CreateLoad(locals[0]->getAllocatedType(), locals[0]) );

      if (verifyFunction(*llvmfunc, &llvm::errs())) {
        tu->module.print(llvm::outs(), nullptr);
        std::quick_exit(1);
      }
    }

    return llvmfunc;
  }

public:

  void lowerToLLVM(PeepMIR::TU& tu) {
    // All of this is so stupid
    char buff[256];
    auto const module_name = tu.module->nameof();
    auto i{0uz};
    if (not module_name.empty()) {
      for (auto const c : module_name) {
        buff[i] = c;
        ++i;
      }
      buff[i++] = '.'; buff[i] = '\0';
    }

    std::vector<llvm::Function*> public_functions;
    public_functions.reserve(tu.functions.size());
    for (auto& func : tu.functions) {
      auto x = compileFunction(func);
      if (func.is_public)
        public_functions.push_back(x);
    }

    for (auto const func : public_functions) {
      auto j{0uz};
      auto func_name = func->getName();
      if (func_name == "main") continue;
      for (auto c : func_name) {
        buff[i + j] = c;
        ++j;
      }
      buff[i + j] = '\0';
      func->setName(buff);
    }
  }

  explicit Lowerer(TU* tu, TypeContext const* type_context)
  : tu(tu), builder(tu->context) {
    auto& context = tu->context;
    type_map.reserve(type_context->totalNumberOfTypes());
    i1 = llvm::Type::getInt1Ty(context);
    i8 = llvm::Type::getInt8Ty(context); type_map.emplace(PrimitiveType::i8(), i8); type_map.emplace(PrimitiveType::u8(), i8); type_map.emplace(PrimitiveType::char_(), i8); type_map.emplace(PrimitiveType::bool_(), i8);
    bool_true = llvm::ConstantInt::get(i8, 1); bool_false = llvm::ConstantInt::get(i8, 0);

    i16 = llvm::Type::getInt16Ty(context); type_map.emplace(PrimitiveType::i16(), i16); type_map.emplace(PrimitiveType::u16(), i16);
    i32 = llvm::Type::getInt32Ty(context); type_map.emplace(PrimitiveType::i32(), i32); type_map.emplace(PrimitiveType::u32(), i32);
    i64 = llvm::Type::getInt64Ty(context); type_map.emplace(PrimitiveType::i64(), i64); type_map.emplace(PrimitiveType::u64(), i64);
    f32 = llvm::Type::getFloatTy(context); type_map.emplace(PrimitiveType::f32(), f32);
    f64 = llvm::Type::getDoubleTy(context); type_map.emplace(PrimitiveType::f64(), f64);
    devoid = llvm::Type::getVoidTy(context); type_map.emplace(PrimitiveType::devoid(), devoid);
    ptr = llvm::PointerType::get(context, 0); type_map.emplace(PointerType::vague(false), ptr); type_map.emplace(PointerType::vague(true), ptr);

    for (auto pointer : type_context->getPointers()) type_map.emplace(pointer, ptr);
    assert(type_context->getVariants().empty());
    for (auto custom : type_context->getCustomTypes()) type_map.emplace(custom, translateType(custom));
    for (auto func : type_context->getFunctions()) type_map.emplace(func, translateFunctionType(func)); // func should be last since it may contain anything as a subtype
  }

};

}

std::unique_ptr<Backend> ToLLVM::codegen(PeepMIR::TU&& peeped_tu, std::filesystem::path const& file) {

#ifdef STAGE_BENCHMARKS
  auto begin_time = std::chrono::high_resolution_clock::now();
#endif

  auto backend = std::make_unique<TU>(file.native());
  Lowerer codegen(backend.get(), peeped_tu.module->getTypeContext());
  codegen.lowerToLLVM(peeped_tu);

#ifdef STAGE_BENCHMARKS
  auto end_time = std::chrono::high_resolution_clock::now();
  std::println("LLVM {}: {} | {} | {}",
    peeped_tu.module->nameof(),
    end_time - begin_time,
    std::chrono::duration_cast<std::chrono::microseconds>(end_time - begin_time),
    std::chrono::duration_cast<std::chrono::milliseconds>(end_time - begin_time)
  );
#endif
  return backend;
}