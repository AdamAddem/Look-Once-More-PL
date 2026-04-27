#include "tollvm.hpp"
#include "../../parsing/ast.hpp"
#include "build_system/build.hpp"
#include "error.hpp"
#include "peep_mir/peep_mir.hpp"
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

using namespace LOM;

namespace {

using PeepMIR::released_string;
using PeepMIR::released_span;
using PeepMIR::released_ptr;


//shes big but shes a beaut
class TU final : public Backend {
  llvm::LLVMContext context;
  llvm::Module module;

  llvm::IntegerType* i1; llvm::ConstantInt* i1_0; llvm::ConstantInt* i1_1;
  llvm::IntegerType* i8; llvm::IntegerType* i16; llvm::IntegerType* i32; llvm::IntegerType* i64;
  llvm::Type* f32; llvm::Type* f64; llvm::Type* devoid; llvm::PointerType* ptr;

  std::unordered_map<std::string_view, llvm::Value*> imports;

  [[nodiscard]] std::filesystem::path
  createASMFile(const std::filesystem::path &file) override {
    static const std::filesystem::path asm_folder = "build/asm/";
    std::filesystem::path asm_path = asm_folder;
    asm_path.append(file.string());
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
  createIRFile(const std::filesystem::path &file) override {
    static const std::filesystem::path ir_path = "build/llvm_ir/";
    std::filesystem::path file_path = ir_path;
    file_path.append(file.string());
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
  createObjectFile(const std::filesystem::path &file) override {
    static const std::filesystem::path object_folder = "build/obj/";
    std::filesystem::path obj_path = object_folder;
    obj_path.append(file.string());
    std::filesystem::create_directories(obj_path.parent_path());
#ifdef _WIN32
    obj_path.replace_extension(".obj");
#else
    obj_path.replace_extension(".o");
#endif

    createFile(obj_path, llvm::CodeGenFileType::ObjectFile);
    return obj_path;
  }

  void createFile(const std::filesystem::path& obj_path, const llvm::CodeGenFileType filetype) {
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    static const llvm::Triple target_triple{llvm::sys::getDefaultTargetTriple()};
    module.setTargetTriple(target_triple);
    std::string err;
    const auto target = llvm::TargetRegistry::lookupTarget(module.getTargetTriple(), err);
    if (not target) {
      llvm::errs() << err;
      std::unreachable();
    }

    static constexpr auto CPU = "generic";
    static constexpr auto Features = "";
    static const llvm::TargetOptions opt;
    const auto target_machine = target->createTargetMachine(
      target_triple, CPU, Features, opt, llvm::Reloc::PIC_);
    module.setDataLayout(target_machine->createDataLayout());

    std::error_code EC;
    llvm::raw_fd_ostream dest(obj_path.string(), EC, llvm::sys::fs::OF_None);
    if (EC) {
      llvm::errs() << "Could not open file: " << obj_path.string() << " | " << EC.message() << '\n';
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

  [[nodiscard]] llvm::Value*
  getFunctionImport(const char* name) {
    std::string_view name_view(name);
    if (imports.contains(name_view))
      return imports[name_view];

    auto i{0uz};
    while (name[i] not_eq '.') {
      ++i;
    }
    const std::string_view module_name(name, i);

    const char* func_name = name+i+1;
    auto m = getModule(module_name);
    auto func = m->getPublicFunction(func_name); assert(func);
    auto function_type = translateFunctionType(func.value());
    if (module_name == "__C") {
      name = func_name;
    }
    return imports[name_view] = module.getOrInsertFunction(name, function_type).getCallee();
  }

  [[nodiscard]] llvm::FunctionType*
  translateFunctionType(const FunctionType* t) {
    llvm::Type* arg_types[Settings::MAX_FUNCTION_PARAMETERS];
    auto num_params{0uz};
    for (const auto param_type : t->parameterTypes()) {
      arg_types[num_params] = translateType(param_type);
      ++num_params;
    }

    //hack
    auto return_type =
    t->returnType()->isBool()
    ? i1
    : translateType(t->returnType());

    return llvm::FunctionType::get(return_type, {arg_types, num_params}, t->isVariadic());
  }

  [[nodiscard]] llvm::Type*
  translateType(const Type* t) {
    assert(not t->isCustom());
    assert(not t->isVariant());
    assert(t not_eq PrimitiveType::string());

    if (t->isIntegral()) {
      if (t == PrimitiveType::i32() or t == PrimitiveType::u32())
        return i32;
      if (t == PrimitiveType::i64() or t == PrimitiveType::u64())
        return i64;
      if (t == PrimitiveType::i8() or t == PrimitiveType::u8())
        return i8;
      if (t == PrimitiveType::i16() or t == PrimitiveType::u16())
        return i16;
      std::unreachable();
    }

    if (t->isFloating()) {
      if (t == PrimitiveType::f32())
        return f32;
      if (t == PrimitiveType::f64())
        return f64;
      std::unreachable();
    }

    if (t == Type::devoid())
      return devoid;

    if (t == PrimitiveType::bool_() or t == PrimitiveType::char_())
      return i8;

    if (t->isPointer())
      return ptr;

    std::unreachable();
  }

  [[nodiscard]] llvm::IntegerType*
  typeForInteger(i64_t int_literal) noexcept {
    return
    llvm::cast<llvm::IntegerType>(translateType(signedToLiteralInstance(int_literal).type));
  }

  [[nodiscard]] llvm::IntegerType*
  typeForUnsigned(u64_t uint_literal) noexcept {
    return
    llvm::cast<llvm::IntegerType>(translateType(unsignedToLiteralInstance(uint_literal).type));
  }

  friend struct Function;
  struct Function {
    TU* tu;

    llvm::Type* return_type;
    llvm::Function* llvmfunc;
    llvm::IRBuilder<> builder;
    std::vector<llvm::AllocaInst*> locals;

    released_ptr<PeepMIR::Instruction> instructions; sz_t instruction_idx{};
    released_span<PeepMIR::Block> mir_blocks; sz_t block_idx{};
    std::vector<llvm::BasicBlock*> llvm_blocks;

    Function(PeepMIR::Function& func, TU* tu)
    : tu(tu), builder(tu->context) {
      llvm_blocks.reserve(func.blocks.size());

      llvm::Type* arg_types[Settings::MAX_FUNCTION_PARAMETERS];
      const auto function_type = func.type;
      auto num_params{0uz};
      for (const auto t : function_type->parameterTypes()) {
        arg_types[num_params] = tu->translateType(t);
        ++num_params;
      }

      //hack
      return_type =
      function_type->returnType()->isBool()
      ? tu->i1
      : tu->translateType(function_type->returnType());

      const auto func_type = llvm::FunctionType::get(return_type, {arg_types, num_params}, false);
      const auto linkage = func.is_public ? llvm::Function::ExternalLinkage : llvm::Function::InternalLinkage;
      llvmfunc = llvm::Function::Create(func_type, linkage, 0, std::string_view(func.name), &tu->module);

      const auto entry = llvm::BasicBlock::Create(tu->context, "", llvmfunc);
      builder.SetInsertPoint(entry);
      llvm_blocks.emplace_back(entry);
      for (auto i{1uz}; i<func.blocks.size(); ++i) {
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

      const auto num_locals = func.locals.size();
      for (auto i{num_params + 1}; i<num_locals; ++i) {
          locals.emplace_back(
            builder.CreateAlloca(tu->translateType(func.locals[i]), nullptr)
            );
      }

      func.locals.destroy_and_deallocate();
      instructions = std::move(func.instructions);
      mir_blocks = std::move(func.blocks);
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
        const auto var = llvm::cast<llvm::AllocaInst>(genExpression());
        const auto var_type = var->getAllocatedType();
        res = builder.CreateAdd(
          builder.CreateLoad(var_type, var),
          unsignedConstant(var_type, 1));
        builder.CreateStore(res, var);
        return var;
      }
      case FPRE_INC: {
        const auto var = llvm::cast<llvm::AllocaInst>(genExpression());
        const auto var_type = var->getAllocatedType();
        res = builder.CreateFAdd(
          builder.CreateLoad(var_type, var),
          fpConstant(var_type, 1));
        builder.CreateStore(res, var);
        return var;
      }
      case PRE_DEC: {
        const auto var = llvm::cast<llvm::AllocaInst>(genExpression());
        const auto var_type = var->getAllocatedType();
        res = builder.CreateSub(
          builder.CreateLoad(var_type, var),
          unsignedConstant(var_type, 1));
        builder.CreateStore(res, var);
        return var;
      }
      case FPRE_DEC: {
        const auto var = llvm::cast<llvm::AllocaInst>(genExpression());
        const auto var_type = var->getAllocatedType();
        res = builder.CreateFSub(
          builder.CreateLoad(var_type, var),
          fpConstant(var_type, 1));
        builder.CreateStore(res, var);
        return var;
      }
      case ADDRESS_OF:
        return genExpression();
      case NEGATE:
        return builder.CreateNeg(genReadExpression());
      case FNEGATE:
        return builder.CreateFNeg(genReadExpression());
      case BITNOT:
        return builder.CreateNot(genReadExpression());
      case POST_INC: {
        const auto var = llvm::cast<llvm::AllocaInst>(genExpression());
        const auto var_type = var->getAllocatedType();
        auto load = builder.CreateLoad(var_type, var);
        res = builder.CreateAdd(load, unsignedConstant(var_type, 1));
        builder.CreateStore(res, var);
        return load;
      }
      case FPOST_INC: {
        const auto var = llvm::cast<llvm::AllocaInst>(genExpression());
        const auto var_type = var->getAllocatedType();
        auto load = builder.CreateLoad(var_type, var);
        res = builder.CreateFAdd(load, fpConstant(var_type, 1));
        builder.CreateStore(res, var);
        return load;
      }
      case POST_DEC: {
        const auto var = llvm::cast<llvm::AllocaInst>(genExpression());
        const auto var_type = var->getAllocatedType();
        auto load = builder.CreateLoad(var_type, var);
        res = builder.CreateSub(load, unsignedConstant(var_type, 1));
        builder.CreateStore(res, var);
        return load;
      }
      case FPOST_DEC: {
        const auto var = llvm::cast<llvm::AllocaInst>(genExpression());
        const auto var_type = var->getAllocatedType();
        auto load = builder.CreateLoad(var_type, var);
        res = builder.CreateFSub(load, fpConstant(var_type, 1));
        builder.CreateStore(res, var);
        return load;
      }
      default:
        std::unreachable();
      }
    }

    [[nodiscard]] llvm::Value*
    genBinary(PeepMIR::Instruction::Type type) noexcept {
      if (type == PeepMIR::Instruction::ASSIGN) {
        const auto left = genExpression();
        const auto right = genExpression();
        return builder.CreateStore(right, left);
      }

      const auto left = genReadExpression();
      const auto right = genExpression();

      switch (type) {
        using enum PeepMIR::Instruction::Type;
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

      case ASSIGN: return builder.CreateStore(right, left);

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

      case AND: return builder.CreateLogicalAnd(builder.CreateTrunc(left, tu->i1), builder.CreateTrunc(right, tu->i1));
      case OR: return builder.CreateLogicalOr(builder.CreateTrunc(left, tu->i1), builder.CreateTrunc(right, tu->i1));
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
        std::unreachable();
      }
    }

    [[nodiscard]] llvm::Value*
    genReadExpression() noexcept { //this whole function is so stupid
      const auto instruction = instructions[instruction_idx];
      const auto res = genExpression();
      if (res == nullptr)
        return nullptr;
      if (llvm::isa<llvm::AllocaInst>(res))
        return builder.CreateLoad(llvm::cast<llvm::AllocaInst>(res)->getAllocatedType(), res);
      if (instruction.type == PeepMIR::Instruction::DEREFERENCE)
        return builder.CreateLoad(tu->translateType(instruction.dereference_type()), res);
      if (instruction.type == PeepMIR::Instruction::GLOBAL)
        return builder.CreateLoad(llvm::cast<llvm::GlobalVariable>(res)->getValueType(), res);
      if (instruction.type == PeepMIR::Instruction::LOCAL)
        return builder.CreateLoad(llvm::cast<llvm::AllocaInst>(res)->getAllocatedType(), res);

      return res;
    }

    [[nodiscard]] llvm::Value*
    genExpression() noexcept {
      auto const& instruction = instructions[instruction_idx++];
      switch (instruction.type) {
      using enum PeepMIR::Instruction::Type;
      case NOOP:
        return nullptr;
      case GLOBAL: {
        const auto global = (tu->module.getNamedGlobal(instruction.global_name()));
        assert(global);
        return global;
      }
      case FUNCTION: {
        const auto function = (tu->module.getFunction(instruction.function_name())); assert(function);
        return function;
      }
      case IMPORTED_GLOBAL:
        assert(false);
      case IMPORTED_FUNCTION: {
        return tu->getFunctionImport(instruction.imported_function_name());
      }

      case LOCAL: {
        if(instruction.local_idx() >= locals.size()) {
          llvmfunc->print(llvm::outs());
          //std::cout << llvmfunc->getName().str() << std::endl;
          std::quick_exit(1);
        }
        const auto local = (locals[instruction.local_idx()]);
        return local;
      }
      case INT_LITERAL:
        return llvm::ConstantInt::get(tu->i64, instruction.value, true);
        //return signedConstant(tu->typeForInteger(instruction.int_value()), instruction.int_value());
      case UINT_LITERAL:
        return llvm::ConstantInt::get(tu->i64, instruction.value);
        //return unsignedConstant(tu->typeForUnsigned(instruction.uint_value()), instruction.uint_value());
      case FLOAT_LITERAL:
        return llvm::ConstantFP::get(tu->f32, instruction.float_value());
      case DOUBLE_LITERAL:
        return llvm::ConstantFP::get(tu->f64, instruction.double_value());
      case BOOL_LITERAL:
        return instruction.bool_value() ? tu->i1_1 : tu->i1_0;
      case CHAR_LITERAL:
        return llvm::ConstantInt::get(tu->i8, instruction.char_value());
      case STRING_LITERAL: {
        const auto str_lit = builder.CreateGlobalString(instruction.string_value());
          //llvm::ConstantDataArray::getString(tu->context, instruction.string_value());
        return str_lit;
      }

      case ADD: case FADD:
      case SUB: case FSUB:
      case MULT: case FMULT:
      case UDIV: case SDIV: case FDIV:
      case UMOD: case SMOD: case FMOD:
      case ASSIGN:
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

      case PRE_INC: case FPRE_INC:
      case PRE_DEC: case FPRE_DEC:
      case ADDRESS_OF:
      case NEGATE: case FNEGATE:
      case BITNOT:
      case POST_INC: case FPOST_INC:
      case POST_DEC: case FPOST_DEC:
        return genUnary(instruction.type);
      case DEREFERENCE:
        return builder.CreateLoad(tu->ptr, genExpression());

      case UCAST:
        return builder.CreateZExtOrTrunc(genReadExpression(), llvm::IntegerType::get(tu->context, instruction.value));
      case SCAST:
        return builder.CreateSExtOrTrunc(genReadExpression(), llvm::IntegerType::get(tu->context, instruction.value));
      case FCAST:
        assert(instruction.value == 32 or instruction.value == 64);
        return builder.CreateFPCast(genReadExpression(),
          instruction.value == 32
          ? llvm::Type::getFloatTy(tu->context)
          : llvm::Type::getDoubleTy(tu->context));
      case PCAST:
      case NCAST:
        return genReadExpression();
      case CALL: {
        const auto fn = genExpression();
        llvm::Value* parameters[Settings::MAX_FUNCTION_PARAMETERS];
        for (auto i{0uz}; i<instruction.num_params(); ++i)
          parameters[i] = genExpression();

        return builder.CreateCall(
          llvm::cast<llvm::Function>(fn),
          {parameters, instruction.num_params()}
          );
      }
      default:
        std::unreachable();
      }
    }

    void genBlock(u32_t instruction_cut_off) {
      llvm::Value* branch_value{};
      while (instruction_idx < instruction_cut_off)
        branch_value = genExpression();

      const auto& block = mir_blocks[block_idx];
      switch (block.terminator_type) {
        using enum PeepMIR::Block::Terminator;
      case BR:
        builder.CreateBr(
          llvm_blocks[block.br.next_block_idx]);
        break;
      case BRC:
        builder.CreateCondBr( branch_value,
          llvm_blocks[block.brc.true_block_idx],
          llvm_blocks[block.brc.false_block_idx]);
        break;
      case RET:
        builder.CreateRet(branch_value);
        break;
      default:
        std::unreachable();
      }
    }

    void codegenFunction() {
      const auto num_blocks = mir_blocks.size();
      while (block_idx < (num_blocks - 1)) {
        genBlock(mir_blocks[block_idx+1].first_instruction_idx);
        ++block_idx;
        builder.SetInsertPoint(llvm_blocks[block_idx]);
      }
      builder.SetInsertPoint(&llvmfunc->back());
      if (return_type->isVoidTy()) {
        builder.CreateRetVoid();
      }
      else {
        builder.CreateRet(
            builder.CreateLoad(locals[0]->getAllocatedType(), locals[0])
          );
      }

      mir_blocks.destroy_and_deallocate();
      instructions.destroy_and_deallocate();
      if (verifyFunction(*llvmfunc, &llvm::errs())) {
        tu->module.print(llvm::outs(), nullptr);
        throw BackendError("Failed to verify Function!", llvmfunc->getName().str(), 0);
      }
    }
  };

  llvm::Function* compileFunction(PeepMIR::Function& func) {
    Function lowering_function(func, this);
    lowering_function.codegenFunction();
    return lowering_function.llvmfunc;
  }

public:

  void lowerToLLVM(PeepMIR::TU& tu) {
    char buff[256];
    const auto module_name = tu.table->getName();
    auto i{0uz};
    if (not module_name.empty()) [[likely]] {
      for (const auto c : module_name) {
        buff[i] = c;
        ++i;
      }
      buff[i++] = '.'; buff[i] = '\0';
    }

    /* auto i{0uz};
    for (auto& instruction : tu.global_instructions) {
      if (instruction.type not_eq PeepMIR::Instruction::GLOBAL)
        continue;

      module.insertGlobalVariable(
        new llvm::GlobalVariable(
          translateType(tu.globals[i]),
          false,
          llvm::GlobalValue::LinkageTypes::ExternalLinkage,
          nullptr,
          instruction.global_name()
          )
        );
      ++i;
      instruction.release_string_value().destroy_and_deallocate();
    }*/

    std::vector<llvm::Function*> public_functions;
    public_functions.reserve(tu.functions.size());
    for (auto& func : tu.functions) {
      auto x = compileFunction(func);
      if (func.is_public)
        public_functions.push_back(x);
    }

    for (auto func : public_functions) {
      auto j{0uz};
      for (auto c : func->getName()) {
        buff[i + j] = c;
        ++j;
      }
      buff[i + j] = '\0';
      func->setName(buff);
    }
  }

  explicit TU(const std::string& filename)
  : module(filename, context) {
    i1 = llvm::Type::getInt1Ty(context); i1_0 = llvm::ConstantInt::get(i1, 0); i1_1 = llvm::ConstantInt::get(i1, 1);
    i8 = llvm::Type::getInt8Ty(context); i16 = llvm::Type::getInt16Ty(context);
    i32 = llvm::Type::getInt32Ty(context); i64 = llvm::Type::getInt64Ty(context);
    f32 = llvm::Type::getFloatTy(context); f64 = llvm::Type::getDoubleTy(context);
    devoid = llvm::Type::getVoidTy(context); ptr = llvm::PointerType::get(context, 0);
  }

  void printModule(llvm::raw_ostream& out = llvm::outs()) const
  { module.print(out, nullptr); }

};
}

std::unique_ptr<Backend> ToLLVM::codegen(PeepMIR::TU&& peeped_tu, const std::filesystem::path &file) {
  auto ptr = std::make_unique<TU>(file.string());
  ptr->lowerToLLVM(peeped_tu);
  return ptr;
}