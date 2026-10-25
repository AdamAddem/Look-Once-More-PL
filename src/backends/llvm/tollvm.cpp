#include "tollvm.hpp"
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

#include "build_system/build.hpp"
#include "error.hpp"
#include "parsing/ast.hpp"
#include "peepir/peepir.hpp"
#include "settings.hpp"

#include "edenlib/vectors/swap_vector.hpp"

#include <cassert>
#include <chrono>
#include <filesystem>
#include <print>


#include "edenlib/shortened_macros.hpp"
using namespace LOM;

namespace {

struct TargetStruct {
  static constexpr std::string_view CPU = "generic";
  static constexpr std::string_view Features = "";

  std::string err;
  llvm::Triple triple;
  llvm::TargetMachine* machine;
  llvm::DataLayout data_layout;

  TargetStruct() {
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    triple = llvm::Triple(llvm::sys::getDefaultTargetTriple());
    auto const target = llvm::TargetRegistry::lookupTarget(triple, err);
    if (not target) {
      llvm::errs() << err;
      std::quick_exit(1);
    }

    machine = target->createTargetMachine(triple, CPU, Features, {}, llvm::Reloc::PIC_);
    data_layout = machine->createDataLayout();
    machine->Options.MCOptions.AsmVerbose = true;
  }

}Target;

struct LLVM_Backend final : Backend {
  llvm::LLVMContext context;
  llvm::Module module;

  explicit LLVM_Backend(std::string_view module_name) : context(), module(module_name, context) {
    module.setTargetTriple(Target.triple);
    module.setDataLayout(Target.data_layout);
  }

  void printModule(llvm::raw_ostream& out = llvm::outs()) const
  { module.print(out, nullptr); }

  void createASMFile() override {
    std::filesystem::path asm_path = asm_folder;
    asm_path.append( std::string_view(module.getName()) );
    std::filesystem::create_directories(asm_path.parent_path());
    asm_path.replace_extension(asm_extension);

    createFile(asm_path, llvm::CodeGenFileType::AssemblyFile);
  }

  void createIRFile() override {
    std::filesystem::path ir_path = ToLLVM::ir_path;
    ir_path.append( std::string_view(module.getName()) );
    std::filesystem::create_directories(ir_path.parent_path());
    ir_path.replace_extension(".ll");

    std::error_code EC;
    llvm::raw_fd_ostream dest(ir_path.string(), EC, llvm::sys::fs::OF_None);
    if (EC) {
      llvm::errs() << "Could not open file: " << ir_path.string() << " | " << EC.message() << '\n';
      std::quick_exit(1);
    }

    printModule(dest);
  }

  void createObjectFile() override {
    std::filesystem::path obj_path = object_folder;
    obj_path.append(std::string_view(module.getName()));
    std::filesystem::create_directories(obj_path.parent_path());
    obj_path.replace_extension(obj_extension);

    createFile(obj_path, llvm::CodeGenFileType::ObjectFile);
  }

  void createFile(std::filesystem::path const& output_path, llvm::CodeGenFileType filetype) {
    std::error_code EC;
    llvm::raw_fd_ostream dest(output_path.string(), EC, llvm::sys::fs::OF_None);
    if (EC) {
      llvm::errs() << "Could not open file: " << output_path.string() << " | " << EC.message() << '\n';
      std::exit(1);
    }

    llvm::legacy::PassManager pass;
    if (Target.machine->addPassesToEmitFile(pass, dest, nullptr, filetype)) {
      llvm::errs() << "Target machine can't emit a file of this type";
      std::exit(1);
    }

    pass.run(module);
    dest.flush();
  }
};

class LoweringContext final {
  LLVM_Backend& backend;
  PeepIR::TU& tu;

  llvm::IntegerType* i1; llvm::IntegerType* i8;
  llvm::IntegerType* i16; llvm::IntegerType* i32; llvm::IntegerType* i64;
  llvm::Type* f32; llvm::Type* f64; llvm::Type* devoid; llvm::PointerType* ptr;
  llvm::Value* i64_0;
  llvm::Value* bool_true; llvm::Value* bool_false;

  // aligned and removed padding to make operator== faster
  struct alignas(8) TypeMapKey { u16_t module_id; u16_t _pad{}; u32_t type_id; bool operator==(TypeMapKey const&) const noexcept = default; };
  eden::swap_map<TypeMapKey, llvm::Type*> custom_type_map;

  /* Variables used when lowering a function */
  llvm::Type* return_type{};
  llvm::IRBuilder<> builder;
  eden::vector<llvm::AllocaInst*> locals;

  eden::vector16<PeepIR::Instruction> instructions; sz_t instruction_idx{};
  eden::vector16<PeepIR::Block> peepir_blocks;
  eden::vector<llvm::BasicBlock*> llvm_blocks;
  /* Variables used when lowering a function */

  using TemporaryStringCat = llvm::SmallString<128>; // why do so many things just not take a twine

  [[nodiscard]] llvm::Value*
  getFunctionImport(PeepIR::Instruction module_fn) {
    auto const module_id = module_fn.module_member_data.module_id;
    auto const member_id = module_fn.module_member_data.member_id;

    auto const& imported_tu = getPeepTU(module_id);
    auto const& imported_fn_symbol = imported_tu.module.getFunction(member_id);
    auto const& imported_module_name = imported_tu.name;
    auto const  imported_fn_name = imported_fn_symbol.nameof();

    TemporaryStringCat full_function_name{imported_module_name, "::"};
    if (imported_module_name == "__C") full_function_name.clear();
    full_function_name += imported_fn_name;

    return backend.module.getOrInsertFunction(full_function_name, translateFunctionType(imported_fn_symbol)).getCallee();
  }

  [[nodiscard]] llvm::FunctionType*
  translateFunctionType(Symbol::Function const& fn) noexcept {
    auto const num_params = fn.num_parameters;

    llvm::SmallVector<llvm::Type*, 8> arg_types;
    for (sz_t i{}; i<num_params; ++i)
      arg_types.push_back(translateType( fn.getLocal(i).typeID ));

    // hack
    auto const return_typeID = fn.return_typeID;
    auto const llvm_return_type = return_typeID.isBool() ? i1 : translateType(return_typeID);

    auto const res = llvm::FunctionType::get(llvm_return_type, arg_types, fn.is_variadic);
    return res;
  }

  [[nodiscard]] llvm::Type*
  translateType(TypeID typeID) noexcept {

    // translate non-custom types
    {
      if (typeID.isPointer()) return ptr;

      switch (typeID.derived) { using enum Type::DerivedType;
      case DEVOID: return devoid;
      case ARRAY: {
        auto const& array_type = typeID.getArrayType();
        return llvm::ArrayType::get( translateType(array_type.getSubtypeID()), array_type.getSize() );
      }

      case PRIMITIVE: {
        auto const& primitive = typeID.getPrimitiveType();
        switch (primitive.getUnderlyingPrimitiveType()) { using enum PrimitiveType::PrimitiveTypeEnum;
        case STRING:        Unreachable("WIP");
        case F32: return f32;
        case F64: return f64;

        case U8: case I8: case CHAR: case BOOL:
          return i8;

        case U16: case I16: return i16;
        case U32: case I32: return i32;
        case U64: case I64: return i64;
        default:
          Unreachable("Invalid primitive type.");
        }
      }

      case CUSTOM: break;
      default: Unreachable("Invalid type being translated.");
      }
    }

    auto const key = TypeMapKey {
      .module_id = typeID.module_id,
      .type_id = typeID.id
    };

    // search for existing custom translation
    {
      auto const element = custom_type_map[key];
      if (element) return element->value;
    }

    // translate custom types
    auto const& custom_type = typeID.getCustomType();
    auto const& member_table = custom_type.member_table();
    auto const num_members = member_table.num_variables();

    llvm::SmallVector<llvm::Type*, 64> member_types;
    for (sz_t i{}; i<num_members; ++i)
      member_types.push_back(
        translateType(member_table.getVariable(i).typeID)
      );

    auto const res = llvm::StructType::create(backend.context, llvm::ArrayRef(member_types), custom_type.nameof());
    custom_type_map.emplace_back(key, res);
    return res;
  }

  [[nodiscard]] llvm::Constant*
  fpConstant(llvm::Type* t, double value) const noexcept
  { return llvm::ConstantFP::get(t, value); }

  [[maybe_unused]] [[nodiscard]] llvm::Constant*
  signedConstant(llvm::Type* t, i64_t value) const noexcept
  { return llvm::ConstantInt::get(llvm::cast<llvm::IntegerType>(t), value, true); }

  [[nodiscard]] llvm::Constant*
  unsignedConstant(llvm::Type* t, u64_t value) const noexcept
  { return llvm::ConstantInt::get(llvm::cast<llvm::IntegerType>(t), value); }

  [[nodiscard]] llvm::Value*
  genUnary(PeepIR::Instruction::Enum type) noexcept {
    llvm::Value* res{};
    switch (type) {
    using enum PeepIR::Instruction::Enum;
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
      Unreachable("Invalid unary peep instruction type.");
    }
  }

  [[nodiscard]] llvm::Value*
  genBinary(PeepIR::Instruction::Enum type) noexcept {
    auto const left = genValueExpression();
    auto const right = genValueExpression();

    switch (type) { using enum PeepIR::Instruction::Enum;
    case ADD:   return builder.CreateAdd(left, right);
    case FADD:  return builder.CreateFAdd(left, right);

    case SUB:   return builder.CreateSub(left, right);
    case FSUB:  return builder.CreateFSub(left, right);
    case MULT:  return builder.CreateMul(left, right);
    case FMULT: return builder.CreateFMul(left, right);

    case UDIV: return builder.CreateUDiv(left, right);
    case SDIV: return builder.CreateSDiv(left, right);
    case FDIV: return builder.CreateFDiv(left, right);
    case UMOD: return builder.CreateURem(left, right);
    case SMOD: return builder.CreateSRem(left, right);
    case FMOD: return builder.CreateFRem(left, right);

    case ASSIGN:
      Unreachable("Assign shouldn't be called here.");

    case ULESS: return builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_ULT, left, right);
    case SLESS: return builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_SLT, left, right);
    case FLESS: return builder.CreateCmp(llvm::CmpInst::Predicate::FCMP_OLT, left, right);

    case UGTR:  return builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_UGT, left, right);
    case SGTR:  return builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_SGT, left, right);
    case FGTR:  return builder.CreateCmp(llvm::CmpInst::Predicate::FCMP_OGT, left, right);

    case ULEQ:  return builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_ULE, left, right);
    case SLEQ:  return builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_SLE, left, right);
    case FLEQ:  return builder.CreateCmp(llvm::CmpInst::Predicate::FCMP_OLE, left, right);

    case UGEQ:  return builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_UGE, left, right);
    case SGEQ:  return builder.CreateCmp(llvm::CmpInst::Predicate::ICMP_SGE, left, right);
    case FGEQ:  return builder.CreateCmp(llvm::CmpInst::Predicate::FCMP_OGE, left, right);

    case AND:    return builder.CreateLogicalAnd(builder.CreateTrunc(left, i1), builder.CreateTrunc(right, i1));
    case OR:     return builder.CreateLogicalOr(builder.CreateTrunc(left, i1), builder.CreateTrunc(right, i1));

    case BITAND: return builder.CreateAnd(left, right);
    case BITOR:  return builder.CreateOr(left, right);
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
      Unreachable("Invalid binary peep instruction type.");
    }
  }

  [[nodiscard]] llvm::Value*
  genAssign(PeepIR::Instruction::Enum type, u64_t bitwidth = 0) noexcept {
    auto const left = genRefExpression();
    llvm::Value* right;
    switch (type) {
      using enum PeepIR::Instruction::Enum;
    case ASSIGN:
      right = genValueExpression();
      break;
    case UCAST_ASSIGN:
      right = builder.CreateZExt(genValueExpression(), llvm::IntegerType::get(backend.context, bitwidth));
      break;
    case SCAST_ASSIGN:
      right = builder.CreateSExt(genValueExpression(), llvm::IntegerType::get(backend.context, bitwidth));
      break;

    default:
      Unreachable("Invalid assign peep instruction type.");
    }

    return builder.CreateStore(right, left);
  }

  [[nodiscard]] llvm::Value*
  genValueExpression() noexcept { // only relevant for that which can be used by value, with the addition of functions
    auto const& instruction = instructions[instruction_idx++];
    switch (instruction.type) {
      using enum PeepIR::Instruction::Enum;
    case NOOP: return nullptr;

    case FUNCTION: {
      auto const function = backend.module.getFunction(instruction.original_string(tu.source_files[instruction.file_idx])); assert(function);
      return function;
    }
    case LOCAL: {
      assert(instruction.local_data.idx < locals.size());
      auto const local = locals[instruction.local_data.idx];
      auto const local_type = local->getAllocatedType();
      return builder.CreateLoad(local_type, local);
    }

    case TYPE_VARIABLE: {
      auto const custom_typeID = instruction.custom_typeID();
      auto const& custom_type = custom_typeID.getCustomType();

      auto const id = instruction.type_member_data.member_id;
      auto const member = custom_type.member_table().getVariable(id);
      auto const member_ptr = builder.CreateStructGEP(translateType(custom_typeID), genRefExpression(), id);
      return builder.CreateLoad(translateType(member.typeID), member_ptr);
    }
    case MODULE_FUNCTION:
      return getFunctionImport(instruction);

    case I8_LITERAL:              return llvm::ConstantInt::get(i8, instruction.signed_literal_data.value, true);
    case I16_LITERAL:             return llvm::ConstantInt::get(i16, instruction.signed_literal_data.value, true);
    case I32_LITERAL:             return llvm::ConstantInt::get(i32, instruction.signed_literal_data.value, true);
    case I64_LITERAL:             return llvm::ConstantInt::get(i64, instruction.signed_literal_data.value, true);
    case U8_LITERAL:              return llvm::ConstantInt::get(i8, instruction.unsigned_literal_data.value);
    case U16_LITERAL:             return llvm::ConstantInt::get(i16, instruction.unsigned_literal_data.value);
    case U32_LITERAL:             return llvm::ConstantInt::get(i32, instruction.unsigned_literal_data.value);
    case U64_LITERAL:             return llvm::ConstantInt::get(i64, instruction.unsigned_literal_data.value);
    case FLOAT_LITERAL:           return llvm::ConstantFP::get(f32, instruction.float_literal_data.value);
    case DOUBLE_LITERAL:          return llvm::ConstantFP::get(f64, instruction.double_literal_data.value);
    case BOOL_LITERAL:            return instruction.bool_literal_data.value ? bool_true : bool_false;
    case CHAR_LITERAL:            return llvm::ConstantInt::get(i8, instruction.char_literal_data.value);
    case STRING_LITERAL:          return builder.CreateGlobalString(instruction.original_string(tu.source_files[instruction.file_idx]));
    case ESCAPED_STRING_LITERAL:  return builder.CreateGlobalString(instruction.escaped_string_value(tu.source_files[instruction.file_idx]));

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
    case SUBSCRIPT: {
      auto const array = genRefExpression();
      auto const array_typeID = instruction.subscript_data.array_typeID;
      auto const& array_type = array_typeID.getArrayType();

      auto const index = genValueExpression();
      auto const element_ptr = builder.CreateGEP( translateType(array_typeID), array, {i64_0, index} );

      return builder.CreateLoad( translateType(array_type.getSubtypeID()), element_ptr );
    }

    case ASSIGN:       return genAssign(instruction.type);
    case UCAST_ASSIGN:
    case SCAST_ASSIGN: return genAssign(instruction.type, instruction.cast_assign_data.bitwidth);

    case PRE_INC:
    case FPRE_INC:
    case PRE_DEC:
    case FPRE_DEC: {
      auto const var = llvm::cast<llvm::AllocaInst>(genUnary(instruction.type));
      return builder.CreateLoad(var->getAllocatedType(), var);
    }

    case NEGATE: case FNEGATE:
    case POST_INC: case FPOST_INC:
    case POST_DEC: case FPOST_DEC:
    case ADDRESS_OF:
    case BITNOT:    return genUnary(instruction.type);

    case DEREFERENCE:
      return builder.CreateLoad(
        translateType(instruction.dereference_data.dereference_typeID),
        genValueExpression());

    case UCAST: {
      auto const dest_typeID = instruction.cast_data.destination_typeID;
      auto const llvm_dest_type = translateType(dest_typeID);
      if (dest_typeID.isFloating())
        return builder.CreateUIToFP(genValueExpression(), llvm_dest_type);
      return builder.CreateZExtOrTrunc(genValueExpression(), llvm_dest_type);
    }
    case SCAST: {
      auto const dest_typeID = instruction.cast_data.destination_typeID;
      auto const llvm_dest_type = translateType(dest_typeID);
      if (dest_typeID.isFloating())
        return builder.CreateSIToFP(genValueExpression(), llvm_dest_type);
      return builder.CreateSExtOrTrunc(genValueExpression(), llvm_dest_type);
    }
    case FCAST: {
      auto const dest_typeID = instruction.cast_data.destination_typeID;
      auto const llvm_dest_type = translateType(dest_typeID);
      if (dest_typeID.isIntegral()) {
        if (dest_typeID.isSignedIntegral())
          return builder.CreateFPToSI(genValueExpression(), llvm_dest_type);
        return builder.CreateFPToUI(genValueExpression(), llvm_dest_type);
      }
      return builder.CreateFPCast(genValueExpression(), llvm_dest_type);
    }
    case PCAST: return genValueExpression();

    case CALL: {
      auto const fn = genRefExpression();

      llvm::SmallVector<llvm::Value*, 8> parameters;
      for (sz_t i{}; i<instruction.call_data.num_parameters; ++i)
        parameters.push_back(genValueExpression());

      return builder.CreateCall(
        llvm::cast<llvm::Function>(fn),
        parameters
        );
    }

    default: Unreachable("Invalid peep instruction type.");
    }
  }

  [[nodiscard]] llvm::Value*
  genRefExpression() noexcept { // only relevant for that which can be referenced
    auto const& instruction = instructions[instruction_idx++];
    switch (instruction.type) {
    using enum PeepIR::Instruction::Enum;
    case FUNCTION: {
      auto const str = instruction.original_string(tu.source_files[instruction.file_idx]);
      auto const function = backend.module.getFunction(
        TemporaryStringCat{ tu.name, "::", str }
        ); assert(function);
      return function;
    }
    case MODULE_FUNCTION:
      return getFunctionImport(instruction);

    case LOCAL:
      assert(instruction.local_data.idx < locals.size());
      return locals[instruction.local_data.idx];

    case TYPE_VARIABLE: {
      auto const custom_typeID = instruction.custom_typeID();
      auto const id = instruction.type_member_data.member_id;
      return builder.CreateStructGEP(translateType(custom_typeID), genRefExpression(), id);
    }

    case SUBSCRIPT: {
      auto const array = genRefExpression(); auto const array_type = instruction.subscript_data.array_typeID;
      auto const index = genValueExpression();
      return builder.CreateGEP( translateType(array_type), array, {i64_0, index} );
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
      auto const right = builder.CreateZExt(genValueExpression(), llvm::IntegerType::get(backend.context, instruction.cast_assign_data.bitwidth));
      return builder.CreateStore(right, left);
    }
    case SCAST_ASSIGN: {
      auto const left = genRefExpression();
      auto const right = builder.CreateSExt(genValueExpression(), llvm::IntegerType::get(backend.context, instruction.cast_assign_data.bitwidth));
      return builder.CreateStore(right, left);
    }

    case DEREFERENCE:
      return builder.CreateLoad(ptr, genRefExpression());

    case CALL: {
      auto const fn = genRefExpression();
      
      llvm::SmallVector<llvm::Value*, 8> parameters;
      for (sz_t i{}; i<instruction.call_data.num_parameters; ++i)
        parameters.push_back(genValueExpression());

      return builder.CreateCall(
        llvm::cast<llvm::Function>(fn),
        parameters
        );
    }

    default:
      Unreachable("Invalid peep instruction type.");
    }
  }

  void genBlock(sz_t block_idx) {
    llvm::Value* branch_value{};
    auto const& block = peepir_blocks[block_idx];

    auto const instruction_cutoff = instruction_idx + block.num_instructions;
    while (instruction_idx < instruction_cutoff)
      branch_value = genValueExpression();

    switch (block.terminator_type) { using enum PeepIR::Block::Terminator;
    case BR:
      builder.CreateBr(
        llvm_blocks[block.br.next_block_offset + (i16_t)block_idx]);
      break;
    case BRC:
      builder.CreateCondBr(
        builder.CreateTrunc(branch_value, i1),
        llvm_blocks[block.brc.true_block_offset + (i16_t)block_idx],
        llvm_blocks[block.brc.false_block_offset + (i16_t)block_idx]);
      break;
    case RET:
      builder.CreateRet(branch_value);
      break;
    default:
      Unreachable("Invalid block terminator type.");
    }
  }

  void genFunction(u16_t fn_id, llvm::Function* llvm_fn) {

    // reset state
    {
      instruction_idx = 0;
      locals.clear();
      llvm_blocks.clear();

      auto& fn = tu.functions[fn_id];
      instructions = std::move(fn.instructions);
      peepir_blocks = std::move(fn.blocks);
      return_type = llvm_fn->getReturnType();
    }

    // initialize state for new function
    {
      auto const& fn_symbol = tu.module.getFunction(fn_id);

      llvm_blocks.reserve(peepir_blocks.size());
      for (sz_t i{}; i<peepir_blocks.size(); ++i)
        llvm_blocks.emplace_back_unchecked(llvm::BasicBlock::Create(backend.context, "", llvm_fn));
      builder.SetInsertPoint(llvm_blocks.front());

      locals.reserve(fn_symbol.locals.size());

      auto const llvm_fn_type = llvm_fn->getFunctionType();
      auto const param_types = llvm_fn_type->param_begin();
      auto const num_params = llvm_fn_type->getNumParams();
      auto arg = llvm_fn->arg_begin();

      sz_t i{};
      for (; i<num_params; ++i) {
        auto const param_alloca = builder.CreateAlloca(param_types[i]);
        builder.CreateStore(arg, param_alloca);
        locals.emplace_back_unchecked(param_alloca);
        ++arg;
      }

      // we don't reset i here because locals includes parameters
      auto const num_locals = fn_symbol.locals.size();
      for (; i<num_locals; ++i)
        locals.emplace_back_unchecked( builder.CreateAlloca(translateType(fn_symbol.getLocal(i).typeID)) );

    }

    auto const num_blocks = peepir_blocks.size();
    for (sz_t i{}; i<num_blocks; ++i) {
      builder.SetInsertPoint(llvm_blocks[i]);
      genBlock(i);
    }

    if (verifyFunction(*llvm_fn, &llvm::errs())) {
      backend.module.print(llvm::outs(), nullptr);
      std::quick_exit(1);
    }
  }

public:

  static std::unique_ptr<LLVM_Backend>
  lowerToLLVM(PeepIR::TU& peeped_tu) {
    auto backend = std::make_unique<LLVM_Backend>(peeped_tu.name);
    LoweringContext codegen(*backend, peeped_tu);

    llvm::SmallVector<llvm::Function*, 128> llvm_fns;
    auto const num_fns = peeped_tu.functions.size();

    // add all functions to llvm table
    for (sz_t i{}; i<num_fns; ++i) {
      auto const& fn_symbol = peeped_tu.module.getFunction(eden::narrow<u16_t>(i));
      llvm_fns.emplace_back(
        llvm::Function::Create(
          codegen.translateFunctionType(fn_symbol),
          fn_symbol.is_public ? llvm::Function::ExternalLinkage : llvm::Function::InternalLinkage,
          /*AddrSpace=*/0,
          llvm::Twine(peeped_tu.name) + "::" + fn_symbol.nameof(),
          &backend->module)
        );
    }

    // actual codegen
    for (sz_t i{}; i<num_fns; ++i)
      codegen.genFunction(eden::narrow<u16_t>(i), llvm_fns[i]);

    if (peeped_tu.module.getID() == MAIN_TU_ID) [[unlikely]] {
      auto const main_fn = backend->module.getFunction( TemporaryStringCat{peeped_tu.name, "::main"} );
      assert(main_fn); // should've been checked for existence after parsing
      main_fn->setName("main");
    }

    return backend;
  }

  explicit LoweringContext(LLVM_Backend& backend, PeepIR::TU& peeped_tu)
  : backend(backend), tu(peeped_tu), builder(backend.context) {
    i1 = llvm::Type::getInt1Ty(backend.context);
    i8 = llvm::Type::getInt8Ty(backend.context);
    bool_true = llvm::ConstantInt::get(i8, 1); bool_false = llvm::ConstantInt::get(i8, 0);

    i16 = llvm::Type::getInt16Ty(backend.context);
    i32 = llvm::Type::getInt32Ty(backend.context);
    i64 = llvm::Type::getInt64Ty(backend.context); i64_0 = llvm::ConstantInt::get(i64, 0);
    f32 = llvm::Type::getFloatTy(backend.context);
    f64 = llvm::Type::getDoubleTy(backend.context);
    devoid = llvm::Type::getVoidTy(backend.context);
    ptr = llvm::PointerType::get(backend.context, 0);

    auto const num_custom_types = peeped_tu.module.numCustomTypes();
    custom_type_map.reserve(num_custom_types);
  }

};

}

std::unique_ptr<Backend> ToLLVM::codegen(PeepIR::TU& peeped_tu) {
#ifdef STAGE_BENCHMARKS
  auto const begin_time = std::chrono::high_resolution_clock::now();
#endif

  auto backend = LoweringContext::lowerToLLVM(peeped_tu);

#ifdef STAGE_BENCHMARKS
  auto const end_time = std::chrono::high_resolution_clock::now();
  std::println("{:>10}, {:>10} | LLVM {}",
    end_time - begin_time,
    std::chrono::duration_cast<std::chrono::microseconds>(end_time - begin_time),
    peeped_tu.name
  );
#endif

#ifdef PROFILE // free all files when profiling to prevent insane memory leaks
  for (auto file_ : peeped_tu.source_files)
    free_file(file_);
#endif

  return backend;
}