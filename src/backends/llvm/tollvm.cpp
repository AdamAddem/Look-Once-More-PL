#include "tollvm.hpp"
#include "ast/ast.hpp"
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
/*
void finalizeFunction() {
  if (not builder.GetInsertBlock()->back().isTerminator())
    builder.CreateBr(return_block);
  func->insert(func->end(), return_block);
  builder.SetInsertPoint(return_block);
  if (not return_type->isVoidTy()) {
    llvm::Value* v = builder.CreateLoad(return_type, locals[retval_location_name]);
    builder.CreateRet(v);
  }
  else
    builder.CreateRetVoid();

  if (verifyFunction(*func, &llvm::errs())) {
    return_block->getModule()->print(llvm::outs(), nullptr);
    throw BackendError("Failed to verify Function!", func->getName().str(), 0);
  }
} */


using PeepMIR::released_string;
using PeepMIR::released_span;
using PeepMIR::released_ptr;


//shes big but shes a beaut
class TU final : public Backend {
  llvm::LLVMContext context;
  llvm::Module module;

  llvm::IntegerType* i1; llvm::ConstantInt* i1_0; llvm::ConstantInt* i1_1;
  llvm::IntegerType* i8; llvm::ConstantInt* i8_0; llvm::ConstantInt* i8_1;
  llvm::IntegerType* i16; llvm::ConstantInt* i16_0; llvm::ConstantInt* i16_1;
  llvm::IntegerType* i32; llvm::ConstantInt* i32_0; llvm::ConstantInt* i32_1;
  llvm::IntegerType* i64; llvm::ConstantInt* i64_0; llvm::ConstantInt* i64_1;
  llvm::Type* f32;  llvm::Constant* f32_0; llvm::Constant* f32_1;
  llvm::Type* f64; llvm::Constant* f64_0; llvm::Constant* f64_1;
  llvm::Type* devoid;

  [[nodiscard]] std::filesystem::path
  createASMFile(const std::filesystem::path &file) override {
    static const std::filesystem::path asm_folder = Settings::getBuildLocation() + "asm/";
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
    static const std::filesystem::path ir_path = Settings::getBuildLocation() + "llvm_ir/";
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
    static const std::filesystem::path object_folder = Settings::getBuildLocation() + "obj/";
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


  [[nodiscard]] llvm::Type*
  translateType(const Type* t) const {
    assert(not t->isCustom());
    assert(not t->isVariant());
    assert(not t->isPointer());
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

    std::unreachable();
  }

  [[nodiscard]] llvm::IntegerType*
  typeForInteger(i64_t int_literal) const noexcept {
    return
    llvm::cast<llvm::IntegerType>(translateType(signedToLiteralInstance(int_literal).type));
  }

  [[nodiscard]] llvm::IntegerType*
  typeForUnsigned(u64_t uint_literal) const noexcept {
    return
    llvm::cast<llvm::IntegerType>(translateType(signedToLiteralInstance(uint_literal).type));
  }

  friend class Function;
  struct Function {
    TU* tu;

    llvm::Type* return_type;
    llvm::Function* llvmfunc;
    llvm::IRBuilder<>* builder;
    std::vector<llvm::AllocaInst*> locals;

    released_ptr<PeepMIR::Instruction> instructions; sz_t instruction_idx{};
    released_span<PeepMIR::Block> mir_blocks; sz_t block_idx{};
    std::vector<llvm::BasicBlock*> llvm_blocks;

    [[nodiscard]] llvm::Constant*
    fpConstant(llvm::Type* t, double value) const noexcept
    {return llvm::ConstantFP::get(t, value);}

    [[nodiscard]] llvm::Constant*
    signedConstant(llvm::Type* t, i64_t value) const noexcept
    {return llvm::ConstantInt::get(llvm::cast<llvm::IntegerType>(t), value, true);}

    [[nodiscard]] llvm::Constant*
    unsignedConstant(llvm::Type* t, u64_t value) const noexcept
    {return llvm::ConstantInt::get(llvm::cast<llvm::IntegerType>(t), value);}

    void genUnary(PeepMIR::Instruction::Type type) noexcept {
      const auto var = llvm::cast<llvm::AllocaInst>(expression_buffer.back());
      const bool is_float = var->getType()->isFloatingPointTy();
      llvm::Value* res{};
      switch (type) {
      using enum PeepMIR::Instruction::Type;
      case PRE_INC:
        res = is_float
        ? builder->CreateFAdd(var, fpConstant(var->getType(), 1))
        : builder->CreateAdd(var, unsignedConstant(var->getType(), 1));
        builder->CreateStore(res, var);
        break;

      case PRE_DEC:
        res = is_float
        ? builder->CreateFSub(var, fpConstant(var->getType(), 1))
        : builder->CreateSub(var, unsignedConstant(var->getType(), 1));
        builder->CreateStore(res, var);
        break;

      case NEGATE:
        res = is_float ? builder->CreateFNeg(var) : builder->CreateNeg(var);
        break;

      case ADDRESS_OF:
        res = var;
        break;

      case BITNOT:
        res = builder->CreateNot(var);
        break;

      case POST_INC:
        builder->CreateStore(
          is_float
          ? builder->CreateFAdd(var, fpConstant(var->getType(), 1))
          : builder->CreateAdd(var, unsignedConstant(var->getType(), 1)),
          var
          );
        res = var;
        break;

      case POST_DEC:
        builder->CreateStore(
          is_float
          ? builder->CreateFSub(var, fpConstant(var->getType(), 1))
          : builder->CreateSub(var, unsignedConstant(var->getType(), 1)),
          var
          );
        res = var;
        break;

      default:
        std::unreachable();
      }

      expression_buffer.clear();
      expression_buffer.emplace_back(res);
    }

    void genBinary(PeepMIR::Instruction::Type type) noexcept {
      auto e = expression_buffer.crbegin();
      auto right = *(e++);
      right =
      llvm::isa<llvm::AllocaInst>(right)
      ? builder->CreateLoad(llvm::cast<llvm::AllocaInst>(right)->getAllocatedType(), right)
      : right;

      auto left = *e;
      const bool is_float = left->getType()->isFloatingPointTy();
      llvm::Value* res{};

      switch (type) {
        using enum PeepMIR::Instruction::Type;
      case ADD:
        res = is_float ?
        builder->CreateFAdd(left, right) : builder->CreateAdd(left, right);
        break;
      case SUB:
        res = is_float ?
        builder->CreateFSub(left, right) : builder->CreateSub(left, right);
        break;
      case MULT:
        res = is_float ?
        builder->CreateFMul(left, right) : builder->CreateMul(left, right);
        break;
      case DIV:
        res = is_float ?                       // CHANGE THIS
        builder->CreateFDiv(left, right) : builder->CreateSDiv(left, right);
        break;
      case MOD:
        res = is_float ?                       // CHANGE THIS
        builder->CreateFRem(left, right) : builder->CreateSRem(left, right);
        break;
      case ASSIGN:
        assert(llvm::isa<llvm::AllocaInst>(left));
        builder->CreateStore(
          right,
          llvm::cast<llvm::AllocaInst>(left));
        res = left;
        break;
      case LESS:
        res = builder->CreateCmp( // CHANGE THIS
          is_float ? llvm::CmpInst::Predicate::FCMP_OLT : llvm::CmpInst::Predicate::ICMP_ULT, left, right
          );
        break;
      case GTR:
        res = builder->CreateCmp(
          is_float ? llvm::CmpInst::Predicate::FCMP_OGT : llvm::CmpInst::Predicate::ICMP_UGT, left, right
          );
        break;
      case LEQ:
        res = builder->CreateCmp(
          is_float ? llvm::CmpInst::Predicate::FCMP_OLE : llvm::CmpInst::Predicate::ICMP_ULE, left, right
        );
        break;
      case GEQ:
        res = builder->CreateCmp(
          is_float ? llvm::CmpInst::Predicate::FCMP_OGE : llvm::CmpInst::Predicate::ICMP_UGE, left, right
        );
        break;
      case AND:
        assert(not is_float);
        res = builder->CreateLogicalAnd(left, right);
        break;
      case OR:
        assert(not is_float);
        res = builder->CreateLogicalOr(left, right);
        break;
      case BITAND:
        res = builder->CreateAnd(left, right);
        break;
      case BITOR:
        res = builder->CreateOr(left, right);
        break;
      case BITXOR:
        res = builder->CreateXor(left, right);
        break;
      case EQ:
        res = builder->CreateCmp(
          is_float ? llvm::CmpInst::Predicate::FCMP_OEQ : llvm::CmpInst::Predicate::ICMP_EQ, left, right
          );
        break;
      case NEQ:
        res = builder->CreateCmp(
          is_float ? llvm::CmpInst::Predicate::FCMP_ONE : llvm::CmpInst::Predicate::ICMP_NE, left, right
        );
        break;
      default:
        std::unreachable();
      }

      expression_buffer.clear();
      expression_buffer.emplace_back(res);
    }

    std::vector<llvm::Value*> expression_buffer;
    void genExpression() noexcept {
      auto& instruction = instructions[instruction_idx++];
      switch (instruction.type) {
        using enum PeepMIR::Instruction::Type;
      case NOOP:
        return;
      case GLOBAL:
        expression_buffer.emplace_back(tu->module.getNamedGlobal(instruction.string_value()));
        assert(expression_buffer.back());
        instruction.release_string_value().destroy_and_deallocate();
        return;

      case LOCAL:
        assert(instruction.local_idx() < locals.size());
        expression_buffer.emplace_back(locals[instruction.local_idx()]);
        return;

      case FUNCTION:
        expression_buffer.emplace_back(tu->module.getFunction(instruction.function_name())); assert(expression_buffer.back());
        instruction.release_string_value().destroy_and_deallocate();
        return;

      case INT_LITERAL:
        expression_buffer.emplace_back(
          llvm::ConstantInt::get(tu->typeForInteger(instruction.int_value()),
          std::bit_cast<u64_t>(instruction.int_value()), true));
        return;
      case UINT_LITERAL:
        expression_buffer.emplace_back(llvm::ConstantInt::get(tu->typeForUnsigned(instruction.int_value()),
          instruction.uint_value()));
        return;
      case FLOAT_LITERAL:
        expression_buffer.emplace_back(llvm::ConstantFP::get(tu->f32, instruction.float_value()));
        return;
      case DOUBLE_LITERAL:
        expression_buffer.emplace_back(llvm::ConstantFP::get(tu->f64, instruction.double_value()));
        return;
      case BOOL_LITERAL:
        expression_buffer.emplace_back(instruction.bool_value() ? tu->i1_1 : tu->i1_0);
        return;
      case CHAR_LITERAL:
        expression_buffer.emplace_back(llvm::ConstantInt::get(tu->i8, instruction.char_value()));
        return;
      case STRING_LITERAL:
        expression_buffer.emplace_back(llvm::ConstantDataArray::getString(tu->context, instruction.string_value()));
        instruction.release_string_value().destroy_and_deallocate();
        return;

      case ADD:
      case SUB:
      case MULT:
      case DIV:
      case MOD:
      case ASSIGN:
      case LESS:
      case GTR:
      case LEQ:
      case GEQ:
      case AND:
      case OR:
      case BITAND:
      case BITOR:
      case BITXOR:
      case BITNOT:
      case EQ:
      case NEQ:
        genBinary(instruction.type);
        return;

      case PRE_INC:
      case PRE_DEC:
      case ADDRESS_OF:
      case NEGATE:
      case POST_INC:
      case POST_DEC:
        genUnary(instruction.type);
        return;

      case CALL: {
        assert(expression_buffer.size() >= instruction.num_params() + 1);
        auto iter = expression_buffer.crbegin();
        llvm::Value* parameters[Settings::MAX_FUNCTION_PARAMETERS];
        for (auto i{0uz}; i<instruction.num_params(); ++i)
          parameters[i] = *iter++;

        auto call_res = builder->CreateCall(
          llvm::cast<llvm::Function>(*iter),
          {parameters, instruction.num_params()}
          );
        expression_buffer.clear();
        expression_buffer.emplace_back(call_res);
        return;
      }
      default:
        std::unreachable();
      }

    }

    llvm::Value* branch_value;
    void genBlock(u32_t instruction_cut_off) {
      while (instruction_idx < instruction_cut_off)
        genExpression();

      branch_value = expression_buffer.back();
      expression_buffer.clear();
      const auto& block = mir_blocks[block_idx];
      switch (block.terminator_type) {
        using enum PeepMIR::Block::Terminator;
      case BR:
        builder->CreateBr(
          llvm_blocks[block.br.next_block_idx]);
        break;
      case BRC:
        builder->CreateCondBr( branch_value,
          llvm_blocks[block.brc.true_block_idx],
          llvm_blocks[block.brc.false_block_idx]);
        break;
      case RET:
        builder->CreateRet(branch_value);
        break;
      default:
        std::unreachable();
      }
    }

    void genFunction() {
      const auto num_blocks = mir_blocks.size();
      while (block_idx < (num_blocks - 1)) {
        genBlock(mir_blocks[block_idx+1].first_instruction_idx);
        ++block_idx;
      }
      builder->SetInsertPoint(&llvmfunc->back());
      if (return_type->isVoidTy()) {
        builder->CreateRetVoid();
      }
      else {
        builder->CreateRet(
            builder->CreateLoad(locals[0]->getAllocatedType(), locals[0])
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

  void genGlobals() {
    /*
    llvm::Constant* v = var.expr ?  genConstant(*var.expr) : llvm::Constant::getNullValue(translateType(var.type.type));
    module.insertGlobalVariable(
      new llvm::GlobalVariable(
       translateType(var.type.type),
       not var.type.details.is_mutable,
       llvm::GlobalValue::LinkageTypes::ExternalLinkage,
       v,
       var.ident
       )
     );
     */
  }

  void compileFunction(PeepMIR::Function& func) {
    Function lowering_function; lowering_function.tu = this;
    lowering_function.llvm_blocks.reserve(func.blocks.size());

    llvm::Type* arg_types[Settings::MAX_FUNCTION_PARAMETERS];
    const auto function_type = func.type;
    auto num_params{0uz};
    for (const auto t : function_type->parameterTypes()) {
      arg_types[num_params] = translateType(t);
      ++num_params;
    }

    //hack
    lowering_function.return_type =
      function_type->returnType()->isBool() ? i1 : translateType(function_type->returnType());

    const auto func_type = llvm::FunctionType::get(lowering_function.return_type, {arg_types, num_params}, false);
    lowering_function.llvmfunc = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, 0, std::string_view(func.name), &module);
    const auto entry = llvm::BasicBlock::Create(context, "", lowering_function.llvmfunc);
    llvm::IRBuilder builder(entry); lowering_function.builder = &builder;
    lowering_function.llvm_blocks.emplace_back(entry);
    for (auto i{1uz}; i<func.blocks.size(); ++i) {
      lowering_function.llvm_blocks.emplace_back(
        llvm::BasicBlock::Create(context, "", lowering_function.llvmfunc));
    }

    auto arg = lowering_function.llvmfunc->arg_begin();
    lowering_function.locals.reserve(func.locals.size() + 1);
    lowering_function.locals.emplace_back(
      (lowering_function.return_type->isVoidTy()) ? nullptr
      : builder.CreateAlloca(lowering_function.return_type, nullptr) );

    for (auto i{0uz}; i<num_params; ++i) {
      llvm::AllocaInst* param_alloca = builder.CreateAlloca(arg_types[i], nullptr);
      builder.CreateStore(arg, param_alloca);
      lowering_function.locals.emplace_back(param_alloca);
      ++arg, ++i;
    }

    const auto num_locals = func.locals.size();
      for (auto i{num_params + 1}; i<num_locals; ++i) {
        lowering_function.locals.emplace_back(
          builder.CreateAlloca(translateType(func.locals[i]), nullptr)
          );
    }

    func.name.destroy_and_deallocate();
    func.locals.destroy_and_deallocate();
    lowering_function.instructions = std::move(func.instructions);
    lowering_function.mir_blocks = std::move(func.blocks);
    lowering_function.genFunction();



  }

public:

  void lowerToLLVM(PeepMIR::TU& tu) {
    //for (const auto& v : tu.globals)
      //void;

    for (auto& func : tu.functions)
      compileFunction(func);
  }

  explicit TU(const std::string& filename)
  : module(filename, context) {
    i1 = llvm::Type::getInt1Ty(context); i1_0 = llvm::ConstantInt::get(i1, 0); i1_1 = llvm::ConstantInt::get(i1, 1);
    i8 = llvm::Type::getInt8Ty(context); i8_0 = llvm::ConstantInt::get(i8, 0); i8_1 = llvm::ConstantInt::get(i8, 1);
    i16 = llvm::Type::getInt16Ty(context); i16_0 = llvm::ConstantInt::get(i16, 0); i16_1 = llvm::ConstantInt::get(i16, 1);
    i32 = llvm::Type::getInt32Ty(context); i32_0 = llvm::ConstantInt::get(i32, 0); i32_1 = llvm::ConstantInt::get(i32, 1);
    i64 = llvm::Type::getInt64Ty(context); i64_0 = llvm::ConstantInt::get(i64, 0); i64_1 = llvm::ConstantInt::get(i64, 1);

    f32 = llvm::Type::getFloatTy(context); f32_0 = llvm::ConstantFP::get(f32, 0); f32_1 = llvm::ConstantFP::get(f32, 1);
    f64 = llvm::Type::getDoubleTy(context); f64_0 = llvm::ConstantFP::get(f64, 0); f64_1 = llvm::ConstantFP::get(f64, 1);
    devoid = llvm::Type::getVoidTy(context);
  }

  void printModule(llvm::raw_ostream& out = llvm::outs()) const
  { module.print(out, nullptr); }


};
}

std::unique_ptr<Backend> ToLLVM::codegen(PeepMIR::TU&& peeped_tu, const std::filesystem::path &file) {
  auto ptr = std::make_unique<TU>(file.string());
  ptr->lowerToLLVM(peeped_tu);
  ptr->printModule();
  return ptr;
}
