#include "tollvm.hpp"
/*
#include "ast/ast.hpp"
#include "error.hpp"
#include "peep_mir/peep_mir.hpp"
#include "settings.hpp"
#include "utilities/variant_overload.hpp"

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

using namespace llvm;

namespace {
struct FunctionBuilder {
  static constexpr std::string retval_location_name = "__retval";
  using LocalsMap = std::unordered_map<std::string, AllocaInst*>;
  Type* return_type;
  Function* func;
  BasicBlock* return_block;

  IRBuilder<> builder;
  LocalsMap locals;

  FunctionBuilder(Type* ret_type, Function* function, BasicBlock* entry, BasicBlock* ret_block, LocalsMap locals_map) :
  return_type(ret_type), func(function), return_block(ret_block), builder(entry), locals(std::move(locals_map)) {}

  void finalizeFunction() {
    if (!builder.GetInsertBlock()->back().isTerminator())
      builder.CreateBr(return_block);
    func->insert(func->end(), return_block);
    builder.SetInsertPoint(return_block);
    if (!return_type->isVoidTy()) {
      Value* v = builder.CreateLoad(return_type, locals[retval_location_name]);
      builder.CreateRet(v);
    }
    else
      builder.CreateRetVoid();

    if (verifyFunction(*func, &errs())) {
      return_block->getModule()->print(outs(), nullptr);
      throw BackendError("Failed to verify Function!", func->getName().str(), 0);
    }
  }

};

//shes big but shes a beauty
class TU final : public Backend {
  LLVMContext context;
  Module module;

  IntegerType* i1; ConstantInt* i1_0; ConstantInt* i1_1;
  IntegerType* i8; ConstantInt* i8_0; ConstantInt* i8_1;
  IntegerType* i16; ConstantInt* i16_0; ConstantInt* i16_1;
  IntegerType* i32; ConstantInt* i32_0; ConstantInt* i32_1;
  IntegerType* i64; ConstantInt* i64_0; ConstantInt* i64_1;
  Type* f32;  Constant* f32_0; Constant* f32_1;
  Type* f64; Constant* f64_0; Constant* f64_1;
  Type* devoid;


public:
  explicit TU(const std::string& filename) : module(filename, context)  {
    i1 = Type::getInt1Ty(context); i1_0 = ConstantInt::get(i1, 0); i1_1 = ConstantInt::get(i1, 1);
    i8 = Type::getInt8Ty(context); i8_0 = ConstantInt::get(i8, 0); i8_1 = ConstantInt::get(i8, 1);
    i16 = Type::getInt16Ty(context); i16_0 = ConstantInt::get(i16, 0); i16_1 = ConstantInt::get(i16, 1);
    i32 = Type::getInt32Ty(context); i32_0 = ConstantInt::get(i32, 0); i32_1 = ConstantInt::get(i32, 1);
    i64 = Type::getInt64Ty(context); i64_0 = ConstantInt::get(i64, 0); i64_1 = ConstantInt::get(i64, 1);

    f32 = Type::getFloatTy(context); f32_0 = ConstantFP::get(f32, 0); f32_1 = ConstantFP::get(f32, 1);
    f64 = Type::getDoubleTy(context); f64_0 = ConstantFP::get(f64, 0); f64_1 = ConstantFP::get(f64, 1);
    devoid = Type::getVoidTy(context);
  }

  [[nodiscard]] Type* getType(const AST::Type* t) const {
    assert(not t->isCustom());
    assert(not t->isVariant());
    assert(not t->isPointer());
    assert(t not_eq &AST::string_type);

    if (t == &AST::i32_type || t == &AST::u32_type)
      return i32;
    if (t == AST::devoid_type)
      return devoid;
    if (t == &AST::i64_type || t == &AST::u64_type)
      return i64;

    if (t == &AST::f32_type)
      return f32;
    if (t == &AST::f64_type)
      return f64;

    if (t == &AST::bool_type || t == &AST::char_type || t == &AST::i8_type || t == &AST::u8_type)
      return i8;
    if (t == &AST::i16_type || t == &AST::u16_type)
      return i16;

    assert(false && "invalid type idfk");
  }
  void genGlobal(const AST::VarDeclaration& var) {
    Constant* v = var.expr ?  genConstant(*var.expr) : Constant::getNullValue(getType(var.type.type));
    module.insertGlobalVariable(
      new GlobalVariable(
       getType(var.type.type),
       not var.type.details.is_mutable,
       GlobalValue::LinkageTypes::ExternalLinkage,
       v,
       var.ident
       )
     );
  }
  [[nodiscard]] FunctionBuilder createFunction(const PeepMIR::Function& func) {
    std::vector<Type*> arg_types;
    arg_types.reserve(func.parameter_list.size());
    for (const auto& v : func.parameter_list)
      arg_types.push_back(getType(v.type.type));

    //hack
    Type* ret_type = func.return_type->isBool() ? i1 : getType(func.return_type);
    const auto func_type = FunctionType::get(ret_type, {arg_types.data(), arg_types.size()}, false);
    const auto llvmfunc = Function::Create(func_type, Function::ExternalLinkage, 0, func.name, &module);
    const auto entry = BasicBlock::Create(context, "__entry", llvmfunc);
    const auto ending = BasicBlock::Create(context, "__return");
    IRBuilder<> init(entry);

    auto arg = llvmfunc->arg_begin();
    FunctionBuilder::LocalsMap locals;
    if (!ret_type->isVoidTy())
      locals.emplace(FunctionBuilder::retval_location_name, init.CreateAlloca(ret_type, nullptr, FunctionBuilder::retval_location_name));

    auto i{0uz};
    for (const auto& v : func.parameter_list) {
      assert(!locals.contains(arg->getName().str()));
      arg->setName(v.ident);

      AllocaInst* param_alloca = init.CreateAlloca(arg_types[i], nullptr);
      locals.emplace(arg->getName().str(), param_alloca);

      init.CreateStore(arg, param_alloca);
      ++arg;
      ++i;
    }

    return FunctionBuilder(ret_type, llvmfunc, entry, ending, std::move(locals));
  }
  void lowerToLLVM(const PeepMIR::PeepTU& vtu) {
    for (const auto& v : vtu.globals)
      genGlobal(v);

    for (const auto& func : vtu.functions) {
      auto function_builder = createFunction(func);
      for (const auto s : func.function_body)
        genStatement(*s, function_builder);

      function_builder.finalizeFunction();
    }

  }

  Value* genUnary(const AST::UnaryExpression& unary, FunctionBuilder& f) const noexcept {
    const auto load = cast<LoadInst>(genIdentifier(std::get<AST::IdentifierExpression>(*unary.expr), f));
    const bool is_float = load->getAccessType()->isFloatingPointTy();
    Value* result;
    IRBuilder<>& builder = f.builder;
    switch (unary.opr) {
    case AST::Operator::PRE_INCREMENT:
      result = is_float ? builder.CreateFAdd(load, f32_1) : builder.CreateAdd(load, i32_1);
      builder.CreateStore(result, load->getPointerOperand() );
      return result;

    case AST::Operator::PRE_DECREMENT:
      result = is_float ? builder.CreateFSub(load, f32_1) : builder.CreateSub(load, i32_1);
      builder.CreateStore(result, load->getPointerOperand() );
      return result;

    case AST::Operator::UNARY_MINUS:
      return is_float ? builder.CreateFNeg(load) : builder.CreateNeg(load);

    case AST::Operator::ADDRESS_OF:
    case AST::Operator::BITNOT:
      return builder.CreateNot(load);
    case AST::Operator::NOT:
      assert(false && "Operator NOT used in codegen, should have been changed to operator bitnot in the ast_validator. Fix that thx");

    case AST::Operator::POST_INCREMENT:
      builder.CreateStore(
        is_float ? builder.CreateFAdd(load, f32_1) : builder.CreateAdd(load, i32_1),
        load->getPointerOperand());
      return load;

    case AST::Operator::POST_DECREMENT:
      builder.CreateStore(
      is_float ? builder.CreateFSub(load, f32_1) : builder.CreateSub(load, i32_1),
      load->getPointerOperand());
      return load;

    case AST::Operator::CAST:
    case AST::Operator::CAST_IF:
    case AST::Operator::UNSAFE_CAST:
    default:
      assert(false && "Unimplemented expression in codegen srry");
    }

    assert(false && "wtf");
  }
  Value* genBinary(const AST::BinaryExpression& binary,  FunctionBuilder& f) noexcept {
    Value* left = genExpression(*binary.expr_left, f);
    const bool is_float = left->getType()->isFloatingPointTy();
    IRBuilder<>& builder = f.builder;

    switch (binary.opr) {
    case AST::Operator::ADD: {
      Value* right = genExpression(*binary.expr_right, f);
      return is_float ? builder.CreateFAdd(left, right) : builder.CreateAdd(left, right);
    }
    case AST::Operator::SUBTRACT: {
      Value* right = genExpression(*binary.expr_right, f);
      return is_float ? builder.CreateFAdd(left, right) : builder.CreateAdd(left, right);
    }
    case AST::Operator::MULTIPLY: {
      Value* right = genExpression(*binary.expr_right, f);
      return is_float ? builder.CreateFMul(left, right) : builder.CreateMul(left, right);
    }
    case AST::Operator::DIVIDE: {
      Value* right = genExpression(*binary.expr_right, f);
      return is_float ? builder.CreateFDiv(left, right) : builder.CreateSDiv(left, right);
    }
    case AST::Operator::POWER:
      assert(false && "Unimplemented exponent operator");
    case AST::Operator::MODULUS: {
      Value* right = genExpression(*binary.expr_right, f);
      return is_float ? builder.CreateFRem(left, right) : builder.CreateSRem(left, right);
    }
    case AST::Operator::ASSIGN:
      assert(isa<LoadInst>(left));
      builder.CreateStore(genExpression(*binary.expr_right, f), cast<LoadInst>(left)->getPointerOperand());
      return left;
    case AST::Operator::LESS:
      return builder.CreateCmp( is_float ? CmpInst::Predicate::FCMP_OLT : CmpInst::Predicate::ICMP_ULT, left, genExpression(*binary.expr_right, f));
    case AST::Operator::GREATER:
      return builder.CreateCmp( is_float ? CmpInst::Predicate::FCMP_OGT : CmpInst::Predicate::ICMP_UGT, left, genExpression(*binary.expr_right, f));
    case AST::Operator::LESS_EQUAL:
      return builder.CreateCmp( is_float ? CmpInst::Predicate::FCMP_OLE : CmpInst::Predicate::ICMP_ULE, left, genExpression(*binary.expr_right, f));
    case AST::Operator::GREATER_EQUAL:
      return builder.CreateCmp( is_float ? CmpInst::Predicate::FCMP_OGE : CmpInst::Predicate::ICMP_UGE, left, genExpression(*binary.expr_right, f));
    case AST::Operator::AND: {
      assert(!is_float && "float expression used as conditional");
      const auto first_value = builder.CreateCmp(CmpInst::Predicate::ICMP_NE, builder.CreateZExtOrTrunc(left, i1), i1_0);

      BasicBlock* begin = builder.GetInsertBlock();
      BasicBlock* test_second = BasicBlock::Create(context); f.func->insert(f.func->end(), test_second);
      BasicBlock* end = BasicBlock::Create(context);
      builder.CreateCondBr(first_value, test_second, end);

      builder.SetInsertPoint(test_second);
      Value* right = genExpression(*binary.expr_right, f);
      const auto second_value = builder.CreateCmp(CmpInst::Predicate::ICMP_NE, builder.CreateZExtOrTrunc(right, i1), i1_0);
      f.func->insert(f.func->end(), end);
      builder.CreateBr(end);

      builder.SetInsertPoint(end);
      const auto phi =  builder.CreatePHI(i1, 2);
      phi->addIncoming(i1_0, begin);
      phi->addIncoming(second_value, test_second);
      return phi;
    }
    case AST::Operator::OR: {
      assert(!is_float && "float expression used as conditional");
      const auto first_value = builder.CreateCmp(CmpInst::Predicate::ICMP_NE, builder.CreateZExtOrTrunc(left, i1), i1_0);

      BasicBlock* begin = builder.GetInsertBlock();
      BasicBlock* test_second = BasicBlock::Create(context); f.func->insert(f.func->end(), test_second);
      BasicBlock* end = BasicBlock::Create(context);
      builder.CreateCondBr(first_value, end, test_second);

      builder.SetInsertPoint(test_second);
      Value* right = genExpression(*binary.expr_right, f);
      const auto second_value = builder.CreateCmp(CmpInst::Predicate::ICMP_NE, builder.CreateZExtOrTrunc(right, i1), i1_0);
      f.func->insert(f.func->end(), end);
      builder.CreateBr(end);

      builder.SetInsertPoint(end);
      const auto phi =  builder.CreatePHI(i1, 2);
      phi->addIncoming(i1_1, begin);
      phi->addIncoming(second_value, test_second);
      return phi;
    }
    case AST::Operator::XOR:
      assert(false && "Operator XOR should have been changed to not_eq in the ast_validator. If you're seeing this, that didn't happen for some reason, soz");
    case AST::Operator::BITAND:
      return builder.CreateAnd(left, genExpression(*binary.expr_right, f));
    case AST::Operator::BITOR:
      return builder.CreateOr(left, genExpression(*binary.expr_right, f));
    case AST::Operator::BITXOR:
      return builder.CreateXor(left, genExpression(*binary.expr_right, f));

    case AST::Operator::EQUAL:
      return builder.CreateCmp( is_float ? CmpInst::Predicate::FCMP_OEQ : CmpInst::Predicate::ICMP_EQ, left, genExpression(*binary.expr_right, f));
    case AST::Operator::NOT_EQUAL:
      return builder.CreateCmp( is_float ? CmpInst::Predicate::FCMP_ONE : CmpInst::Predicate::ICMP_NE, left, genExpression(*binary.expr_right, f));

    default: assert(false && "Binary operator not supported");
    }

    assert(false && "wtf");
  }
  Value* genCalling(const AST::CallingExpression& calling, FunctionBuilder& f) noexcept {
    std::vector<Value*> params;
    params.reserve(calling.parameters.size());
    for (const auto e : calling.parameters)
      params.push_back(genExpression(*e, f));

    const auto d = std::get<AST::IdentifierExpression>(*calling.func);
    return f.builder.CreateCall(module.getFunction(d.ident), params);
  }
  Value* genSubscript(const AST::SubscriptExpression &, FunctionBuilder& ) const noexcept { assert(false); }
  Value* genIdentifier(const AST::IdentifierExpression & identifier, FunctionBuilder& f) const noexcept {
    Value* v;
    Type* t;
    if (f.locals.contains(identifier.ident)) {
      v = f.locals[identifier.ident];
      t = static_cast<AllocaInst*>(v)->getAllocatedType();
    }
    else {
      v = module.getGlobalVariable(identifier.ident); assert(v != nullptr);
      t = static_cast<GlobalVariable*>(v)->getValueType();
    }

    return f.builder.CreateLoad(t, v);
  }
  Value* genLiteral(const AST::LiteralExpression& literal) noexcept {
    if (literal.type->isIntegral())
      return ConstantInt::get(getType(literal.type), literal.getUint(), not literal.type->isUnsignedIntegral());

    if (literal.type->isFloating()) {
      return ConstantFP::get(
        getType(literal.type),
        literal.type == &AST::f64_type ? literal.getDouble() : literal.getFloat()); //ConstantFP::get only accepts double for some reason, so I have to do this weird hackery
    }

    if (literal.type == &AST::bool_type || literal.type == &AST::char_type)
      return ConstantInt::get(i8, literal.getUint());

    if (literal.type == &AST::string_type)
      return ConstantDataArray::getString(context, literal.getString());

    assert(false && "Invalid / Unimplemented literal expression in codegen");
  }
  Value* genExpression(const AST::Expression& expr, FunctionBuilder& f) noexcept {
    return utils_match(expr,
      utils_callon(const AST::UnaryExpression&, genUnary, f),
      utils_callon(const AST::BinaryExpression&, genBinary, f),
      utils_callon(const AST::CallingExpression&, genCalling, f),
      utils_callon(const AST::SubscriptExpression&, genSubscript, f),
      utils_callon(const AST::IdentifierExpression&, genIdentifier, f),
      utils_callon(const AST::LiteralExpression&, genLiteral)
      );
  }
  Constant* genConstant(const AST::Expression& expr) noexcept {
    if (std::holds_alternative<AST::LiteralExpression>(expr))
      return cast<Constant>(genLiteral(std::get<AST::LiteralExpression>(expr)));

    if (std::holds_alternative<AST::IdentifierExpression>(expr))
      return module.getGlobalVariable(std::get<AST::IdentifierExpression>(expr).ident, true);

    assert(false && "Non-constant expression used to initialize global variable");
  }


  bool genVarDeclaration(const AST::VarDeclaration& declaration, FunctionBuilder& f) {
    IRBuilder<> func_entry(&f.func->getEntryBlock());
    AllocaInst* i = func_entry.CreateAlloca(getType(declaration.type.type), nullptr);
    f.locals.emplace(declaration.ident, i);
    if (declaration.expr) f.builder.CreateStore(genExpression(*declaration.expr, f), i);
    return false;
  }
  bool genIfStatement(const AST::IfStatement& ifstmt, FunctionBuilder& f) {
    IRBuilder<>& builder = f.builder;
    Value* condition_val = genExpression(*ifstmt.condition, f);
    condition_val = builder.CreateZExtOrTrunc(condition_val, i1);

    BasicBlock* thenBB = BasicBlock::Create(context); f.func->insert(f.func->end(), thenBB);
    BasicBlock* mergeBB = BasicBlock::Create(context);
    BasicBlock* elseBB = ifstmt.false_branch ? BasicBlock::Create(context) : mergeBB;

    builder.CreateCondBr(condition_val, thenBB, elseBB);

    builder.SetInsertPoint(thenBB);

    if (!genStatement(*ifstmt.true_branch, f))
      builder.CreateBr(mergeBB);

    f.func->insert(f.func->end(), elseBB);
    builder.SetInsertPoint(elseBB);
    if (ifstmt.false_branch) {
      if (!genStatement(*ifstmt.false_branch, f))
        builder.CreateBr(mergeBB);

      f.func->insert(f.func->end(), mergeBB);
      builder.SetInsertPoint(mergeBB);
    }

    return false;
  }
  bool genForLoop(const AST::ForLoop& , FunctionBuilder& ) const { assert(false); }
  bool genWhileLoop(const AST::WhileLoop& , FunctionBuilder& ) const { assert(false); }
  bool genScoped(const AST::ScopedStatement& scoped, FunctionBuilder& f) {
    auto curr = scoped.scope_body.begin();
    const auto end = scoped.scope_body.end();
    bool jumps_at_end{false};
    while (curr != end) {
      jumps_at_end = genStatement(**curr, f);
      ++curr;
    }
    return jumps_at_end;
  }
  bool genReturn(const AST::ReturnStatement& ret, FunctionBuilder& f) {
    if (ret.return_value) {
      Value* retval = genExpression(*ret.return_value, f);
      f.builder.CreateStore(retval, f.locals[FunctionBuilder::retval_location_name]);
    }
    f.builder.CreateBr(f.return_block);
    return true;
  }
  bool genExpressionStatement(const AST::ExpressionStatement& expr_stmt, FunctionBuilder& f) { if (expr_stmt.expr) genExpression(*expr_stmt.expr, f); return false; }
  bool genStatement(const AST::Statement& stmt, FunctionBuilder& f) {
    return utils_match(stmt,
      utils_callon(const AST::VarDeclaration&, genVarDeclaration, f),
      utils_callon(const AST::IfStatement&, genIfStatement, f),
      utils_callon(const AST::ForLoop&, genForLoop, f),
      utils_callon(const AST::WhileLoop&, genWhileLoop, f),
      utils_callon(const AST::ScopedStatement&, genScoped, f),
      utils_callon(const AST::ReturnStatement&, genReturn, f),
      utils_callon(const AST::ExpressionStatement&, genExpressionStatement, f)
    );
  }

  void createFile(const std::filesystem::path& obj_path, const CodeGenFileType filetype) {
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    static const auto target_triple = sys::getDefaultTargetTriple();
    module.setTargetTriple(target_triple);
    std::string err;
    const auto target = TargetRegistry::lookupTarget(module.getTargetTriple(), err);
    if (!target) {
      errs() << err;
      assert(false);
    }

    static constexpr auto CPU = "generic";
    static constexpr auto Features = "";
    static const TargetOptions opt;
    auto target_machine = target->createTargetMachine(
      target_triple, CPU, Features, opt, Reloc::PIC_);
    module.setDataLayout(target_machine->createDataLayout());

    std::error_code EC;
    raw_fd_ostream dest(obj_path.string(), EC, sys::fs::OF_None);
    if (EC) {
      errs() << "Could not open file: " << obj_path.string() << " | " << EC.message() << '\n';
      assert(false);
    }

    legacy::PassManager pass;
    if (target_machine->addPassesToEmitFile(pass, dest, nullptr, filetype)) {
      errs() << "Target machine can't emit a file of this type";
      assert(false);
    }

    pass.run(module);
    dest.flush();
    delete target_machine;
  }

  void printModule(raw_ostream& out = outs()) const { module.print(out, nullptr); }

  virtual std::filesystem::path createASMFile (const std::filesystem::path &file) override {
    static const std::filesystem::path asm_folder = Settings::getBuildLocation() + "asm/";
    std::filesystem::path asm_path = asm_folder;
    asm_path.append(file.string());
    std::filesystem::create_directories(asm_path.parent_path());
#ifdef _WIN32
    asm_path.replace_extension(".asm");
#else
    asm_path.replace_extension(".s");
#endif

    createFile(asm_path, CodeGenFileType::AssemblyFile);
    return asm_path;
  }

  virtual std::filesystem::path createIRFile (const std::filesystem::path &file) override {
    static const std::filesystem::path ir_path = Settings::getBuildLocation() + "llvm_ir/";
    std::filesystem::path file_path = ir_path;
    file_path.append(file.string());
    std::filesystem::create_directories(file_path.parent_path());
    file_path.replace_extension(".ll");

    std::error_code EC;
    raw_fd_ostream dest(file_path.string(), EC, sys::fs::OF_None);
    if (EC) {
      errs() << "Could not open file: " << file_path.string() << " | " << EC.message() << '\n';
      std::quick_exit(1);
    }

    printModule(dest);
    return file_path;
  }

  virtual std::filesystem::path createObjectFile(const std::filesystem::path &file) override {
    static const std::filesystem::path object_folder = Settings::getBuildLocation() + "obj/";
    std::filesystem::path obj_path = object_folder;
    obj_path.append(file.string());
    std::filesystem::create_directories(obj_path.parent_path());
#ifdef _WIN32
    obj_path.replace_extension(".obj");
#else
    obj_path.replace_extension(".o");
#endif

    createFile(obj_path, CodeGenFileType::ObjectFile);
    return obj_path;
  }
};
}

std::unique_ptr<Backend> ToLLVM::codegen(const PeepMIR::PeepTU& vtu, const std::filesystem::path &file) {
  auto ptr = std::make_unique<TU>(file.string());
  ptr->lowerToLLVM(vtu);
  return ptr;
}
*/