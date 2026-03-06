#include "tollvm.hpp"

#include "ast/expressions.hpp"
#include "ast/statements.hpp"
#include "error.hpp"
#include "settings.hpp"
#include "utilities/variant_overload.hpp"
#include "validation/ast_validation.hpp"

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

    if (verifyFunction(*func, &errs()))
      throw BackendError("Failed to verify Function!", func->getName().str(), 0);
  }

};

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
  explicit TU(const std::string& filename) : module(filename, context) {
    i1 = Type::getInt1Ty(context); i1_0 = ConstantInt::get(i1, 0); i1_1 = ConstantInt::get(i1, 1);
    i8 = Type::getInt8Ty(context); i8_0 = ConstantInt::get(i8, 0); i8_1 = ConstantInt::get(i8, 1);
    i16 = Type::getInt16Ty(context); i16_0 = ConstantInt::get(i16, 0); i16_1 = ConstantInt::get(i16, 1);
    i32 = Type::getInt32Ty(context); i32_0 = ConstantInt::get(i32, 0); i32_1 = ConstantInt::get(i32, 1);
    i64 = Type::getInt64Ty(context); i64_0 = ConstantInt::get(i64, 0); i64_1 = ConstantInt::get(i64, 1);

    f32 = Type::getFloatTy(context); f32_0 = ConstantFP::get(f32, 0); f32_1 = ConstantFP::get(f32, 1);
    f64 = Type::getDoubleTy(context); f64_0 = ConstantFP::get(f64, 0); f64_1 = ConstantFP::get(f64, 1);
    devoid = Type::getVoidTy(context);
  }

  [[nodiscard]] Type* getType(const AST::Type& t) const {
    assert(!t.isVariant());
    assert(t.subtype == nullptr);
    const auto& n = t.getTypename();
    assert(n != "string");

    if (n == "i32" || n == "u32")
      return i32;
    if (n.empty())
      return devoid;
    if (n == "i64" || n == "u64")
      return i64;

    if (n == "f32")
      return f32;
    if (n == "f64")
      return f64;

    if (n == "bool" || n == "char" || n == "i8" || n == "u8")
      return i8;
    if (n == "i16" || n == "u16")
      return i16;

    assert(false && "invalid type idfk");
  }
  void addGlobal(const AST::VarDeclaration& var) {
    module.insertGlobalVariable(
      new GlobalVariable(
       getType(var.type),
       !var.type.is_mutable,
       GlobalValue::LinkageTypes::ExternalLinkage,
       nullptr,
       var.ident
       )
     );
  }
  [[nodiscard]] FunctionBuilder createFunction(const Validation::ValidatedFunction& func) {
    std::vector<Type*> arg_types;
    arg_types.reserve(func.parameter_list.size());
    for (const auto& v : func.parameter_list)
      arg_types.push_back(getType(v.type));

    //hack
    Type* ret_type = func.return_type.getTypename() != "bool" ? getType(func.return_type) : i1;
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
  void lowerToLLVM(const Validation::ValidatedTU& vtu) {
    for (const auto& v : vtu.globals)
      addGlobal(v);

    for (const auto& func : vtu.functions) {
      auto function_builder = createFunction(func);
      for (const auto s : func.function_body)
        genStatement(s, function_builder);

      function_builder.finalizeFunction();
    }

  }

  Value* genUnary(const AST::UnaryExpression& unary, FunctionBuilder& f) const noexcept {
    auto* load = cast<LoadInst>(genIdentifier(std::get<AST::IdentifierExpression>(*unary.expr), f));
    const bool is_float = load->getAccessType()->isFloatingPointTy();
    Value* result;
    IRBuilder<>& builder = f.builder;
    bool dostore;
    switch (unary.opr) {
    case AST::Operator::PRE_INCREMENT:
      result = is_float ? builder.CreateFAdd(load, f32_1) : builder.CreateAdd(load, i32_1);
      dostore = true;
      break;
    case AST::Operator::PRE_DECREMENT:
      result = is_float ? builder.CreateFSub(load, f32_1) : builder.CreateSub(load, i32_1);
      dostore = true;
      break;
    case AST::Operator::UNARY_MINUS:
      result = is_float ? builder.CreateFNeg(load) : builder.CreateNeg(load);
      dostore = false;
      break;

    case AST::Operator::ADDRESS_OF:
    case AST::Operator::NOT:
    case AST::Operator::POST_INCREMENT:
    case AST::Operator::POST_DECREMENT:
    case AST::Operator::CAST:
    case AST::Operator::CAST_IF:
    case AST::Operator::UNSAFE_CAST:
    default:
      assert(false && "Unimplemented expression in codegen srry");
    }

    if (dostore)
      builder.CreateStore(result, load->getPointerOperand() );

    return result;
  }
  Value* genBinary(const AST::BinaryExpression& binary,  FunctionBuilder& f) noexcept {
    Value* left = genExpression(*binary.expr_left, f);
    Value* right = genExpression(*binary.expr_right, f);
    Value* result;
    const bool is_float = left->getType()->isFloatingPointTy();
    IRBuilder<>& builder = f.builder;

    switch (binary.opr) {
    case AST::Operator::ADD:
      result = is_float ? builder.CreateFAdd(left, right) : builder.CreateAdd(left, right);
      break;
    case AST::Operator::SUBTRACT:
      result = is_float ? builder.CreateFSub(left, right) : builder.CreateSub(left, right);
      break;
    case AST::Operator::MULTIPLY:
      result = is_float ? builder.CreateFMul(left, right) : builder.CreateMul(left, right);
      break;
    case AST::Operator::DIVIDE:
      result = is_float ? builder.CreateFDiv(left, right) : builder.CreateSDiv(left, right);
      break;
    case AST::Operator::POWER:
      assert(false && "Unimplemented exponent operator");
    case AST::Operator::MODULUS:
      result = is_float ? builder.CreateFRem(left, right) : builder.CreateSRem(left, right);
      break;
    case AST::Operator::ASSIGN:
      assert(isa<LoadInst>(left));
      builder.CreateStore(right, cast<LoadInst>(left)->getPointerOperand());
      result = left;
      break;
    case AST::Operator::LESS: {
      using enum CmpInst::Predicate;
      const auto predicate = is_float ? FCMP_OLT : ICMP_ULT;
      result = builder.CreateCmp(predicate, left, right);
      break;
    }
    case AST::Operator::GREATER: {
      using enum CmpInst::Predicate;
      const auto predicate = is_float ? FCMP_OGT : ICMP_UGT;
      result = builder.CreateCmp(predicate, left, right);
      break;
    }
    case AST::Operator::LESS_EQUAL: {
      using enum CmpInst::Predicate;
      const auto predicate = is_float ? FCMP_OLE : ICMP_ULE;
      result = builder.CreateCmp(predicate, left, right);
      break;
    }
    case AST::Operator::GREATER_EQUAL: {
      using enum CmpInst::Predicate;
      const auto predicate = is_float ? FCMP_OGE : ICMP_UGE;
      result = builder.CreateCmp(predicate, left, right);
      break;
    }

    case AST::Operator::AND:
    case AST::Operator::OR:
    case AST::Operator::XOR:
    case AST::Operator::BITAND:
    case AST::Operator::BITOR:
    case AST::Operator::BITXOR:
    case AST::Operator::BITNOT:
      assert(false && "Binary operator not supported");

    case AST::Operator::EQUAL: {
      using enum CmpInst::Predicate;
      const auto predicate = is_float ? FCMP_OEQ : ICMP_EQ;
      result = builder.CreateCmp(predicate, left, right);
      break;
    }

    case AST::Operator::NOT_EQUAL: {
      using enum CmpInst::Predicate;
      const auto predicate = is_float ? FCMP_ONE : ICMP_NE;
      result = builder.CreateCmp(predicate, left, right);
      break;
    }

    default:
      assert(false && "Binary operator not supported");
    }

    return result;
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
      v = module.getGlobalVariable(identifier.ident);
      t = static_cast<GlobalVariable*>(v)->getValueType();
    }

    return f.builder.CreateLoad(t, v);
  }
  Value* genLiteral(const AST::LiteralExpression& literal) noexcept {
    switch (literal.type) {
    case AST::LiteralExpression::INT:
      return ConstantInt::get(i32, std::get<int>(literal.value));
    case AST::LiteralExpression::FLOAT:
      return ConstantFP::get(f32, std::get<float>(literal.value));
    case AST::LiteralExpression::DOUBLE:
      return ConstantFP::get(f32, std::get<double>(literal.value));
    case AST::LiteralExpression::BOOL:
    case AST::LiteralExpression::CHAR:
      return ConstantInt::get(i8, std::get<int>(literal.value));
    case AST::LiteralExpression::STRING:
      return ConstantDataArray::getString(context, std::get<std::string>(literal.value));
    default:
      assert(false && "Invalid literal expression in codegen");
    }
  }
  Value* genExpression(const AST::Expression& expr, FunctionBuilder& f) {
    return utils_match(expr,
      utils_callon(const AST::UnaryExpression&, genUnary, f),
      utils_callon(const AST::BinaryExpression&, genBinary, f),
      utils_callon(const AST::CallingExpression&, genCalling, f),
      utils_callon(const AST::SubscriptExpression&, genSubscript, f),
      utils_callon(const AST::IdentifierExpression&, genIdentifier, f),
      utils_callon(const AST::LiteralExpression&, genLiteral)
      );
  }
  bool genVarDeclaration(const AST::VarDeclaration& declaration, FunctionBuilder& f) {
    IRBuilder<> func_entry(&f.func->getEntryBlock());
    AllocaInst* i = func_entry.CreateAlloca(getType(declaration.type), nullptr);
    f.locals.emplace(declaration.ident, i);
    f.builder.CreateStore(i, genExpression(*declaration.expr, f));
    return false;
  }
  bool genIfStatement(const AST::IfStatement& ifstmt, FunctionBuilder& f) {
    IRBuilder<>& builder = f.builder;
    Value* condition_val = genExpression(*ifstmt.condition, f);

    BasicBlock* thenBB = BasicBlock::Create(context); f.func->insert(f.func->end(), thenBB);
    BasicBlock* mergeBB = BasicBlock::Create(context);
    BasicBlock* elseBB = ifstmt.false_branch ? BasicBlock::Create(context) : mergeBB;

    builder.CreateCondBr(condition_val, thenBB, elseBB);

    builder.SetInsertPoint(thenBB);

    if (!genStatement(ifstmt.true_branch, f))
      builder.CreateBr(mergeBB);

    f.func->insert(f.func->end(), elseBB);
    builder.SetInsertPoint(elseBB);
    if (ifstmt.false_branch) {
      if (!genStatement(ifstmt.false_branch, f))
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
    while (true) {
      const bool jumps_at_end = genStatement(*curr, f);
      ++curr;
      if (curr == end)
        return jumps_at_end;
    }
  }
  bool genReturn(const AST::ReturnStatement& ret, FunctionBuilder& f) {
    if (ret.return_value) {
      Value* retval = genExpression(*ret.return_value, f);
      f.builder.CreateStore(retval, f.locals[FunctionBuilder::retval_location_name]);
    }
    f.builder.CreateBr(f.return_block);
    return true;
  }
  bool genExpressionStatement(const AST::ExpressionStatement& expr_stmt, FunctionBuilder& f) { genExpression(*expr_stmt.expr, f); return false; }
  bool genStatement(const AST::Statement* stmt, FunctionBuilder& f) {
    return utils_match(*stmt,
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
    raw_fd_ostream dest(obj_path.c_str(), EC, sys::fs::OF_None);
    if (EC) {
      errs() << "Could not open file: " << obj_path << " | " << EC.message() << '\n';
      assert(false);
    }

    legacy::PassManager pass;
    if (target_machine->addPassesToEmitFile(pass, dest, nullptr, filetype)) {
      errs() << "Target machine can't emit a file of this type";
      assert(false);
    }

    pass.run(module);
    dest.flush();
  }

  void printModule(raw_ostream& out = outs()) const { module.print(out, nullptr); }

  virtual std::filesystem::path createASMFile (const std::string &filename) override {
    static const std::filesystem::path asm_folder = Settings::getBuildLocation() + "asm/";
    std::filesystem::create_directories(asm_folder);
    std::filesystem::path asm_path = asm_folder;
    asm_path.append(filename);

#ifdef _WIN32
    asm_path.replace_extension(".asm");
#else
    asm_path.replace_extension(".s");
#endif

    createFile(asm_path, CodeGenFileType::AssemblyFile);
    return asm_path;
  }

  virtual std::filesystem::path createIRFile (const std::string &filename) override {
    static const std::filesystem::path ir_path = Settings::getBuildLocation() + "llvm_ir/";
    std::filesystem::create_directories(ir_path);

    std::filesystem::path file_path = ir_path;
    file_path.append(filename);
    file_path.replace_extension(".ll");

    std::error_code EC;
    raw_fd_ostream dest(file_path.c_str(), EC, sys::fs::OF_None);
    if (EC) {
      errs() << "Could not open file: " << file_path << " | " << EC.message() << '\n';
      std::quick_exit(1);
    }

    printModule(dest);
    return file_path;
  }

  virtual std::filesystem::path createObjectFile(const std::string &filename) override {
    static const std::filesystem::path object_folder = Settings::getBuildLocation() + "obj/";
    std::filesystem::create_directories(object_folder);

    std::filesystem::path obj_path = object_folder;
    obj_path.append(filename);
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


auto ToLLVM::codegen(const Validation::ValidatedTU& vtu, const std::string &filename) -> std::unique_ptr<Backend> {
  std::unique_ptr<TU> ptr(new TU(filename));
  ptr->lowerToLLVM(vtu);
  return ptr;
}