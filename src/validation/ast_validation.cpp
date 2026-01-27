#include "ast_validation.hpp"
#include "../grammar/expressions.hpp"
#include "../parsing/secondparse.hpp"
#include "symbol_table.hpp"
#include <stdexcept>
#include <variant>

#include "lexing/lex.hpp"
using namespace Parser;

/* Expressions */

struct ExpressionResult {
  StrictType type;
  bool is_mutable;
};

static ExpressionResult validateExpression(SymbolTable &table, const Expression *expression);

static ExpressionResult validateLiteralExpression(SymbolTable &table, const LiteralExpression *literal) {}

static ExpressionResult validateIdentifierExpression(SymbolTable &table, const IdentifierExpression *identifier) {}

static ExpressionResult validateSubscriptExpression(SymbolTable &table, const SubscriptExpression *subscript) {}

static ExpressionResult validateCallingExpression(SymbolTable &table, const CallingExpression *calling) {
  const auto expr_details = validateExpression(table, calling->func);
  const auto type_details = table.detailsOfType(expr_details.type);

  if (!type_details.callable)
    throw std::runtime_error("Call operator used on non-callable");

  const auto identexpr = std::get_if<IdentifierExpression>(&calling->func->value);
  if (identexpr == nullptr)
    throw std::runtime_error("Call operator used on non-identifier");

  std::vector<Type> provided_params;
  for (const auto param : calling->parameters)
    provided_params.emplace_back(validateExpression(table, param).type);

  return {table.returnTypeOfCall(identexpr->ident, provided_params), true};
}

static ExpressionResult validateBinaryExpression(SymbolTable &table, const BinaryExpression *binary) {
  const auto lexpr_result = validateExpression(table, binary->expr_left);
  const auto ltype_details = table.detailsOfType(lexpr_result.type);
  const auto rexpr_result = validateExpression(table, binary->expr_right);
  const auto rtype_details = table.detailsOfType(rexpr_result.type);

  if(!ltype_details.arithmetic || !rtype_details.arithmetic) {
     if(binary->opr != Operator::ASSIGN)
       throw std::runtime_error("Non-assignment operator used on on-arithmetic type(s)");

    if(!lexpr_result.is_mutable)
      throw std::runtime_error("Assignment operator used on non-mutable left expression");

    if(!convertibleFromTo(rexpr_result.type, lexpr_result.type)) 
      throw std::runtime_error("Right expression not convertible to left expression in assignment");

    return lexpr_result;
  }

  switch(binary->opr) {
    case Operator::ADD:
    case Operator::SUBTRACT:
    case Operator::MULTIPLY:
    case Operator::DIVIDE:
    case Operator::POWER:
    case Operator::MODULUS:
      break;

    case Operator::ADD_ASSIGN:
    case Operator::SUB_ASSIGN:
    case Operator::MULT_ASSIGN:
    case Operator::DIV_ASSIGN:
    case Operator::POW_ASSIGN:
    case Operator::MOD_ASSIGN:
      break;
  }
  
}

static ExpressionResult validateUnaryExpression(SymbolTable &table, const UnaryExpression *unary) {
  auto expr_details = validateExpression(table, unary->expr);
  const auto type_details = table.detailsOfType(expr_details.type);
  if (!type_details.arithmetic)
    throw std::runtime_error(
        "Unary operator used on non-arithmetic expresison");

  if (unary->opr == Operator::UNARY_MINUS) {
    expr_details.is_mutable = false;
    return expr_details;
  }

  if (!expr_details.is_mutable)
    throw std::runtime_error(
        "Pre/Postfix operator used on non-mutable expression");

  if (unary->opr == Operator::POST_INCREMENT ||
      unary->opr == Operator::POST_DECREMENT)
    expr_details.is_mutable = false;

  return expr_details;
}

static ExpressionResult validateExpression(SymbolTable &table, const Expression *expression) {
  if (const auto e = std::get_if<UnaryExpression>(&expression->value))
    return validateUnaryExpression(table, e);
  if (const auto e = std::get_if<BinaryExpression>(&expression->value))
    return validateBinaryExpression(table, e);
  if (const auto e = std::get_if<CallingExpression>(&expression->value))
    return validateCallingExpression(table, e);
  if (const auto e = std::get_if<SubscriptExpression>(&expression->value))
    return validateSubscriptExpression(table, e);
  if (const auto e = std::get_if<IdentifierExpression>(&expression->value))
    return validateIdentifierExpression(table, e);
  if (const auto e = std::get_if<LiteralExpression>(&expression->value))
    return validateLiteralExpression(table, e);

  throw std::runtime_error(
      "Impossible expression in validateExpression function");
}

/* Expressions */

/* Statements */

static void validateStatement(SymbolTable &table, const Statement *statement);

static void validateExpressionStatement(SymbolTable &table, const ExpressionStatement *expression_statement) {
  if (expression_statement->expr)
    validateExpression(table, expression_statement->expr);
}

static void validateReturnStatement(SymbolTable &table, const ReturnStatement *return_statement) {
  // add return value validation
  if (return_statement->return_value)
    validateExpression(table, return_statement->return_value);
}

static void validateScopedStatement(SymbolTable &table, const ScopedStatement *scoped) {
  for (const auto s : scoped->scope_body)
    validateStatement(table, s);
}

static void validateWhileLoop(SymbolTable &table, const WhileLoop *while_loop) {
  const auto expr_details = validateExpression(table, while_loop->condition);
  const auto type_details = table.detailsOfType(expr_details.type);
  if (!type_details.arithmetic)
    throw std::runtime_error("While Loop condition non-convertible to boolean");

  validateStatement(table, while_loop->loop_body);
}

static void validateForLoop(SymbolTable &table, const ForLoop *for_loop) {
  validateStatement(table, for_loop->var_statement);

  if (for_loop->condition) {
    const auto expr_details = validateExpression(table, for_loop->condition);
    const auto type_details = table.detailsOfType(expr_details.type);

    if (!type_details.arithmetic)
      throw std::runtime_error("For Loop condition non-convertible to boolean");
  }

  if (for_loop->iteration)
    validateExpression(table, for_loop->iteration);

  validateStatement(table, for_loop->loop_body);
}

static void validateIfStatement(SymbolTable &table, const IfStatement *if_statement) {
  const auto expr_details = validateExpression(table, if_statement->condition);
  const auto type_details = table.detailsOfType(expr_details.type);
  if (!type_details.arithmetic)
    throw std::runtime_error(
        "If statement condition non-convertible to boolean");

  validateStatement(table, if_statement->true_branch);
  if (if_statement->false_branch)
    validateStatement(table, if_statement->false_branch);
}

static void validateVarDeclaration(SymbolTable &table, const VarDeclaration *declaration) {
  // some sort of type validation here
  if (table.isSymbolInCurrentScope(declaration->ident))
    throw std::runtime_error(
        "Redefinition of symbol name in variable declaration");

  validateExpression(table, declaration->expr);
  table.addLocalVariable(declaration->ident, declaration->type);
}

static void validateStatement(SymbolTable &table, const Statement *statement) {
  if (const auto s = std::get_if<VarDeclaration>(&statement->value))
    validateVarDeclaration(table, s);

  else if (const auto s = std::get_if<IfStatement>(&statement->value))
    validateIfStatement(table, s);

  else if (const auto s = std::get_if<ForLoop>(&statement->value))
    validateForLoop(table, s);

  else if (const auto s = std::get_if<WhileLoop>(&statement->value))
    validateWhileLoop(table, s);

  else if (const auto s = std::get_if<ScopedStatement>(&statement->value))
    validateScopedStatement(table, s);

  else if (const auto s = std::get_if<ReturnStatement>(&statement->value))
    validateReturnStatement(table, s);
  
  else if (const auto s = std::get_if<ExpressionStatement>(&statement->value))
    validateExpressionStatement(table, s);
}

/* Statements */


static void validateFunction(SymbolTable &table, ParsedFunction &func) {

  //add return type functionality
  std::vector<Type> param_types;
  table.enterScope();
  for (auto &decl : func.parameter_list) {
    validateVarDeclaration(table, &decl);
    param_types.emplace_back(decl.type);
  }
  table.addFunction(func.name, func.return_type, std::move(param_types));

  for (const auto statement : func.function_body)
    validateStatement(table, statement);

  table.leaveScope();
}

static void validateGlobals(SymbolTable &table, const ParsedGlobals &globals) {
  for (auto &decl : globals.declarations)
    table.addGlobalVariable(decl.ident, decl.type);

  table.enterScope();
  for (const auto statement : globals.global_init_body)
    validateStatement(table, statement);
  table.leaveScope();
}

void Validation::validateTU(ParsedTranslationUnit &&unverified_tu) {

  SymbolTable table;
  ParsedTranslationUnit ptu = std::move(unverified_tu);

  validateGlobals(table, ptu.global);

  for (auto &f : ptu.functions)
    validateFunction(table, f);

  ptu.print();
}
