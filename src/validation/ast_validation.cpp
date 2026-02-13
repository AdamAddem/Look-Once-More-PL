#include "ast_validation.hpp"
#include <stdexcept>
#include <variant>
#include "symbol_table.hpp"
#include "../grammar/expressions.hpp"
#include "../parsing/secondparse.hpp"

#include "debug_flags.hpp"
using namespace Parser;

/* Expressions */

struct ExpressionReturn {
  StrictType type;
  bool is_mutable;
};

static ExpressionReturn validateExpression(
  [[maybe_unused]] SymbolTable &table,
  [[maybe_unused]] const Expression *expression);

static ExpressionReturn validateLiteralExpression(
  [[maybe_unused]] SymbolTable &table,
  [[maybe_unused]] const LiteralExpression *literal) { return {StrictType("temp"), false}; }

static ExpressionReturn validateIdentifierExpression(
    [[maybe_unused]] SymbolTable &table,
    [[maybe_unused]] const IdentifierExpression *identifier) { return {StrictType("temp"), false}; }

static ExpressionReturn validateSubscriptExpression(
    [[maybe_unused]] SymbolTable &table,
    [[maybe_unused]] const SubscriptExpression *subscript) { return {StrictType("temp"), false}; }

static ExpressionReturn validateCallingExpression(
  [[maybe_unused]] SymbolTable &table,
  const CallingExpression *calling) {
  auto expr_details = validateExpression(table, calling->func);
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

ExpressionReturn validateBinaryExpression([[maybe_unused]] SymbolTable &table,
                         [[maybe_unused]] const BinaryExpression *binary) {
  // auto expr_details = validateExpression(table, binary->expr_left);
  //  auto type_details = table.detailsOfType(expr_details.type);
  return {StrictType("temp"), false};
}

static ExpressionReturn validateUnaryExpression(SymbolTable &table, const UnaryExpression *unary) {
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

static ExpressionReturn validateExpression(SymbolTable &table, const Expression *expression) {
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
  if (const auto var_stmt = std::get_if<VarDeclaration>(&statement->value))
    validateVarDeclaration(table, var_stmt);

  else if (const auto if_stmt = std::get_if<IfStatement>(&statement->value))
    validateIfStatement(table, if_stmt);

  else if (const auto for_stmt = std::get_if<ForLoop>(&statement->value))
    validateForLoop(table, for_stmt);

  else if (const auto while_stmt = std::get_if<WhileLoop>(&statement->value))
    validateWhileLoop(table, while_stmt);

  else if (const auto scoped_stmt = std::get_if<ScopedStatement>(&statement->value))
    validateScopedStatement(table, scoped_stmt);

  else if (const auto return_stmt = std::get_if<ReturnStatement>(&statement->value))
    validateReturnStatement(table, return_stmt);

  else if (const auto expr_stmt = std::get_if<ExpressionStatement>(&statement->value))
    validateExpressionStatement(table, expr_stmt);
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

  throw std::runtime_error("validation not complete yet");

  SymbolTable table;
  ParsedTranslationUnit ptu = std::move(unverified_tu);

  validateGlobals(table, ptu.global);

  for (auto &f : ptu.functions)
    validateFunction(table, f);

  if constexpr (lom_debug::stage_to_halt == lom_debug::halt_flags::VALIDATION) {
    ptu.print();
    std::terminate();
  }

}
