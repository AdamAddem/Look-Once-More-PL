#include "ast_validation.hpp"
#include "../grammar/expressions.hpp"
#include "../parsing/secondparse.hpp"
#include "symbol_table.hpp"
#include <stdexcept>
#include <variant>
using namespace Parser;

/* Expressions */

TypeDetails validateLiteralExpression(SymbolTable &table,
                                      const LiteralExpression *literal) {}

TypeDetails
validateIdentifierExpression(SymbolTable &table,
                             const IdentifierExpression *identifier) {}

TypeDetails validateSubscriptExpression(SymbolTable &table,
                                        const SubscriptExpression *subscript) {}

TypeDetails validateCallingExpression(SymbolTable &table,
                                      const CallingExpression *calling) {}

TypeDetails validateBinaryExpression(SymbolTable &table,
                                     const BinaryExpression *binary) {}

TypeDetails validateUnaryExpression(SymbolTable &table,
                                    const UnaryExpression *unary) {}

TypeDetails validateExpression(SymbolTable &table,
                               const Expression *expression) {
  if (auto e = std::get_if<UnaryExpression>(&expression->value))
    return validateUnaryExpression(table, e);
  if (auto e = std::get_if<BinaryExpression>(&expression->value))
    return validateBinaryExpression(table, e);
  if (auto e = std::get_if<CallingExpression>(&expression->value))
    return validateCallingExpression(table, e);
  if (auto e = std::get_if<SubscriptExpression>(&expression->value))
    return validateSubscriptExpression(table, e);
  if (auto e = std::get_if<IdentifierExpression>(&expression->value))
    return validateIdentifierExpression(table, e);
  if (auto e = std::get_if<LiteralExpression>(&expression->value))
    return validateLiteralExpression(table, e);

  throw std::runtime_error(
      "Impossible expression in validateExpression function");
}

/* Expressions */

/* Statements */
void validateStatement(SymbolTable &table, const Statement *statement);

void validateExpressionStatement(
    SymbolTable &table, const ExpressionStatement *expression_statement) {
  if (expression_statement->expr)
    validateExpression(table, expression_statement->expr);
}

void validateReturnStatement(SymbolTable &table,
                             const ReturnStatement *return_statement) {
  // add return value validation
  if (return_statement->return_value)
    validateExpression(table, return_statement->return_value);
}

void validateScopedStatement(SymbolTable &table,
                             const ScopedStatement *scoped) {
  for (auto s : scoped->scope_body)
    validateStatement(table, s);
}

void validateWhileLoop(SymbolTable &table, const WhileLoop *while_loop) {
  if (!validateExpression(table, while_loop->condition).arithmetic)
    throw std::runtime_error("While Loop condition non-convertible to boolean");

  validateStatement(table, while_loop->loop_body);
}

void validateForLoop(SymbolTable &table, const ForLoop *for_loop) {
  validateStatement(table, for_loop->var_statement);

  if (for_loop->condition &&
      !validateExpression(table, for_loop->condition).arithmetic)
    throw std::runtime_error("For Loop condition non-convertible to boolean");

  if (for_loop->iteration)
    validateExpression(table, for_loop->iteration);

  validateStatement(table, for_loop->loop_body);
}

void validateIfStatement(SymbolTable &table, const IfStatement *if_statement) {
  if (!validateExpression(table, if_statement->condition).arithmetic)
    throw std::runtime_error(
        "If statement condition non-convertible to boolean");

  validateStatement(table, if_statement->true_branch);
  if (if_statement->false_branch)
    validateStatement(table, if_statement->false_branch);
}

void validateVarDeclaration(SymbolTable &table,
                            const VarDeclaration *declaration) {
  // some sort of type validation here
  if (table.isSymbolInCurrentScope(declaration->ident))
    throw std::runtime_error(
        "Redefinition of symbol name in variable declaration");

  validateExpression(table, declaration->expr);
  table.addLocalVariable(declaration->ident, declaration->type);
}

void validateStatement(SymbolTable &table, const Statement *statement) {
  if (auto s = std::get_if<VarDeclaration>(&statement->value))
    validateVarDeclaration(table, s);

  else if (auto s = std::get_if<IfStatement>(&statement->value))
    validateIfStatement(table, s);

  else if (auto s = std::get_if<ForLoop>(&statement->value))
    validateForLoop(table, s);

  else if (auto s = std::get_if<WhileLoop>(&statement->value))
    validateWhileLoop(table, s);

  else if (auto s = std::get_if<ScopedStatement>(&statement->value))
    validateScopedStatement(table, s);

  else if (auto s = std::get_if<ReturnStatement>(&statement->value))

    validateReturnStatement(table, s);
  else if (auto s = std::get_if<ExpressionStatement>(&statement->value))
    validateExpressionStatement(table, s);
}

/* Statements */

void validateFunction(SymbolTable &table, ParsedFunction &func) {
  table.expectedReturnType = func.return_type;
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

void validateGlobals(SymbolTable &table, ParsedGlobals &globals) {

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
