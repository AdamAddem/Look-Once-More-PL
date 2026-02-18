#include "ast_validation.hpp"
#include "../grammar/expressions.hpp"
#include "../parsing/secondparse.hpp"
#include "symbol_table.hpp"
#include <iostream>
#include <stdexcept>
#include <variant>

#include "debug_flags.hpp"
using namespace Parser;

/* Expressions */

struct ExpressionReturn {
  Types type;
  bool is_mutable;
};

static ExpressionReturn
validateExpression([[maybe_unused]] SymbolTable &table,
                   [[maybe_unused]] const Expression *expression);

static ExpressionReturn
validateLiteralExpression(const LiteralExpression *literal) {
  switch (literal->type) {
  case LiteralExpression::INT:
    return {.type = {"int"}, .is_mutable = false};
  case LiteralExpression::FLOAT:
    return {.type = {"float"}, .is_mutable = false};
  case LiteralExpression::DOUBLE:
    return {.type = {"double"}, .is_mutable = false};
  case LiteralExpression::BOOL:
    return {.type = {"bool"}, .is_mutable = false};
  case LiteralExpression::CHAR:
    return {.type = {"char"}, .is_mutable = false};
  case LiteralExpression::STRING:
    return {.type = {"string"}, .is_mutable = false};

  default:
    throw std::runtime_error("Unrecognized literal expression type");
  }
}

[[maybe_unused]] static ExpressionReturn validateIdentifierExpression(
    [[maybe_unused]] const SymbolTable &table,
    [[maybe_unused]] const IdentifierExpression *identifier) {
  if (!table.containsVariable(identifier->ident))
    throw std::runtime_error("Undeclared identifier");

  const auto &v = table.closestVariable(identifier->ident);
  if (v.is_stolen)
    throw std::runtime_error("Stolen variable used");

  return {.type = v.type, .is_mutable = v.is_mutable};
}

[[maybe_unused]] static ExpressionReturn validateSubscriptExpression(
    [[maybe_unused]] SymbolTable &table,
    [[maybe_unused]] const SubscriptExpression *subscript) {
  throw std::runtime_error("Subscript expression not implemented yet");
}

static ExpressionReturn
validateCallingExpression([[maybe_unused]] SymbolTable &table,
                          const CallingExpression *calling) {
  const auto [type, is_mutable] = validateExpression(table, calling->func);
  if (type.size() != 1)
    throw std::runtime_error("Call operator used on variant type");

  const auto [arithmetic, callable, array] = table.detailsOfType(type.front());

  if (!callable)
    throw std::runtime_error("Call operator used on non-callable");

  const auto identexpr =
      std::get_if<IdentifierExpression>(&calling->func->value);
  if (identexpr == nullptr)
    throw std::runtime_error("Call operator used on non-identifier");

  std::vector<Types> provided_params;
  for (const auto param : calling->parameters)
    provided_params.emplace_back(validateExpression(table, param).type);

  return {{table.returnTypeOfCall(identexpr->ident, provided_params)}, true};
}

// current implementation returns the left expressions type for arithmetic
// operations fix that shit lol
ExpressionReturn
validateBinaryExpression([[maybe_unused]] SymbolTable &table,
                         [[maybe_unused]] const BinaryExpression *binary) {
  const auto [left_type, left_is_mutable] =
      validateExpression(table, binary->expr_left);
  const auto [right_type, right_is_mutable] =
      validateExpression(table, binary->expr_right);
  if (left_type.size() != 1 || right_type.size() != 1)
    throw std::runtime_error("Binary operator used on variant type(s)");

  const auto [left_arithmetic, left_callable, left_array] =
      table.detailsOfType(left_type.front());
  const auto [right_arithmetic, right_callable, right_array] =
      table.detailsOfType(right_type.front());

  switch (binary->opr) {
  case Operator::ADD:
  case Operator::SUBTRACT:
  case Operator::MULTIPLY:
  case Operator::DIVIDE:
  case Operator::POWER:
  case Operator::MODULUS:
  case Operator::LESS:
  case Operator::GREATER:
  case Operator::LESS_EQUAL:
  case Operator::GREATER_EQUAL:
  case Operator::AND:
  case Operator::OR:
  case Operator::XOR:
    if (!left_arithmetic || !right_arithmetic)
      throw std::runtime_error(
          "Non-arithmetic expression in arithmetic binary operation");

    break;

  case Operator::BITAND:
  case Operator::BITOR:
  case Operator::BITXOR:
  case Operator::BITNOT:
    if (!left_arithmetic || !right_arithmetic)
      throw std::runtime_error(
          "Non-arithmetic expression in arithmetic binary operation");

    if (!are_types_convertible(right_type, left_type))
      throw std::runtime_error(
          "Bitwise operation on different types not allowed");

    break;

  case Operator::ASSIGN:
    if (!left_is_mutable) {
      std::cout << left_type.front() << std::endl;
      throw std::runtime_error("Left expression in assignment non-mutable");
    }

    if (left_arithmetic && right_arithmetic)
      break;

    if (!are_types_convertible(right_type, left_type))
      throw std::runtime_error("Assignment with non-convertible types");

    break;

  case Operator::EQUAL:
  case Operator::NOT_EQUAL:
    if ((left_arithmetic && right_arithmetic) || left_type == right_type)
      break;

    throw std::runtime_error(
        "Equality operator used on different non-arithmetic types");

  default:
    throw std::runtime_error(
        "Non binary operator set in binary expression. How?");
  }

  switch (binary->opr) {
  case Operator::ADD:
  case Operator::SUBTRACT:
  case Operator::MULTIPLY:
  case Operator::DIVIDE:
  case Operator::POWER:
  case Operator::MODULUS:
    return {.type = {left_type}, .is_mutable = false};
  case Operator::LESS:
  case Operator::GREATER:
  case Operator::LESS_EQUAL:
  case Operator::GREATER_EQUAL:
  case Operator::AND:
  case Operator::OR:
  case Operator::XOR:
    return {.type = {"bool"}, .is_mutable = false};

  case Operator::BITAND:
  case Operator::BITOR:
  case Operator::BITXOR:
  case Operator::BITNOT:
  case Operator::ASSIGN:
    return {.type = {left_type}, .is_mutable = false};

  case Operator::EQUAL:
  case Operator::NOT_EQUAL:
    return {.type = {"bool"}, .is_mutable = false};

  default:
    throw std::runtime_error(
        "Non binary operator set in binary expression. How?");
  }
}

static ExpressionReturn validateUnaryExpression(SymbolTable &table,
                                                const UnaryExpression *unary) {
  const auto [type, is_mutable] = validateExpression(table, unary->expr);
  if (type.size() != 1)
    throw std::runtime_error("Unary operator used on variant type");

  const auto [arithmetic, callable, array] = table.detailsOfType(type.front());
  if (!arithmetic)
    throw std::runtime_error(
        "Unary operator used on non-arithmetic expresison");

  if (unary->opr == Operator::UNARY_MINUS) {
    return {.type = type, .is_mutable = false};
  }

  if (unary->opr == Operator::NOT) {
    return {.type = {"bool"}, .is_mutable = false};
  }

  if (!is_mutable)
    throw std::runtime_error(
        "Pre/Postfix operator used on non-mutable expression");

  if (unary->opr == Operator::POST_INCREMENT ||
      unary->opr == Operator::POST_DECREMENT)
    return {.type = type, .is_mutable = false};

  return {.type = type, .is_mutable = is_mutable};
}

static ExpressionReturn validateExpression(SymbolTable &table,
                                           const Expression *expression) {
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
    return validateLiteralExpression(e);
  if (std::holds_alternative<TemporaryExpr>(expression->value))
    std::cout << "Temporary!" << std::endl;

  throw std::runtime_error(
      "Impossible expression in validateExpression function");
}

/* Expressions */

/* Statements */

static void validateStatement(SymbolTable &table, const Statement *statement);

static void
validateExpressionStatement(SymbolTable &table,
                            const ExpressionStatement *expression_statement) {
  if (expression_statement->expr)
    validateExpression(table, expression_statement->expr);
}

static void validateReturnStatement(SymbolTable &table,
                                    const ReturnStatement *return_statement) {
  if (return_statement->return_value) {
    const auto [type, is_mutable] =
        validateExpression(table, return_statement->return_value);
    if (!are_types_convertible(table.returnTypeOfCurrentScope(), type)) {
      std::cout << table.returnTypeOfCurrentScope() << std::endl;
      std::cout << type.front() << std::endl;
      throw std::runtime_error(
          "Return statement's type is not compatable with scope return type");
    }
  }
}

static void validateScopedStatement(SymbolTable &table,
                                    const ScopedStatement *scoped) {
  for (const auto s : scoped->scope_body)
    validateStatement(table, s);
}

static void validateWhileLoop(SymbolTable &table, const WhileLoop *while_loop) {
  const auto [type, is_mutable] =
      validateExpression(table, while_loop->condition);
  if (type.size() != 1)
    throw std::runtime_error("Variant in while loop condition");

  const auto [arithmetic, callable, array] = table.detailsOfType(type.front());
  if (!arithmetic)
    throw std::runtime_error("While Loop condition non-convertible to boolean");

  validateStatement(table, while_loop->loop_body);
}

static void validateForLoop(SymbolTable &table, const ForLoop *for_loop) {
  validateStatement(table, for_loop->var_statement);

  if (for_loop->condition) {
    const auto [type, is_mutable] =
        validateExpression(table, for_loop->condition);
    if (type.size() != 1)
      throw std::runtime_error("Variant used in for loop condition");

    const auto [arithmetic, callable, array] =
        table.detailsOfType(type.front());

    if (!arithmetic)
      throw std::runtime_error("For Loop condition non-convertible to boolean");
  }

  if (for_loop->iteration)
    validateExpression(table, for_loop->iteration);

  validateStatement(table, for_loop->loop_body);
}

static void validateIfStatement(SymbolTable &table,
                                const IfStatement *if_statement) {
  const auto [type, is_mutable] =
      validateExpression(table, if_statement->condition);
  if (type.size() != 1)
    throw std::runtime_error("Variant type used in if statement condition");

  const auto [arithmetic, callable, array] = table.detailsOfType(type.front());
  if (!arithmetic)
    throw std::runtime_error(
        "If statement condition non-convertible to boolean");

  validateStatement(table, if_statement->true_branch);
  if (if_statement->false_branch)
    validateStatement(table, if_statement->false_branch);
}

static void validateVarDeclaration(SymbolTable &table,
                                   const VarDeclaration *declaration) {
  // some sort of type validation here
  if (table.isSymbolInCurrentScope(declaration->ident))
    throw std::runtime_error(
        "Redefinition of symbol name in variable declaration");

  if (declaration->expr)
    validateExpression(table, declaration->expr);

  table.addLocalVariable(declaration->ident, declaration->type,
                         true); // making all variables mutable FOR NOW
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

  else if (const auto scoped_stmt =
               std::get_if<ScopedStatement>(&statement->value))
    validateScopedStatement(table, scoped_stmt);

  else if (const auto return_stmt =
               std::get_if<ReturnStatement>(&statement->value))
    validateReturnStatement(table, return_stmt);

  else if (const auto expr_stmt =
               std::get_if<ExpressionStatement>(&statement->value))
    validateExpressionStatement(table, expr_stmt);
}

/* Statements */

static void validateFunction(SymbolTable &table, ParsedFunction &func) {
  std::vector<Types> param_types;
  table.enterScope(func.return_type);
  for (auto &decl : func.parameter_list) {
    validateVarDeclaration(table, &decl);
    param_types.emplace_back(decl.type);
  }
  table.addFunction(func.name, func.return_type, std::move(param_types));

  const bool is_devoid_return = func.return_type.empty();
  bool has_return_statement = false;
  for (const auto statement : func.function_body) {
    validateStatement(table, statement);

    if (!is_devoid_return) {
      if (std::holds_alternative<ReturnStatement>(statement->value)) {
        has_return_statement = true;
      }
    }
  }
  if (!is_devoid_return && !has_return_statement)
    throw std::runtime_error(
        "Non-devoid returning function has no return statement");

  table.leaveScope();
}

static void validateGlobals(SymbolTable &table, const ParsedGlobals &globals) {
  for (auto &decl : globals.declarations)
    table.addGlobalVariable(decl.ident, decl.type,
                            true); // TEMPORARY, CHANGE IS MUTABLE

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

  if (lom_debug::output_validation) {
    ptu.print();
    std::cout << "Validation stage passed!" << std::endl;
    std::exit(0);
  }
}