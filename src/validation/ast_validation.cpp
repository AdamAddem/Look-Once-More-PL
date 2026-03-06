#include "ast_validation.hpp"
#include "ast/expressions.hpp"
#include "parsing/parse.hpp"
#include "symbol_table.hpp"
#include "utilities/variant_overload.hpp"
#include <iostream>
#include <stdexcept>
#include <variant>

#include "error.hpp"
#include "settings.hpp"

#include <cassert>
using namespace Parser;
using namespace Validation;
using namespace AST;


ValidatedFunction::~ValidatedFunction() {
  for (const auto s : function_body)
    delete s;
}

/* Expressions */
static Type validateExpression(const Expression *expression, SymbolTable& table);

static Type validateLiteralExpression(const LiteralExpression &literal) {
  switch (literal.type) {
  case LiteralExpression::INT:
    return int_literal_type;
  case LiteralExpression::FLOAT:
    return float_literal_type;
  case LiteralExpression::DOUBLE:
    return double_literal_type;
  case LiteralExpression::BOOL:
    return bool_literal_type;
  case LiteralExpression::CHAR:
    return char_literal_type;
  case LiteralExpression::STRING:
    return string_literal_type;

  default:
    throw std::runtime_error("Unrecognized literal expression type");
  }
}

static Type validateIdentifierExpression(const IdentifierExpression &identifier, const SymbolTable& table) {
  if (!table.containsSymbol(identifier.ident))
    throw ValidationError("Undeclared identifier.", identifier.ident, identifier.line_number);


  return table.closestSymbolType(identifier.ident);
}

static Type validateSubscriptExpression(const SubscriptExpression &, SymbolTable&) {assert(false);}

//this function is very iffy, revisit
static Type validateCallingExpression(const CallingExpression &calling, SymbolTable& table) {
  const auto type = validateExpression(calling.func, table);
  if (type.isVariant())
    throw ValidationError("Call operator used on variant type.", type.toString(), calling.line_number);


  //this exists for when/if a callable type exists outside of a function itself
  if (!table.containsFunction(type.getTypename())) {
    if (!table.detailsOfType(type.getTypename()).callable)
      throw ValidationError("Call operator used on non-callable.", type.toString(), calling.line_number);


    assert(false && "Calling a non-function not yet supported");
  }

  std::vector<Type> provided_params;
  for (const auto param : calling.parameters)
    provided_params.emplace_back(validateExpression(param, table));

  return table.returnTypeOfCall(type.getTypename(), provided_params);
}

// current implementation returns the left expressions type for arithmetic operations
// fix that lol
static Type validateBinaryExpression(const BinaryExpression &binary, SymbolTable& table) {
  const auto& left_type = validateExpression(binary.expr_left, table);
  const bool left_is_mutable = left_type.is_mutable;
  const auto& right_type = validateExpression(binary.expr_right, table);

  if (left_type.isVariant() || right_type.isVariant()) {
    std::string context = left_type.toString();
    context.append(" and ");
    context.append(right_type.toString());
    throw ValidationError("Binary operator used on variant type(s).", context, binary.line_number);
  }

  const auto [left_arithmetic, left_callable, left_array, left_alignment] =
      table.detailsOfType(left_type.getTypename());
  const auto [right_arithmetic, right_callable, right_array, right_alignment] =
      table.detailsOfType(right_type.getTypename());

  switch (binary.opr) {
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
    if (!left_arithmetic || !right_arithmetic) {
      std::string context = left_type.toString();
      context.append(" and ");
      context.append(right_type.toString());
      throw ValidationError("Non-arithmetic expression in arithmetic binary operation.", context, binary.line_number);
    }
    break;

  case Operator::BITAND:
  case Operator::BITOR:
  case Operator::BITXOR:
  case Operator::BITNOT:
    if (!left_arithmetic || !right_arithmetic) {
      std::string context = left_type.toString();
      context.append(" and ");
      context.append(right_type.toString());
      throw ValidationError("Non-arithmetic expression in arithmetic binary operation.", context, binary.line_number);
    }

    if (!right_type.convertible_to(left_type)) { //since they're both single types, convertible will evaluate equality. prolly should change tho
      std::string context = left_type.toString();
      context.append(" and ");
      context.append(right_type.toString());
      throw ValidationError("Bitwise operations on different types not allowed.", context, binary.line_number);
    }
    break;

  case Operator::ASSIGN:
    if (!left_is_mutable) {
      std::string context("Type of expression is ");
      context.append(left_type.toString());
      throw ValidationError("Left expression in assignment non-mutable.", context, binary.line_number);
    }


    if (left_arithmetic && right_arithmetic)
      break;

    if (right_type.convertible_to(left_type)) { //see above comment
      std::string context = left_type.toString();
      context.append(" and ");
      context.append(right_type.toString());
      throw ValidationError("Assignment with non-convertible types.", context, binary.line_number);
    }
    break;

  case Operator::EQUAL:
  case Operator::NOT_EQUAL:
    if ((left_arithmetic && right_arithmetic) || left_type == right_type)
      break;

    {
      std::string context = left_type.toString();
      context.append(" and ");
      context.append(right_type.toString());
      throw ValidationError("Equality operator used on different non-arithmetic types.", context, binary.line_number);
    }

  default:
    throw ValidationError("Non binary operator set in binary expression. How?", operatorToString(binary.opr), binary.line_number);
  }

  switch (binary.opr) {
  case Operator::ADD:
  case Operator::SUBTRACT:
  case Operator::MULTIPLY:
  case Operator::DIVIDE:
  case Operator::POWER:
  case Operator::MODULUS:
    return  left_type;

  case Operator::LESS:
  case Operator::GREATER:
  case Operator::LESS_EQUAL:
  case Operator::GREATER_EQUAL:
  case Operator::AND:
  case Operator::OR:
  case Operator::XOR:
    return bool_literal_type;

  case Operator::BITAND:
  case Operator::BITOR:
  case Operator::BITXOR:
  case Operator::BITNOT:
  case Operator::ASSIGN:
    return left_type.asImmutable();

  case Operator::EQUAL:
  case Operator::NOT_EQUAL:
    return bool_literal_type;

  default:
    throw ValidationError("Non binary operator set in binary expression. How?", operatorToString(binary.opr), binary.line_number);
  }
}

static Type validateUnaryExpression(const UnaryExpression &unary, SymbolTable& table) {
  const auto& type = validateExpression(unary.expr, table);
  if (type.isVariant()) {
    const std::string context = type.toString();
    throw ValidationError("Binary operator used on variant type(s).", context, unary.line_number);
  }

  const auto [arithmetic, callable, array, alignment] = table.detailsOfType(type.getTypename());
  if (!arithmetic) {
    const std::string context = type.toString();
    throw ValidationError("Unary operator used on non-arithmetic expression.", context, unary.line_number);
  }

  if (unary.opr == Operator::UNARY_MINUS)
    return type.asImmutable();


  if (unary.opr == Operator::NOT)
    return bool_literal_type;


  if (!type.is_mutable) {
    const std::string context = type.toString();
    throw ValidationError("Pre/Postfix operator used on non-mutable expression.", context, unary.line_number);
  }


  if (unary.opr == Operator::POST_INCREMENT ||
      unary.opr == Operator::POST_DECREMENT)
    return type.asImmutable();

  if (unary.opr == Operator::ADDRESS_OF)
    return Type("raw", false, new Type(type));

  return type;
}

static Type validateExpression(const Expression *expression, SymbolTable& table) {

  return utils_match(expression->value,
    utils_callon(const UnaryExpression&, validateUnaryExpression, table),
    utils_callon(const BinaryExpression&, validateBinaryExpression, table),
    utils_callon(const CallingExpression&, validateCallingExpression, table),
    utils_callon(const SubscriptExpression&, validateSubscriptExpression, table),
    utils_callon(const IdentifierExpression&, validateIdentifierExpression, table),
    utils_callon(const LiteralExpression&, validateLiteralExpression)
  );
}

/* Expressions */

/* Statements */

static void validateStatement(const Statement *statement, SymbolTable& table);

static void validateExpressionStatement(const ExpressionStatement &expression_statement, SymbolTable& table) {
  if (expression_statement.expr)
    validateExpression(expression_statement.expr, table);
}

static void validateReturnStatement(const ReturnStatement &return_statement, SymbolTable& table) {

  if (return_statement.return_value) {
    const auto& ret_type = validateExpression(return_statement.return_value, table);
    if (!ret_type.convertible_to(table.returnTypeOfCurrentScope())) {
      std::string context("Scope return type is '");
      context.append(table.returnTypeOfCurrentScope().toString());
      context.append("' and expression '");
      context.append(std::visit(ExpressionToStringVisitor{}, return_statement.return_value->value));
      context.append("' returns type '");
      context.append(ret_type.toString());
      context.push_back('\'');

      throw ValidationError("Return statement's type is not compatible with return type of scope.", context, return_statement.line_number);
    }

    return;
  }

  if (!table.returnTypeOfCurrentScope().isDevoid()) {
    std::string context("Scope return type is ");
    context.append(table.returnTypeOfCurrentScope().toString());
    throw ValidationError("No value returned from return statement when scope expects a value.", context, return_statement.line_number);
  }
}

static void validateScopedStatement(const ScopedStatement &scoped, SymbolTable& table) {
  for (const auto s : scoped.scope_body)
    validateStatement(s, table);
}

static void validateWhileLoop(const WhileLoop &while_loop, SymbolTable& table) {
  const auto type = validateExpression(while_loop.condition, table);
  if (type.isVariant())
    throw ValidationError("Variant in while loop condition.", type.toString(), while_loop.line_number);


  const auto [arithmetic, callable, array, alignment] = table.detailsOfType(type.getTypename());
  if (!arithmetic) {
    std::string context("Condition is of type ");
    context.append(type.toString());
    throw ValidationError("While Loop condition non-convertible to boolean.", context, while_loop.line_number);
  }

  validateStatement(while_loop.loop_body, table);
}

static void validateForLoop(const ForLoop &for_loop, SymbolTable& table) {
  table.enterScope();
  validateStatement(for_loop.var_statement, table);

  if (for_loop.condition) {
    const auto type  = validateExpression(for_loop.condition, table);
    if (type.isVariant())
      throw ValidationError("Variant used in for loop condition.", type.toString(), for_loop.line_number);

    const auto [arithmetic, callable, array, alignment] =
        table.detailsOfType(type.getTypename());

    if (!arithmetic) {
      std::string context("Condition is of type ");
      context.append(type.toString());
      throw ValidationError("For Loop condition non-convertible to boolean.", context, for_loop.line_number);
    }
  }

  if (for_loop.iteration)
    validateExpression(for_loop.iteration, table);

  validateStatement(for_loop.loop_body, table);
  table.enterScope();
}

static void validateIfStatement(const IfStatement &if_statement, SymbolTable& table) {
  const auto type = validateExpression(if_statement.condition, table);
  if (type.isVariant())
    throw ValidationError("Variant type used in if statement condition.", type.toString(), if_statement.line_number);

  const auto [arithmetic, callable, array, alignment] = table.detailsOfType(type.getTypename());
  if (!arithmetic) {
    std::string context("Condition is of type ");
    context.append(type.toString());
    throw ValidationError("If statement condition non-convertible to boolean.", context, if_statement.line_number);

  }

  validateStatement(if_statement.true_branch, table);
  if (if_statement.false_branch)
    validateStatement(if_statement.false_branch, table);
}

static void validateVarDeclaration(const VarDeclaration &declaration, SymbolTable& table) {
  if (table.isSymbolInCurrentScope(declaration.ident))
    throw ValidationError("Redefinition of symbol name in variable declaration.", declaration.ident, declaration.line_number);

  if (declaration.expr) {
    const auto initialization_type = validateExpression(declaration.expr, table);
    if (!declaration.type.convertible_to(validateExpression(declaration.expr, table))) {
      std::string context("Declared type is '");
      context.append(declaration.type.toString());
      context.append("' and expression '");
      context.append(std::visit(ExpressionToStringVisitor{}, declaration.expr->value));
      context.append("' is of type '");
      context.append(initialization_type.toString());
      context.push_back('\'');

      throw ValidationError("Initialization statement's type is not compatible with return type of scope.", context, declaration.line_number);
    }
  }

  table.addLocalVariable(declaration.ident, declaration.type);
}

static void validateStatement(const Statement *statement, SymbolTable& table) {


  utils_match(statement->value,
    utils_callon(const VarDeclaration&, validateVarDeclaration, table),
    utils_callon(const IfStatement&, validateIfStatement, table),
    utils_callon(const ForLoop&, validateForLoop, table),
    utils_callon(const WhileLoop&, validateWhileLoop, table),
    utils_callon(const ScopedStatement&, validateScopedStatement, table),
    utils_callon(const ReturnStatement&, validateReturnStatement, table),
    utils_callon(const ExpressionStatement&, validateExpressionStatement, table),
    );

}

/* Statements */

static void validateFunction(SymbolTable &table, ParsedFunction &func) { // TO DO: Add line number functionality for functions
  std::vector<Type> param_types;
  table.enterScope(func.return_type);

  for (auto &decl : func.parameter_list) {
    validateVarDeclaration(decl, table);
    param_types.emplace_back(decl.type);
  }

  table.addFunction(func.name, func.return_type, std::move(param_types));

  const bool is_devoid_return = func.return_type.isDevoid();
  bool has_return_statement = false;
  for (const auto statement : func.function_body) {
    validateStatement(statement, table);

    if (!is_devoid_return && std::holds_alternative<ReturnStatement>(statement->value))
      has_return_statement = true;
  }

  if (!has_return_statement) {
    if (!is_devoid_return) {
      std::string context("Expected type '");
      context.append(func.return_type.toString());
      throw ValidationError("Value returning function has no return statement.", context, 42069);
    }

    func.function_body.push_back(new Statement(ReturnStatement(0)));
  }


  table.leaveScope();
}

static void validateGlobals(SymbolTable &table, const std::vector<VarDeclaration> &globals) {
  for (auto &decl : globals)
    table.addGlobalVariable(decl.ident, decl.type);
}

static std::vector<ValidatedFunction> parsedToValidated(std::vector<ParsedFunction>& parsed_funcs) {
  std::vector<ValidatedFunction> validated_funcs;
  validated_funcs.reserve(parsed_funcs.size());
  for (auto& f : parsed_funcs)
    validated_funcs.emplace_back(std::move(f.return_type), std::move(f.name), std::move(f.parameter_list), std::move(f.function_body));

  return validated_funcs;
}

ValidatedTU Validation::validateTU(ParsedTU &&ptu) {
  SymbolTable table;

  validateGlobals(table, ptu.globals);

  for (auto &f : ptu.functions)
    validateFunction(table, f);

  if (Settings::doOutputValidation()) {
    std::cout << "--- Validation Passed ---\n\n";
    std::quick_exit(0);
  }

  return {std::move(ptu.globals),
     parsedToValidated(ptu.functions), std::move(table)
  };
}