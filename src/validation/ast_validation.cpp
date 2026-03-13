#include "ast_validation.hpp"
#include "ast/expressions.hpp"
#include "error.hpp"
#include "parsing/parse.hpp"
#include "settings.hpp"
#include "symbol_table.hpp"
#include "utilities/variant_overload.hpp"

#include <cassert>
#include <iostream>
#include <variant>

using namespace Parser;
using namespace Validation;
using namespace AST;


ValidatedFunction::~ValidatedFunction() {for (const auto s : function_body) delete s;}

/* Expressions */
static InstantiatedType validateExpression(Expression &expression, SymbolTable& table);

static InstantiatedType validateLiteralExpression(const LiteralExpression &literal) {
  switch (literal.type) {
  case LiteralExpression::INT:
    switch (literal.bit_width) {
    case 8:
      return i8_instance;
    case 16:
      return i16_instance;
    case 32:
      return i32_instance;
    case 64:
      return i64_instance;
    default:
      assert(false);
    }
  case LiteralExpression::UINT:
    switch (literal.bit_width) {
    case 8:
      return u8_instance;
    case 16:
      return u16_instance;
    case 32:
      return u32_instance;
    case 64:
      return u64_instance;
    default:
      assert(false);
    }
  case LiteralExpression::FLOAT:
    return f32_instance;
  case LiteralExpression::DOUBLE:
    return f64_instance;
  case LiteralExpression::BOOL:
    return bool_instance;
  case LiteralExpression::CHAR:
    return char_instance;
  case LiteralExpression::STRING:
    return string_instance;

  default:
    assert(false);
  }
}

static InstantiatedType validateVariableExpression(const IdentifierExpression &identifier, const SymbolTable& table) {
  if (!table.containsVariable(identifier.ident))
    throw ValidationError("Expected variable name.", identifier.ident, identifier.line_number);


  return table.closestVariable(identifier.ident);
}

static InstantiatedType validateSubscriptExpression(const SubscriptExpression &, SymbolTable&) {assert(false);}

static InstantiatedType validateCallingExpression(const CallingExpression &calling, SymbolTable& table) {
  if (not std::holds_alternative<IdentifierExpression>(*calling.func))
    throw ValidationError("Callable non-functions unfortunately not supported :(", std::visit(ExpressionToStringVisitor{}, *calling.func), calling.line_number);

  const auto& identifier = std::get<IdentifierExpression>(*calling.func);
  if (not table.containsFunction(identifier.ident))
    throw ValidationError("Callable non-functions unfortunately not supported :(", identifier.ident, calling.line_number);

  std::vector<InstantiatedType> provided_params;
  for (const auto& param : calling.parameters)
    provided_params.emplace_back(validateExpression(*param, table));

  const auto result = table.returnTypeOfCall(identifier.ident, provided_params);
  if (result)
    return {result.value(), {}};

  const auto error = result.error();
  std::string context{"Types of parameters used in call: "};
  for (const auto& t : provided_params) {
    context.append(t.toString());
    context.append(", ");
  }
  if (provided_params.empty())
    context = "Called with no parameters";
  else {
    context.pop_back();
    context.pop_back();
  }

  if (error == SymbolTable::CallError::NO_SUITABLE_FUNCTION)
    throw ValidationError("No suitable function found for call.", context, calling.line_number);

  throw ValidationError("Ambiguous function call.", context, calling.line_number);
}

static InstantiatedType validateBinaryExpression(BinaryExpression &binary, SymbolTable& table) {
  InstantiatedType left_instance = validateExpression(*binary.expr_left, table);
  const bool left_is_mutable = left_instance.details.is_mutable;
  const InstantiatedType right_instance = validateExpression(*binary.expr_right, table);

  const auto left_type = left_instance.type;
  const auto right_type = right_instance.type;

  if (left_type not_eq right_type) {
    std::string context = left_type->toString();
    context.append(" and ");
    context.append(right_type->toString());
    throw ValidationError("Binary operator used on differing types.", context, binary.line_number);
  }

  if (left_type->isVariant()) {
    std::string context = left_type->toString();
    context.append(" and ");
    context.append(right_type->toString());
    throw ValidationError("Binary operator used on variant types.", context, binary.line_number);
  }

  const bool left_arithmetic = left_type->isArithmetic();
  const bool right_arithmetic = right_type->isArithmetic();
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
    if (not left_arithmetic) { //types should be same, so checking just one is sufficient
      std::string context = left_type->toString();
      context.append(" and ");
      context.append(right_type->toString());
      throw ValidationError("Non-arithmetic expression in arithmetic binary operation.", context, binary.line_number);
    }
    break;

  case Operator::AND:
  case Operator::OR:
  case Operator::XOR:
    if (not left_type->isBool()) {
      std::string context = left_type->toString();
      context.append(" and ");
      context.append(right_type->toString());
      throw ValidationError("Non-boolean expression(s) in boolean binary operation.", context, binary.line_number);
    }
    break;


  case Operator::BITAND:
  case Operator::BITOR:
  case Operator::BITXOR:
    if (not left_arithmetic) {
      std::string context = left_type->toString();
      context.append(" and ");
      context.append(right_type->toString());
      throw ValidationError("Non-arithmetic expression(s) in bitwise operation.", context, binary.line_number);
    }

    break;

  case Operator::ASSIGN:
    if (not left_is_mutable) {
      std::string context("Type of expression is ");
      context.append(left_type->toString());
      throw ValidationError("Left expression in assignment non-mutable.", context, binary.line_number);
    }
    break;

  case Operator::EQUAL:
  case Operator::NOT_EQUAL:
    if (left_arithmetic and right_arithmetic)
      break;

    {
      std::string context = left_type->toString();
      context.append(" and ");
      context.append(right_type->toString());
      throw ValidationError("Equality operator used on non-arithmetic types.", context, binary.line_number);
    }

  default:
    assert(false);
  }

  switch (binary.opr) {
  case Operator::ADD:
  case Operator::SUBTRACT:
  case Operator::MULTIPLY:
  case Operator::DIVIDE:
  case Operator::POWER:
  case Operator::MODULUS:
    return left_instance;

  case Operator::LESS:
  case Operator::GREATER:
  case Operator::LESS_EQUAL:
  case Operator::GREATER_EQUAL:
  case Operator::AND:
  case Operator::OR:
    return bool_instance;
  case Operator::XOR:
    binary.opr = Operator::NOT_EQUAL;
    return bool_instance;

  case Operator::BITAND:
  case Operator::BITOR:
  case Operator::BITXOR:
  case Operator::BITNOT:
  case Operator::ASSIGN:
    left_instance.details.is_mutable = false;
    return left_instance;

  case Operator::EQUAL:
  case Operator::NOT_EQUAL:
    return bool_instance;

  default:
    assert(false);
  }
}

static InstantiatedType validateUnaryExpression(UnaryExpression &unary, SymbolTable& table) {
  InstantiatedType instance = validateExpression(*unary.expr, table);
  if (instance.type->isVariant()) {
    const std::string context = instance.toString();
    throw ValidationError("Unary operator used on variant type(s).", context, unary.line_number);
  }

  const bool arithmetic = instance.type->isArithmetic();
  if (unary.opr == Operator::UNARY_MINUS) {
    if (not arithmetic)
      throw ValidationError("Unary minus used on non-arithmetic expression.", instance.toString(), unary.line_number);
    instance.details.is_mutable = false;
    return instance;
  }

  if (unary.opr == Operator::BITNOT) {
    if (not arithmetic)
      throw ValidationError("bitnot operator used non-arithmetic expression", instance.toString(), unary.line_number);
    instance.details.is_mutable = false;
    return instance;
  }

  if (unary.opr == Operator::NOT) {
    if (not instance.type->isBool())
      throw ValidationError("not operator used non-boolean expression", instance.toString(), unary.line_number);
    unary.opr = Operator::BITNOT;
    instance.details.is_mutable = false;
    return instance;
  }

  if (unary.opr == Operator::ADDRESS_OF)
    return {table.addRawPointer(instance.type, instance.details.is_mutable), {}};

  if (not instance.details.is_mutable) {
    const std::string context = instance.toString();
    throw ValidationError("Pre/Postfix operator used on non-mutable expression.", context, unary.line_number);
  }


  if (unary.opr == Operator::POST_INCREMENT ||
      unary.opr == Operator::POST_DECREMENT) {
    instance.details.is_mutable = false;
    return instance;
  }


  return instance;
}

static InstantiatedType validateExpression(Expression &expression, SymbolTable& table) {
  return utils_match(expression,
    utils_callon(UnaryExpression&, validateUnaryExpression, table),
    utils_callon(BinaryExpression&, validateBinaryExpression, table),
    utils_callon(const CallingExpression&, validateCallingExpression, table),
    utils_callon(const SubscriptExpression&, validateSubscriptExpression, table),
    utils_callon(const IdentifierExpression&, validateVariableExpression, table),
    utils_callon(const LiteralExpression&, validateLiteralExpression)
  );
}

/* Expressions */

/* Statements */

static void validateStatement(const Statement &statement, SymbolTable& table);

static void validateExpressionStatement(const ExpressionStatement &expression_statement, SymbolTable& table) {
  if (expression_statement.expr)
    validateExpression(*expression_statement.expr, table);
}

static void validateReturnStatement(const ReturnStatement &return_statement, SymbolTable& table) {

  if (return_statement.return_value) {
    const InstantiatedType ret_type = validateExpression(*return_statement.return_value, table);
    if (not ret_type.type->convertibleTo(table.returnTypeOfCurrentScope())) {
      std::string context("Scope return type is '");
      context.append(table.returnTypeOfCurrentScope()->toString());
      context.append("' and expression '");
      context.append(std::visit(ExpressionToStringVisitor{}, *return_statement.return_value));
      context.append("' returns type '");
      context.append(ret_type.toString());
      context.push_back('\'');

      throw ValidationError("Return statement's type is not compatible with return type of scope.", context, return_statement.line_number);
    }

    return;
  }

  if (!table.returnTypeOfCurrentScope()->isDevoid()) {
    std::string context("Scope return type is ");
    context.append(table.returnTypeOfCurrentScope()->toString());
    throw ValidationError("No value returned from return statement when scope expects a value.", context, return_statement.line_number);
  }
}

static void validateScopedStatement(const ScopedStatement &scoped, SymbolTable& table) {
  for (const auto s : scoped.scope_body)
    validateStatement(*s, table);
}

static void validateWhileLoop(const WhileLoop &while_loop, SymbolTable& table) {
  const InstantiatedType instance = validateExpression(*while_loop.condition, table);

  const bool arithmetic = instance.type->isArithmetic();
  if (not arithmetic) {
    std::string context("Condition is of type ");
    context.append(instance.toString());
    throw ValidationError("While Loop condition non-convertible to boolean.", context, while_loop.line_number);
  }

  validateStatement(*while_loop.loop_body, table);
}

static void validateForLoop(const ForLoop &for_loop, SymbolTable& table) {
  table.enterScope(devoid_type);
  validateStatement(*for_loop.var_statement, table);

  if (for_loop.condition) {
    const InstantiatedType instance  = validateExpression(*for_loop.condition, table);
    if (instance.type->isVariant())
      throw ValidationError("Variant used in for loop condition.", instance.toString(), for_loop.line_number);

    const bool arithmetic = instance.type->isArithmetic();
    if (not arithmetic) {
      std::string context("Condition is of type ");
      context.append(instance.toString());
      throw ValidationError("For Loop condition non-convertible to boolean.", context, for_loop.line_number);
    }
  }

  if (for_loop.iteration)
    validateExpression(*for_loop.iteration, table);

  validateStatement(*for_loop.loop_body, table);
  table.leaveScope();
}

static void validateIfStatement(const IfStatement &if_statement, SymbolTable& table) {
  const InstantiatedType instance = validateExpression(*if_statement.condition, table);
  if (instance.type->isVariant())
    throw ValidationError("Variant type used in if statement condition.", instance.toString(), if_statement.line_number);

  if (not instance.type->isBool()) {
    std::string context("Condition is of type ");
    context.append(instance.toString());
    throw ValidationError("If statement condition non-boolean.", context, if_statement.line_number);
  }

  validateStatement(*if_statement.true_branch, table);
  if (if_statement.false_branch)
    validateStatement(*if_statement.false_branch, table);
}

static void validateVarDeclaration(const VarDeclaration &declaration, SymbolTable& table) {
  if (table.isSymbolInCurrentScope(declaration.ident))
    throw ValidationError("Redefinition of symbol name in variable declaration.", declaration.ident, declaration.line_number);

  if (declaration.expr) {
    const InstantiatedType initialization_type = validateExpression(*declaration.expr, table);
    if (not declaration.type.type->convertibleTo(initialization_type.type)) {
      std::string context("Declared type is '");
      context.append(declaration.type.toString());
      context.append("' and expression '");
      context.append(std::visit(ExpressionToStringVisitor{}, *declaration.expr));
      context.append("' is of type '");
      context.append(initialization_type.toString());
      context.push_back('\'');

      throw ValidationError("Initialization statement's type is not compatible with variable type.", context, declaration.line_number);
    }
  }

  table.addLocalVariable(declaration.ident, declaration.type);
}

static void validateStatement(const Statement &statement, SymbolTable& table) {

  utils_match(statement,
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
  std::vector<const Type*> param_types;
  table.enterScope(func.return_type);

  for (auto &decl : func.parameter_list) {
    validateVarDeclaration(decl, table);
    param_types.emplace_back(decl.type.type);
  }

  table.addFunction(func.name, std::move(param_types), func.return_type);

  const bool is_devoid_return = func.return_type->isDevoid();
  bool has_return_statement = false;
  for (const auto statement : func.function_body) {
    validateStatement(*statement, table);

    if (!is_devoid_return && std::holds_alternative<ReturnStatement>(*statement))
      has_return_statement = true;
  }

  if (!has_return_statement) {
    if (!is_devoid_return) {
      std::string context("Expected type '");
      context.append(func.return_type->toString());
      throw ValidationError("Value returning function has no return statement.", context, 42069);
    }

    func.function_body.emplace_back(new Statement{std::in_place_type<ReturnStatement>, 0});
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

  return {std::move(table), std::move(ptu.globals), parsedToValidated(ptu.functions)};
}