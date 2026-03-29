#include "peep_mir.hpp"
#include "ast/ast.hpp"
#include "error.hpp"
#include "parsing/parse.hpp"
#include "semantic_analysis/symbol_table.hpp"
#include "settings.hpp"
#include "utilities/typedefs.hpp"

#include <cassert>
#include <format>
#include <iostream>
#include <variant>

/*
using namespace LOM;
using namespace LOM::Parser;
using namespace LOM::PeepMIR;
using namespace LOM::AST;

namespace {

struct Peeper {
  Function& current_function;
  SymbolTable& table;

  InstantiatedType peepLiteralExpression(const LiteralExpression &literal) const {
    return {literal.type, {}};
  }

  InstantiatedType peepVariableExpression(const IdentifierExpression &identifier) const {
    if (!table.containsVariable(identifier.ident)) {
      if (table.containsFunction(identifier.ident))
        throw ValidationError("Function name where variable name expected.", identifier.ident, identifier.line_number);

      throw ValidationError("Undeclared Identifier.", identifier.ident, identifier.line_number);
    }


    return table.closestVariable(identifier.ident);
  }

  InstantiatedType peepSubscriptExpression(const SubscriptExpression&) const {assert(false);}

  InstantiatedType peepCallingExpression(const CallingExpression &calling) {
    if (not std::holds_alternative<IdentifierExpression>(*calling.called))
      throw ValidationError("Callable non-functions unfortunately not supported :(", std::visit(ExpressionToStringVisitor{}, *calling.called), calling.line_number);

    const auto& identifier = std::get<IdentifierExpression>(*calling.called);
    if (not table.containsFunction(identifier.ident))
      throw ValidationError("Callable non-functions unfortunately not supported :(", identifier.ident, calling.line_number);

    std::vector<InstantiatedType> provided_params;
    for (const auto& param : calling.parameters)
      provided_params.emplace_back(peepExpression(*param));

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

  InstantiatedType peepBinaryExpression(BinaryExpression &binary) {
    InstantiatedType left_instance = peepExpression(*binary.expr_left);
    const bool left_is_mutable = left_instance.details.is_mutable;
    const InstantiatedType right_instance = peepExpression(*binary.expr_right);

    const auto left_type = left_instance.type;
    const auto right_type = right_instance.type;


    if (not left_type->convertibleTo(right_type) && not right_type->convertibleTo(left_type))
      throw ValidationError("Binary operator used on differing types.", std::format("'{}' and '{}'", left_type->toString(), right_type->toString()), binary.line_number);


    if (left_type->isVariant()) //types should be same, so checking just one is sufficient
      throw ValidationError("Binary operator used on variant types.", std::format("'{}' and '{}'", left_type->toString(), right_type->toString()), binary.line_number);


    const bool arithmetic = left_type->isArithmetic();
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
      if (not arithmetic)
        throw ValidationError("Non-arithmetic expression in arithmetic binary operation.", std::format("'{}' and '{}'", left_type->toString(), right_type->toString()), binary.line_number);
      break;

    case Operator::AND:
    case Operator::OR:
    case Operator::XOR:
      if (not left_type->isBool())
        throw ValidationError("Non-boolean expressions in boolean binary operation.", std::format("'{}' and '{}'", left_type->toString(), right_type->toString()), binary.line_number);
      break;

    case Operator::BITAND:
    case Operator::BITOR:
    case Operator::BITXOR:
      if (not arithmetic)
        throw ValidationError("Non-arithmetic expression(s) in bitwise operation.", std::format("'{}' and '{}'", left_type->toString(), right_type->toString()), binary.line_number);
      break;

    case Operator::ASSIGN:
      if (not left_is_mutable)
        throw ValidationError("Left expression in assignment non-mutable.", std::format("Type of expression is '{}'", left_type->toString()), binary.line_number);
      break;

    case Operator::EQUAL:
    case Operator::NOT_EQUAL:
      break;

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

  InstantiatedType peepUnaryExpression(UnaryExpression &unary) {
    InstantiatedType instance = peepExpression(*unary.expr);
    if (instance.type->isVariant())
      throw ValidationError("Unary operator used on variant type.", instance.toString(), unary.line_number);


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

    if (not instance.details.is_mutable)
      throw ValidationError("Pre/Postfix operator used on non-mutable expression.", instance.toString(), unary.line_number);



    if (unary.opr == Operator::POST_INCREMENT ||
        unary.opr == Operator::POST_DECREMENT) {
      instance.details.is_mutable = false;
      return instance;
        }


    return instance;
  }

  InstantiatedType peepExpression(Expression &expression) {
    return utils_match(expression,
      utils_callon(UnaryExpression&, peepUnaryExpression),
      utils_callon(BinaryExpression&, peepBinaryExpression),
      utils_callon(const CallingExpression&, peepCallingExpression),
      utils_callon(const SubscriptExpression&, peepSubscriptExpression),
      utils_callon(const IdentifierExpression&, peepVariableExpression),
      utils_callon(const LiteralExpression&, peepLiteralExpression)
    );
  }

  void peepExpressionStatement(const ExpressionStatement &expression_statement) {
    if (expression_statement.expr)
      peepExpression(*expression_statement.expr);
  }

  void peepReturnStatement(const ReturnStatement &return_statement) {

    if (return_statement.return_value) {
      const InstantiatedType ret_type = peepExpression(*return_statement.return_value);
      if (not ret_type.type->convertibleTo(table.returnTypeOfCurrentScope()))
        throw ValidationError("Return statement's type is not compatible with return type of scope.",
         std::format("Scope return type is '{}' and expression '{}' returns type '{}'",
           table.returnTypeOfCurrentScope()->toString(), std::visit(ExpressionToStringVisitor{}, *return_statement.return_value), ret_type.toString()),
             return_statement.line_number);


      return;
    }

    if (!table.returnTypeOfCurrentScope()->isDevoid())
      throw ValidationError("No value returned from return statement when scope expects a value.",
        std::format("Scope return type is '{}'", table.returnTypeOfCurrentScope()->toString()), return_statement.line_number);

  }

  void peepScopedStatement(const ScopedStatement &scoped) {
    for (const auto s : scoped.scope_body)
      peepStatement(*s);
  }

  void peepWhileLoop(const WhileLoop &while_loop) {
    const InstantiatedType instance = peepExpression(*while_loop.condition);

    if (not instance.type->isBool())
      throw ValidationError("While Loop condition non-convertible to boolean.",
        std::format("Condition is of type '{}'", instance.toString()), while_loop.line_number);


    peepStatement(*while_loop.loop_body);
  }

  void peepForLoop(const ForLoop &for_loop) {
    table.enterScope(&devoid_type);
    if (for_loop.var_statement)
      peepStatement(*for_loop.var_statement);

    if (for_loop.condition) {
      const InstantiatedType instance  = peepExpression(*for_loop.condition);
      if (instance.type->isVariant())
        throw ValidationError("Variant used in for loop condition.", instance.toString(), for_loop.line_number);

      if (not instance.type->isBool())
        throw ValidationError("Non-boolean for-loop condition.", std::format("Condition is of type '{}'", instance.toString()), for_loop.line_number);

    }

    if (for_loop.iteration)
      peepExpression(*for_loop.iteration);

    peepStatement(*for_loop.loop_body);
    table.leaveScope();
  }

  void peepIfStatement(const IfStatement &if_statement) {
    const InstantiatedType instance = peepExpression(*if_statement.condition);
    if (instance.type->isVariant())
      throw ValidationError("Variant type used in if statement condition.", std::format("Condition is of type '{}'", instance.toString()), if_statement.line_number);

    if (not instance.type->isBool())
      throw ValidationError("If statement condition non-boolean.", std::format("Condition is of type '{}'", instance.toString()), if_statement.line_number);

    peepStatement(*if_statement.true_branch);
    if (if_statement.false_branch)
      peepStatement(*if_statement.false_branch);
  }

  void peepVarDeclaration(const VarDeclaration &declaration) {
    if (table.isSymbolInCurrentScope(declaration.ident))
      throw ValidationError("Redefinition of symbol name in variable declaration.", std::format("Symbol name: '{}'", declaration.ident), declaration.line_number);

    if (declaration.expr) {
      const InstantiatedType initialization_type = peepExpression(*declaration.expr);
      if (not initialization_type.type->convertibleTo(declaration.type.type))
        throw ValidationError("Variable initialization's type is not compatible with variable type.",
          std::format("Variable '{}' is of type '{}' and expression '{}' is of type '{}'.",
          declaration.ident,  declaration.type.toString(), std::visit(ExpressionToStringVisitor{}, *declaration.expr), initialization_type.toString()),
          declaration.line_number);

    }

    table.addLocalVariable(declaration.ident, declaration.type);
  }

  void peepStatement(const Statement &statement) {
    utils_match(statement,
      utils_callon(const VarDeclaration&, peepVarDeclaration),
      utils_callon(const IfStatement&, peepIfStatement),
      utils_callon(const ForLoop&, peepForLoop),
      utils_callon(const WhileLoop&, peepWhileLoop),
      utils_callon(const ScopedStatement&, peepScopedStatement),
      utils_callon(const ReturnStatement&, peepReturnStatement),
      utils_callon(const ExpressionStatement&, peepExpressionStatement),
      );
  }
};

void peepFunction(ParsedFunction &func, SymbolTable& table) {
  table.enterFunctionScope(func.name);

  table.leaveFunctionScope();
}

}

PeepTU PeepMIR::lowerToPeep(ParsedTU &&parsed_tu) {
  SymbolTable table;
  PeepTU peeped_tu;
  for (auto &func : parsed_tu.functions)
    peepFunction(func, table);

  if (Settings::doOutputValidation()) {
    std::cout << "--- Validation Passed ---\n\n";
    std::quick_exit(0);
  }


  assert(false);
} */