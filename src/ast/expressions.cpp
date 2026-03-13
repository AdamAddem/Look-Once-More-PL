#include "expressions.hpp"
#include <cassert>
#include <iostream>


using namespace AST;

//see header before you insist I use unique pointer
UnaryExpression::~UnaryExpression() { expr.destroy(); }
BinaryExpression::~BinaryExpression() { expr_left.destroy(); expr_right.destroy(); }
CallingExpression::~CallingExpression() { func.destroy(); for (const auto e : parameters) delete e;}
SubscriptExpression::~SubscriptExpression() { arr.destroy(); inside.destroy(); }

void PrintExpressionVisitor::operator()(const UnaryExpression &unary) const noexcept { std::cout << ExpressionToStringVisitor{}(unary); }
void PrintExpressionVisitor::operator()(const BinaryExpression &binary) const noexcept { std::cout << ExpressionToStringVisitor{}(binary); }
void PrintExpressionVisitor::operator()(const CallingExpression &calling) const noexcept { std::cout << ExpressionToStringVisitor{}(calling); }
void PrintExpressionVisitor::operator()(const SubscriptExpression &subscript) const noexcept { std::cout << ExpressionToStringVisitor{}(subscript); }
void PrintExpressionVisitor::operator()(const IdentifierExpression &identifier) const noexcept { std::cout << ExpressionToStringVisitor{}(identifier); }
void PrintExpressionVisitor::operator()(const LiteralExpression &literal) const noexcept { std::cout << ExpressionToStringVisitor{}(literal); }

std::string ExpressionToStringVisitor::operator()(const UnaryExpression &unary) const noexcept {
  std::string retval;
  if (isCategoryPREFIX_OPS(unary.opr)) {
    retval.append(operatorToString(unary.opr));
    if (unary.opr == Operator::NOT || unary.opr == Operator::BITNOT)
      retval.push_back(' ');
    retval.append(std::visit(ExpressionToStringVisitor{}, *unary.expr));
  } else {
    retval.append(std::visit(ExpressionToStringVisitor{}, *unary.expr));
    retval.append(operatorToString(unary.opr));
  }

  return retval;
}

std::string ExpressionToStringVisitor::operator()(const BinaryExpression &binary) const noexcept {
  std::string string_rep{"( "};
  string_rep.append(std::visit(ExpressionToStringVisitor{}, *binary.expr_left));

  string_rep.push_back(' ');
  string_rep.append(operatorToString(binary.opr));
  string_rep.push_back(' ');

  string_rep.append(std::visit(ExpressionToStringVisitor{}, *binary.expr_right));
  string_rep.append(" )");

  return string_rep;
}

std::string ExpressionToStringVisitor::operator()(const CallingExpression &calling) const noexcept {
  std::string retval = std::visit(ExpressionToStringVisitor{}, *calling.func);
  retval.append("( ");
  for (const auto p : calling.parameters) {
    retval.append(std::visit(ExpressionToStringVisitor{}, *p));
    retval.append(", ");
  }

  if (!calling.parameters.empty()) {
   retval.pop_back();
   retval.pop_back();
  }

  retval.append(" )");
  return retval;
}

std::string ExpressionToStringVisitor::operator()(const SubscriptExpression &subscript) const noexcept {
  std::string retval = std::visit(ExpressionToStringVisitor{}, *subscript.arr);
  retval.push_back('[');
  retval.append(std::visit(ExpressionToStringVisitor{}, *subscript.inside));
  retval.push_back(']');
  return retval;
}

std::string ExpressionToStringVisitor::operator()(const IdentifierExpression &identifier) const noexcept {
  return identifier.ident;
}

std::string ExpressionToStringVisitor::operator()(const LiteralExpression &literal) const noexcept {

  if (literal.type->isIntegral()) {
    if (literal.type->isUnsignedIntegral())
      return std::to_string(literal.getUint());

    return std::to_string(literal.getInt());
  }

  if (literal.type->isFloating()) {
    if (literal.type == &f64_type)
      return std::to_string(literal.getDouble());
    assert(false);
  }

  if (literal.type == &bool_type)
    return literal.getBool() ?  "true" : "false";


  if (literal.type == &char_type)
    return std::string("\'") + literal.getChar() + "\'";


  if (literal.type == &string_type)
    return std::string("\"") + literal.getString() + "\"";

  assert(false);
}