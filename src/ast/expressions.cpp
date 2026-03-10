#include "expressions.hpp"
#include <cassert>
#include <iostream>
#include <utility>


using namespace AST;

//see header before you insist I use unique pointer
UnaryExpression::~UnaryExpression() { expr.destroy(); }
BinaryExpression::~BinaryExpression() { expr_left.destroy(); expr_right.destroy(); }
CallingExpression::~CallingExpression() { func.destroy(); for (const auto e : parameters) delete e;}
SubscriptExpression::~SubscriptExpression() { arr.destroy(); inside.destroy(); }

void PrintExpressionVisitor::operator()(const UnaryExpression &unary) const noexcept {
  if (isCategoryPREFIX_OPS(unary.opr)) {
    std::cout << operatorToString(unary.opr);
    if (unary.opr == Operator::NOT || unary.opr == Operator::BITNOT)
      std::cout << ' ';
    std::visit(PrintExpressionVisitor{}, *unary.expr);
  } else {
    std::visit(PrintExpressionVisitor{}, *unary.expr);
    std::cout << operatorToString(unary.opr);
  }
}

void PrintExpressionVisitor::operator()(const BinaryExpression &binary) const noexcept {
  std::cout << "(";
  std::visit(PrintExpressionVisitor{}, *binary.expr_left);
  std::cout << ' ' << operatorToString(binary.opr) << ' ';

  std::visit(PrintExpressionVisitor{}, *binary.expr_right);
  std::cout << ")";
}

void PrintExpressionVisitor::operator()(const CallingExpression &calling) const noexcept {
  std::visit(PrintExpressionVisitor{}, *calling.func);
  std::cout << "(";
  for (const auto p : calling.parameters) {
    std::visit(PrintExpressionVisitor{}, *p);
    std::cout << ", ";
  }
  if (!calling.parameters.empty())
    std::cout << "\b\b";

  std::cout << ")";
}

void PrintExpressionVisitor::operator()(const SubscriptExpression &subscript) const noexcept {
  std::visit(PrintExpressionVisitor{}, *subscript.arr);
  std::cout << "[";
  std::visit(PrintExpressionVisitor{}, *subscript.inside);
  std::cout << "]";
}

void PrintExpressionVisitor::operator()(const IdentifierExpression &identifier) const noexcept {
  std::cout << identifier.ident;
}

void PrintExpressionVisitor::operator()(const LiteralExpression &literal) const noexcept {
  switch (literal.type) {
  case LiteralExpression::INT:
    std::cout << literal.getInt();
    return;
  case LiteralExpression::UINT:
    std::cout << literal.getUint();
    return;
  case LiteralExpression::FLOAT:
    std::cout << literal.getFloat();
    return;
  case LiteralExpression::DOUBLE:
    std::cout << literal.getDouble();
    return;

  case LiteralExpression::BOOL:
    if (literal.getBool())
      std::cout << "true";

    std::cout << "false";
    return;
  case LiteralExpression::CHAR:
    std::cout << '\'' << literal.getChar() << '\'';
    return;

  case LiteralExpression::STRING:
    std::cout << '"' << literal.getString() << '"';
    return;

  default:
    assert(false && "invalid literalexpression type");
  }
}

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
  switch (literal.type) {
  case LiteralExpression::INT:
    return std::to_string(literal.getInt());
  case LiteralExpression::FLOAT:
    return std::to_string(static_cast<float>(literal.getFloat()));
  case LiteralExpression::DOUBLE:
    return std::to_string(static_cast<double>(literal.getDouble()));
  case LiteralExpression::BOOL:
    if (literal.getBool())
      return "true";

    return "false";
  case LiteralExpression::CHAR:
    return std::string(1, literal.getChar());

  case LiteralExpression::STRING:
    return literal.getString();

  default:
    assert(false && "invalid literalexpression type");
  }
}