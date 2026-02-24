#include "expressions.hpp"
#include <cassert>
#include <iostream>
#include <utility>


[[maybe_unused]] static bool isLeftAssociative(const Operator op) {
  switch (op) {
  case Operator::ADDRESS_OF:
  case Operator::CAST:
  case Operator::CAST_IF:
  case Operator::UNSAFE_CAST:
  case Operator::NOT:
  case Operator::ASSIGN:
    return false;

  default:
    return true;
  }
}


[[maybe_unused]] static bool returnsArithmetic(const Operator op) {
  switch (op) {
  case Operator::ADD:
  case Operator::PRE_INCREMENT:
  case Operator::POST_INCREMENT:
  case Operator::SUBTRACT:
  case Operator::UNARY_MINUS:
  case Operator::PRE_DECREMENT:
  case Operator::POST_DECREMENT:
  case Operator::MULTIPLY:
  case Operator::DIVIDE:
  case Operator::POWER:
  case Operator::MODULUS:
  case Operator::LESS:
  case Operator::GREATER:
  case Operator::LESS_EQUAL:
  case Operator::GREATER_EQUAL:
  case Operator::EQUAL:
  case Operator::NOT_EQUAL:
  case Operator::AND:
  case Operator::OR:
  case Operator::XOR:
  case Operator::NOT:
  case Operator::BITAND:
  case Operator::BITOR:
  case Operator::BITXOR:
  case Operator::BITNOT:
    return true;

  default:
    return false;
  }
}

void PrintExpressionVisitor::operator()(
    const UnaryExpression &unary) const noexcept {
  if (isCategoryPREFIX_OPS(unary.opr)) {
    std::cout << operatorToString(unary.opr);
    if (unary.opr == Operator::NOT)
      std::cout << ' ';
    std::visit(PrintExpressionVisitor{}, unary.expr->value);
  } else {
    std::visit(PrintExpressionVisitor{}, unary.expr->value);
    std::cout << operatorToString(unary.opr);
  }
}

void PrintExpressionVisitor::operator()(
    const BinaryExpression &binary) const noexcept {
  std::cout << "(";
  std::visit(PrintExpressionVisitor{}, binary.expr_left->value);

  std::cout << ' ' << operatorToString(binary.opr) << ' ';

  std::visit(PrintExpressionVisitor{}, binary.expr_right->value);
  std::cout << ")";
}

void PrintExpressionVisitor::operator()(
    const CallingExpression &calling) const noexcept {
  std::visit(PrintExpressionVisitor{}, calling.func->value);
  std::cout << "(";
  for (const auto p : calling.parameters) {
    std::visit(PrintExpressionVisitor{}, p->value);
    std::cout << ", ";
  }
  if (!calling.parameters.empty())
    std::cout << "\b\b";

  std::cout << ")";
}

void PrintExpressionVisitor::operator()(
    const SubscriptExpression &subscript) const noexcept {
  std::visit(PrintExpressionVisitor{}, subscript.arr->value);
  std::cout << "[";
  std::visit(PrintExpressionVisitor{}, subscript.inside->value);
  std::cout << "]";
}

void PrintExpressionVisitor::operator()(
    const IdentifierExpression &identifier) const noexcept {
  std::cout << identifier.ident;
}

void PrintExpressionVisitor::operator()(
    const LiteralExpression &literal) const noexcept {
  switch (literal.type) {
  case LiteralExpression::INT:
    std::cout << std::get<int>(literal.value);
    return;
  case LiteralExpression::FLOAT:
    std::cout << std::get<float>(literal.value);
    return;
  case LiteralExpression::DOUBLE:
    std::cout << std::get<double>(literal.value);
    return;

  case LiteralExpression::BOOL:
    if (std::get<int>(literal.value) == 1)
      std::cout << "true";
    else
      std::cout << "false";
    return;
  case LiteralExpression::CHAR:
    std::cout << '\'' << static_cast<char>(std::get<int>(literal.value)) << '\'';
    return;

  case LiteralExpression::STRING:
    std::cout << '"' << std::get<std::string>(literal.value) << '"';
    return;

  default:
    assert(false && "invalid literalexpression type");
  }
}

void PrintExpressionVisitor::operator()(const TemporaryExpr &) const noexcept {
  std::cout << "temporaryexpr";
}

std::string ExpressionToStringVisitor::operator()(
    const UnaryExpression &unary) const noexcept {
  std::string retval;
  if (isCategoryPREFIX_OPS(unary.opr)) {
    retval.append(operatorToString(unary.opr));
    if (unary.opr == Operator::NOT)
      retval.push_back(' ');
    retval.append(std::visit(ExpressionToStringVisitor{}, unary.expr->value));
  } else {
    retval.append(std::visit(ExpressionToStringVisitor{}, unary.expr->value));
    retval.append(operatorToString(unary.opr));
  }

  return retval;
}

std::string ExpressionToStringVisitor::operator()(
    const BinaryExpression &binary) const noexcept {
  std::string retval;
  retval.push_back('(');
  retval.append(std::visit(ExpressionToStringVisitor{}, binary.expr_left->value));

  retval.push_back(' ');
  retval.append(operatorToString(binary.opr));
  retval.push_back(' ');

  retval.append(std::visit(ExpressionToStringVisitor{}, binary.expr_right->value));
  retval.push_back(')');

  return retval;
}

std::string ExpressionToStringVisitor::operator()(
    const CallingExpression &calling) const noexcept {
  std::string retval = std::visit(ExpressionToStringVisitor{}, calling.func->value);
  retval.push_back('(');
  for (const auto p : calling.parameters) {
    retval.append(std::visit(ExpressionToStringVisitor{}, p->value));
    retval.append(", ");
  }
  if (!calling.parameters.empty()) {
   retval.pop_back();
   retval.pop_back();
  }

  retval.push_back(')');
  return retval;
}

std::string ExpressionToStringVisitor::operator()(
    const SubscriptExpression &subscript) const noexcept {
  std::string retval = std::visit(ExpressionToStringVisitor{}, subscript.arr->value);
  retval.push_back('[');
  retval.append(std::visit(ExpressionToStringVisitor{}, subscript.inside->value));
  retval.push_back(']');
  return retval;
}

std::string ExpressionToStringVisitor::operator()(
    const IdentifierExpression &identifier) const noexcept {
  return identifier.ident;
}

std::string ExpressionToStringVisitor::operator()(
    const LiteralExpression &literal) const noexcept {
  switch (literal.type) {
  case LiteralExpression::INT:
    return std::to_string(std::get<int>(literal.value));
  case LiteralExpression::FLOAT:
    return std::to_string(std::get<float>(literal.value));
  case LiteralExpression::DOUBLE:
    return std::to_string(std::get<double>(literal.value));
  case LiteralExpression::BOOL:
    if (std::get<int>(literal.value) == 1)
      return "true";

    return "false";
  case LiteralExpression::CHAR:
    return std::string(1, static_cast<char>(std::get<int>(literal.value)));

  case LiteralExpression::STRING:
    return std::get<std::string>(literal.value);

  default:
    assert(false && "invalid literalexpression type");
  }
}

std::string ExpressionToStringVisitor::operator()(const TemporaryExpr &) const noexcept {
  return "temporaryexpr";
}