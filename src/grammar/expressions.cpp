#include "expressions.hpp"
#include <iostream>
#include <stdexcept>

void printOperator(Operator op) {
  switch (op) {

  case PRE_INCREMENT:
  case POST_INCREMENT:
    std::cout << '+';
  case ADD:
    std::cout << '+';
    return;

  case PRE_DECREMENT:
  case POST_DECREMENT:
    std::cout << '-';
  case SUBTRACT:
  case UNARY_MINUS:
    std::cout << '-';
    return;

  case DIVIDE:
    std::cout << '/';
    return;
  case MULTIPLY:
    std::cout << '*';
    return;
  case POWER:
    std::cout << '^';
    return;
  case MODULUS:
    std::cout << '%';
    return;

  case ASSIGN:
    std::cout << '=';
    return;
  case ADD_ASSIGN:
    std::cout << "+=";
    return;
  case SUB_ASSIGN:
    std::cout << "-=";
    return;
  case MULT_ASSIGN:
    std::cout << "*=";
    return;
  case DIV_ASSIGN:
    std::cout << "/=";
    return;
  case POW_ASSIGN:
    std::cout << "^=";
    return;
  case MOD_ASSIGN:
    std::cout << "%=";
    return;

  case AND:
    std::cout << "and";
    return;
  case OR:
    std::cout << "or";
    return;
  case XOR:
    std::cout << "xor";
    return;
  case NOT:
    std::cout << "not";
    return;

  case BITAND:
    std::cout << "bitand";
    return;
  case BITOR:
    std::cout << "bitor";
    return;
  case BITXOR:
    std::cout << "bitxor";
    return;
  case BITNOT:
    std::cout << "bitnot";
    return;

  case LESS:
    std::cout << '<';
    return;
  case LESS_EQUAL:
    std::cout << "<=";
    return;
  case GREATER:
    std::cout << '>';
    return;
  case GREATER_EQUAL:
    std::cout << ">=";
    return;
  case EQUAL:
    std::cout << "equals";
    return;

  default:
    std::cout << "undefined_op";
    return;
  }
}

bool isLeftAssociative(Operator op) {
  switch (op) {
  case ADDRESS_OF:
  case CAST:
  case CAST_IF:
  case UNSAFE_CAST:
  case VERY_UNSAFE_CAST:
  case NOT:
  case ASSIGN:
  case ADD_ASSIGN:
  case SUB_ASSIGN:
  case MULT_ASSIGN:
  case DIV_ASSIGN:
  case POW_ASSIGN:
  case MOD_ASSIGN:
    return false;

  default:
    return true;
  }
}

bool isPrefix(Operator op) {
  switch (op) {
  case PRE_INCREMENT:
  case PRE_DECREMENT:
  case CAST:
  case CAST_IF:
  case UNSAFE_CAST:
  case VERY_UNSAFE_CAST:
    return true;

  default:
    return false;
  }
}

bool returnsArithmetic(Operator op) {
  switch (op) {
  case ADD:
  case PRE_INCREMENT:
  case POST_INCREMENT:
  case SUBTRACT:
  case UNARY_MINUS:
  case PRE_DECREMENT:
  case POST_DECREMENT:
  case MULTIPLY:
  case DIVIDE:
  case POWER:
  case MODULUS:
  case LESS:
  case GREATER:
  case LESS_EQUAL:
  case GREATER_EQUAL:
  case EQUAL:
  case NOT_EQUAL:
  case AND:
  case OR:
  case XOR:
  case NOT:
  case BITAND:
  case BITOR:
  case BITXOR:
  case BITNOT:
    return true;

  default:
    return false;
  }
}

void PrintExpressionVisitor::operator()(const UnaryExpression &unary) noexcept {

  if (isPrefix(unary.opr)) {
    printOperator(unary.opr);
    std::visit(PrintExpressionVisitor{}, unary.expr->value);
  } else {
    std::visit(PrintExpressionVisitor{}, unary.expr->value);
    printOperator(unary.opr);
  }
}

void PrintExpressionVisitor::operator()(
    const BinaryExpression &binary) noexcept {
  std::cout << "(";
  std::visit(PrintExpressionVisitor{}, binary.expr_left->value);

  std::cout << " ";
  printOperator(binary.opr);
  std::cout << " ";

  std::visit(PrintExpressionVisitor{}, binary.expr_right->value);
  std::cout << ")";
}

void PrintExpressionVisitor::operator()(
    const CallingExpression &calling) noexcept {
  std::visit(PrintExpressionVisitor{}, calling.func->value);
  std::cout << "(";
  for (auto p : calling.parameters) {
    std::visit(PrintExpressionVisitor{}, p->value);
    std::cout << ", ";
  }
  if (!calling.parameters.empty())
    std::cout << "\b\b";

  std::cout << ")";
}

void PrintExpressionVisitor::operator()(
    const SubscriptExpression &subscript) noexcept {
  std::visit(PrintExpressionVisitor{}, subscript.arr->value);
  std::cout << "[";
  std::visit(PrintExpressionVisitor{}, subscript.inside->value);
  std::cout << "]";
}

void PrintExpressionVisitor::operator()(
    const IdentifierExpression &identifier) noexcept {
  std::cout << identifier.ident;
}
void PrintExpressionVisitor::operator()(
    const LiteralExpression &literal) noexcept {

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
    std::cout << static_cast<char>(std::get<int>(literal.value));
    return;

  case LiteralExpression::STRING:
    std::cout << std::get<std::string>(literal.value);
    return;

  default:
    throw std::runtime_error("invalid type in LiteralExpression");
  }
}

void PrintExpressionVisitor::operator()(const TemporaryExpr &) noexcept {
  std::cout << "temporaryexpr";
}
