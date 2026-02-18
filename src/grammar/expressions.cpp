#include "expressions.hpp"
#include <cassert>
#include <iostream>
#include <utility>

static void printOperator(const Operator op) {

  static constexpr const char *opToString[]{"BEGIN_BINARY_OPS",
                                            "+",
                                            "-",
                                            "*",
                                            "/",
                                            "^",
                                            "%",
                                            "=",
                                            "<",
                                            ">",
                                            "<=",
                                            ">=",
                                            "and",
                                            "or",
                                            "xor",
                                            "bitand",
                                            "bitor",
                                            "bitxor",
                                            "bitnot",
                                            "eq",
                                            "not_eq",
                                            "END_BINARY_OPS",

                                            "BEGIN_UNARY_OPS",
                                            "++",
                                            "++",
                                            "-",
                                            "--",
                                            "--",
                                            "@",
                                            "NOT",
                                            "END_UNARY_OPS",

                                            "BEGIN_CASTS",
                                            "cast",
                                            "cast_if",
                                            "unsafe_cast",
                                            "END_CASTS"};

  std::cout << opToString[std::to_underlying(op)];
}

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

static bool isPrefix(const Operator op) {
  switch (op) {
  case Operator::PRE_INCREMENT:
  case Operator::PRE_DECREMENT:
  case Operator::CAST:
  case Operator::CAST_IF:
  case Operator::UNSAFE_CAST:
    return true;

  default:
    return false;
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
  if (isPrefix(unary.opr)) {
    printOperator(unary.opr);
    std::visit(PrintExpressionVisitor{}, unary.expr->value);
  } else {
    std::visit(PrintExpressionVisitor{}, unary.expr->value);
    printOperator(unary.opr);
  }
}

void PrintExpressionVisitor::operator()(
    const BinaryExpression &binary) const noexcept {
  std::cout << "(";
  std::visit(PrintExpressionVisitor{}, binary.expr_left->value);

  std::cout << " ";
  printOperator(binary.opr);
  std::cout << " ";

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
    std::cout << static_cast<char>(std::get<int>(literal.value));
    return;

  case LiteralExpression::STRING:
    std::cout << std::get<std::string>(literal.value);
    return;

  default:
    assert(false && "invalid literalexpression type");
  }
}

void PrintExpressionVisitor::operator()(const TemporaryExpr &) const noexcept {
  std::cout << "temporaryexpr";
}