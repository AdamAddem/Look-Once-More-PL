#include "expressions.hpp"
#include <iostream>

void UnaryExpression::print() { expr->print(); }
void BinaryExpression::print() {
  std::cout << "(";
  expr_left->print();
  std::cout << " " << opr.toString() << " ";
  expr_right->print();
  std::cout << ")";
}
void CallingExpression::print() {
  func->print();
  std::cout << "(";
  for (auto p : parameters) {
    p->print();
    std::cout << ", ";
  }

  std::cout << ")";
}
void SubscriptExpression::print() {
  arr->print();
  std::cout << "[";
  inside->print();
  std::cout << "]";
}

void IdentifierExpression::print() { std::cout << ident; }
void LiteralExpression::print() { std::cout << literal.toString(); }
void TemporaryExpr::print() { std::cout << "temporaryexpr"; }
