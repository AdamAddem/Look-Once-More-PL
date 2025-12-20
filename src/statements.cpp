#include "statements.hpp"
#include <iostream>

void VarDeclaration::print() {
  std::string type_name;
  if (std::holds_alternative<StrictType>(type))
    type_name = std::get<StrictType>(type).type_name;
  else
    type_name = std::get<VariantType>(type).type_name;

  std::cout << type_name << " " << ident << " = ";

  if (expr == nullptr)
    std::cout << "junk";
  else
    expr->print();

  std::cout << ";";
}

void IfStatement::print() {
  std::cout << "if (";
  condition->print();
  std::cout << ") \n";
  true_branch->print();

  if (false_branch) {
    std::cout << "\n else \n";
    false_branch->print();
  }
}

void ForLoop::print() {
  std::cout << "for (";
  var_statement->print();
  std::cout << " ";
  condition->print();
  std::cout << " ";
  iteration->print();

  std::cout << ")\n";
  loop_body->print();
}

void WhileLoop::print() {
  std::cout << "while (";
  condition->print();
  std::cout << ")\n";
  loop_body->print();
}

void ScopedStatement::print() {
  std::cout << "{\n";
  for (auto s : scope_body) {
    s->print();
    std::cout << "\n";
  }
  std::cout << "}";
}

void ReturnStatement::print() {
  std::cout << "return ";
  if (return_value)
    return_value->print();
  std::cout << ";";
}

void ExpressionStatement::print() {
  if (expr)
    expr->print();
  std::cout << ";";
}
