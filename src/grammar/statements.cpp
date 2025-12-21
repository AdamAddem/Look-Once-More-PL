#include "statements.hpp"
#include "expressions.hpp"
#include <iostream>

void VarDeclaration::print(unsigned indent) {

  for (unsigned i{}; i < indent; ++i)
    std::cout << "  ";

  printType(type);
  std::cout << " " << ident << " = ";

  if (expr == nullptr)
    std::cout << "junk";
  else
    expr->print();

  std::cout << ";";
}

void IfStatement::print(unsigned indent) {

  for (unsigned i{}; i < indent; ++i)
    std::cout << "  ";

  std::cout << "if (";
  condition->print();
  std::cout << ") \n";

  true_branch->print(indent + 1);

  if (false_branch) {
    std::cout << "\n else \n";
    false_branch->print(indent + 1);
  }
}

void ForLoop::print(unsigned indent) {

  for (unsigned i{}; i < indent; ++i)
    std::cout << "  ";

  std::cout << "for (";
  var_statement->print();
  std::cout << " ";
  condition->print();
  std::cout << " ";
  iteration->print();

  std::cout << ")\n";
  loop_body->print(indent + 1);
}

void WhileLoop::print(unsigned indent) {

  for (unsigned i{}; i < indent; ++i)
    std::cout << "  ";

  std::cout << "while (";
  condition->print();
  std::cout << ")\n";
  loop_body->print(indent + 1);
}

void ScopedStatement::print(unsigned indent) {

  for (unsigned i{}; i < indent - 1; ++i)
    std::cout << "  ";

  std::cout << "{\n";

  for (auto s : scope_body) {
    s->print(indent);
    std::cout << "\n";
  }

  for (unsigned i{}; i < indent - 1; ++i)
    std::cout << "  ";

  std::cout << "}";
}

void ReturnStatement::print(unsigned indent) {

  for (unsigned i{}; i < indent; ++i)
    std::cout << "  ";
  std::cout << "return ";

  if (return_value)
    return_value->print();
  std::cout << ";";
}

void ExpressionStatement::print(unsigned indent) {

  for (unsigned i{}; i < indent; ++i)
    std::cout << "  ";

  if (expr)
    expr->print();
  std::cout << ";";
}
