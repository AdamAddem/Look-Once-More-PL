#include "statements.hpp"
#include "expressions.hpp"
#include <iostream>

void PrintStatementVisitor::operator()(
    const ExpressionStatement &stmt) const noexcept {
  for (unsigned i{}; i < indent; ++i)
    std::cout << "  ";

  if (stmt.expr)
    std::visit(PrintExpressionVisitor{}, stmt.expr->value);
  std::cout << ";";
}

void PrintStatementVisitor::operator()(
    const ReturnStatement &stmt) const noexcept {
  for (unsigned i{}; i < indent; ++i)
    std::cout << "  ";
  std::cout << "return ";

  if (stmt.return_value)
    std::visit(PrintExpressionVisitor{}, stmt.return_value->value);
  std::cout << ";";
}

void PrintStatementVisitor::operator()(
    const ScopedStatement &stmt) const noexcept {
  for (unsigned i{}; i < indent; ++i)
    std::cout << "  ";

  std::cout << "{\n";

  for (auto s : stmt.scope_body) {
    std::visit(PrintStatementVisitor{indent + 1}, s->value);
    std::cout << "\n";
  }

  for (unsigned i{}; i < indent; ++i)
    std::cout << "  ";

  std::cout << "}";
}

void PrintStatementVisitor::operator()(const WhileLoop &stmt) const noexcept {
  for (unsigned i{}; i < indent; ++i)
    std::cout << "  ";

  std::cout << "while (";
  std::visit(PrintExpressionVisitor{}, stmt.condition->value);
  std::cout << ")\n";
  std::visit(PrintStatementVisitor{indent + 1}, stmt.loop_body->value);
}

void PrintStatementVisitor::operator()(const ForLoop &stmt) const noexcept {
  for (unsigned i{}; i < indent; ++i)
    std::cout << "  ";

  std::cout << "for (";
  std::visit(PrintStatementVisitor{}, stmt.var_statement->value);
  std::cout << " ";
  if (stmt.condition)
    std::visit(PrintExpressionVisitor{}, stmt.condition->value);
  std::cout << "; ";
  if (stmt.iteration)
    std::visit(PrintExpressionVisitor{}, stmt.iteration->value);

  std::cout << ")\n";
  std::visit(PrintStatementVisitor{indent}, stmt.loop_body->value);
}

void PrintStatementVisitor::operator()(const IfStatement &stmt) const noexcept {
  for (unsigned i{}; i < indent; ++i)
    std::cout << "  ";

  std::cout << "if (";
  std::visit(PrintExpressionVisitor{}, stmt.condition->value);
  std::cout << ") \n";

  std::visit(PrintStatementVisitor{indent + 1}, stmt.true_branch->value);

  if (stmt.false_branch) {
    std::cout << "\n else \n";
    std::visit(PrintStatementVisitor{indent + 1}, stmt.false_branch->value);
  }
}

void PrintStatementVisitor::operator()(
    const VarDeclaration &stmt) const noexcept {
  for (unsigned i{}; i < indent; ++i)
    std::cout << "  ";

  printType(stmt.type);
  std::cout << " " << stmt.ident << " = ";

  if (stmt.expr == nullptr)
    std::cout << "junk";
  else
    std::visit(PrintExpressionVisitor{}, stmt.expr->value);

  std::cout << ";";
}