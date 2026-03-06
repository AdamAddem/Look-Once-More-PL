#pragma once
#include "expressions.hpp"
#include "types.hpp"
#include <memory>
#include <string>
#include <variant>
#include <vector>

namespace AST {

struct ExpressionStatement;
struct ReturnStatement;
struct ScopedStatement;
struct WhileLoop;
struct ForLoop;
struct IfStatement;
struct VarDeclaration;

using Statement = std::variant<ExpressionStatement, ReturnStatement, ScopedStatement, WhileLoop, ForLoop, IfStatement, VarDeclaration>;

struct VarDeclaration {
  Type type;
  std::string ident;
  nullable_owned_ptr<Expression> expr;
  unsigned line_number;

  //takes ownership of sub_expression
  VarDeclaration(Type &&_type, std::string &&_ident, const unsigned line_num, nullable_owned_ptr<Expression> sub_expression = nullptr ) noexcept
  : type(std::move(_type)), ident(std::move(_ident)), expr(std::move(sub_expression)), line_number(line_num){}

  VarDeclaration(VarDeclaration &&other) noexcept
  : type(std::move(other.type)), ident(std::move(other.ident)),
    expr(std::move(other.expr)), line_number(other.line_number) {}

  ~VarDeclaration();
};

struct IfStatement {
  owned_ptr<Expression> condition;
  owned_ptr<Statement> true_branch;
  nullable_owned_ptr<Statement> false_branch;
  unsigned line_number;

  //takes ownership of pointers
  IfStatement(owned_ptr<Expression> _condition, owned_ptr<Statement> _true_branch, const unsigned line_num, nullable_owned_ptr<Statement> _false_branch = nullptr)
  : condition(std::move(_condition)), true_branch(std::move(_true_branch)), false_branch(std::move(_false_branch)), line_number(line_num) {}

  ~IfStatement();
};

struct ForLoop {
  nullable_owned_ptr<Statement> var_statement;
  nullable_owned_ptr<Expression> condition;
  nullable_owned_ptr<Expression> iteration;
  owned_ptr<Statement> loop_body;
  unsigned line_number;

  ForLoop(nullable_owned_ptr<Statement> _decl, nullable_owned_ptr<Expression> _condition, nullable_owned_ptr<Expression> _iteration, owned_ptr<Statement> _loop_body, const unsigned line_num)
  : var_statement(std::move(_decl)), condition(std::move(_condition)), iteration(std::move(_iteration)), loop_body(std::move(_loop_body)), line_number(line_num) {}

  ~ForLoop();
};

struct WhileLoop {
  owned_ptr<Expression> condition;
  owned_ptr<Statement> loop_body;
  unsigned line_number;

  //takes ownership of pointers
  WhileLoop(owned_ptr<Expression> _condition, owned_ptr<Statement> _loop_body, const unsigned line_num)
  : condition(std::move(_condition)), loop_body(std::move(_loop_body)), line_number(line_num) {}
  ~WhileLoop();
};

struct ScopedStatement {
  std::vector<Statement*> scope_body;
  unsigned line_number;

  ScopedStatement(std::vector<Statement*> &&_body, const unsigned line_num)
  : scope_body(std::move(_body)), line_number(line_num) {}

  ~ScopedStatement();
};

struct ReturnStatement {
  nullable_owned_ptr<Expression> return_value;
  unsigned line_number;

  explicit ReturnStatement(const unsigned line_num, nullable_owned_ptr<Expression> _return_value = nullptr)
  : return_value(std::move(_return_value)), line_number(line_num) {}

  ~ReturnStatement();
};

struct ExpressionStatement {
  nullable_owned_ptr<Expression> expr; //owned, may be nullptr
  unsigned line_number;

  explicit ExpressionStatement(const unsigned line_num, nullable_owned_ptr<Expression> _expr = nullptr) : expr(std::move(_expr)), line_number(line_num) {}
  ~ExpressionStatement();
};

struct PrintStatementVisitor {
  unsigned indent{};

  void operator()(const ExpressionStatement &) const noexcept;
  void operator()(const ReturnStatement &) const noexcept;
  void operator()(const ScopedStatement &) const noexcept;
  void operator()(const WhileLoop &) const noexcept;
  void operator()(const ForLoop &) const noexcept;
  void operator()(const IfStatement &) const noexcept;
  void operator()(const VarDeclaration &) const noexcept;
};

}