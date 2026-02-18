#pragma once
#include "types.hpp"
#include <string>
#include <variant>
#include <vector>

struct Expression;
struct Statement;

struct VarDeclaration {
  Types type;
  std::string ident;
  Expression *expr; // nullptr indicates junk initialization

  VarDeclaration(Types &&_type, std::string &&_ident,
                 Expression *_expr = nullptr)
      : type(std::move(_type)), ident(std::move(_ident)), expr(_expr) {}

  VarDeclaration(VarDeclaration &&other) noexcept
      : type(std::move(other.type)), ident(std::move(other.ident)),
        expr(other.expr) {
    other.expr = nullptr;
  }
};

struct IfStatement {
  Expression *condition;
  Statement *true_branch;
  Statement *false_branch; // may be nullptr

  IfStatement(Expression *_condition, Statement *_true_branch,
              Statement *_false_branch = nullptr)
      : condition(_condition), true_branch(_true_branch),
        false_branch(_false_branch) {}
};

struct ForLoop {
  Statement *var_statement;
  Expression *condition; // may be nullptr
  Expression *iteration; // may be nullptr

  Statement *loop_body;

  ForLoop(Statement *_decl, Expression *_condition, Expression *_iteration,
          Statement *_loop_body)
      : var_statement(_decl), condition(_condition), iteration(_iteration),
        loop_body(_loop_body) {}
};

struct WhileLoop {
  Expression *condition;
  Statement *loop_body;

  WhileLoop(Expression *_condition, Statement *_loop_body)
      : condition(_condition), loop_body(_loop_body) {}
};

struct ScopedStatement {
  std::vector<Statement *> scope_body;

  explicit ScopedStatement(std::vector<Statement *> &&_body)
      : scope_body(std::move(_body)) {}
};

struct ReturnStatement {
  Expression *return_value; // may be nullptr
  explicit ReturnStatement(Expression *_return_value = nullptr)
      : return_value(_return_value) {}
};

struct ExpressionStatement {
  Expression *expr; // may be nullptr
  explicit ExpressionStatement(Expression *_expr = nullptr) : expr(_expr) {}
};

struct Statement {
  std::variant<ExpressionStatement, ReturnStatement, ScopedStatement, WhileLoop,
               ForLoop, IfStatement, VarDeclaration>
      value;
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