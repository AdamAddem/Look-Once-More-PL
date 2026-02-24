#pragma once
#include "types.hpp"
#include <string>
#include <variant>
#include <vector>

struct Expression;
struct Statement;

struct VarDeclaration {
  Type type;
  std::string ident;
  Expression *expr; // nullptr indicates junk initialization
  unsigned line_number;

  VarDeclaration(Type &&_type, std::string &&_ident, const unsigned line_num,
                 Expression *_expr = nullptr)
      : type(std::move(_type)), ident(std::move(_ident)), expr(_expr), line_number(line_num){}

  VarDeclaration(VarDeclaration &&other) noexcept
      : type(std::move(other.type)), ident(std::move(other.ident)),
        expr(other.expr), line_number(other.line_number) {
    other.expr = nullptr;
  }
};

struct IfStatement {
  Expression *condition;
  Statement *true_branch;
  Statement *false_branch; // may be nullptr
  unsigned line_number;

  IfStatement(Expression *_condition, Statement *_true_branch, unsigned line_num,
              Statement *_false_branch = nullptr)
      : condition(_condition), true_branch(_true_branch),
        false_branch(_false_branch), line_number(line_num) {}
};

struct ForLoop {
  Statement *var_statement; //may be nullptr
  Expression *condition; // may be nullptr
  Expression *iteration; // may be nullptr
  Statement *loop_body;

  unsigned line_number;

  ForLoop(Statement *_decl, Expression *_condition, Expression *_iteration,
        Statement *_loop_body, const unsigned line_num)
      : var_statement(_decl), condition(_condition), iteration(_iteration),
        loop_body(_loop_body), line_number(line_num) {}
};

struct WhileLoop {
  Expression *condition;
  Statement *loop_body;
  unsigned line_number;

  WhileLoop(Expression *_condition, Statement *_loop_body, const unsigned line_num)
      : condition(_condition), loop_body(_loop_body), line_number(line_num) {}
};

struct ScopedStatement {
  std::vector<Statement *> scope_body;
  unsigned line_number;

  ScopedStatement(std::vector<Statement *> &&_body, const unsigned line_num)
      : scope_body(std::move(_body)), line_number(line_num) {}
};

struct ReturnStatement {
  Expression *return_value; // may be nullptr
  unsigned line_number;
  explicit ReturnStatement(const unsigned line_num, Expression *_return_value = nullptr)
      : return_value(_return_value), line_number(line_num) {}
};

struct ExpressionStatement {
  Expression *expr; // may be nullptr
  unsigned line_number;
  explicit ExpressionStatement(const unsigned line_num, Expression *_expr = nullptr) : expr(_expr), line_number(line_num) {}
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