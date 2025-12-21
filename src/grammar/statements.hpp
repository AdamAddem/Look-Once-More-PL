#pragma once
#include "types.hpp"
#include <string>
#include <vector>

struct Expression;

struct Statement {
  virtual ~Statement() = default;
  virtual void print(unsigned indent = 0) = 0;
};

struct VarDeclaration : Statement {

  Type type;
  std::string ident;
  Expression *expr; // nullptr indicates junk initialization

  VarDeclaration(Type &&_type, std::string &&_ident,
                 Expression *_expr = nullptr)
      : type(std::move(_type)), ident(std::move(_ident)), expr(_expr) {}

  VarDeclaration(VarDeclaration &&other) noexcept
      : type(std::move(other.type)), ident(std::move(other.ident)) {}

  virtual void print(unsigned indent = 0) override;
};

struct IfStatement : Statement {
  Expression *condition;
  Statement *true_branch;
  Statement *false_branch; // nullptr indicates no false branch

  IfStatement(Expression *_condition, Statement *_true_branch,
              Statement *_false_branch = nullptr)
      : condition(_condition), true_branch(_true_branch),
        false_branch(_false_branch) {}

  virtual void print(unsigned indent = 0) override;
};

struct ForLoop : Statement {
  Statement *var_statement;
  Statement *condition;
  Expression *iteration;

  Statement *loop_body;

  ForLoop(Statement *_decl, Statement *_condition, Expression *_iteration,
          Statement *_loop_body)
      : var_statement(_decl), condition(_condition), iteration(_iteration),
        loop_body(_loop_body) {}

  virtual void print(unsigned indent = 0) override;
};

struct WhileLoop : Statement {
  Expression *condition;
  Statement *loop_body;
  WhileLoop(Expression *_condition, Statement *_loop_body)
      : condition(_condition), loop_body(_loop_body) {}

  virtual void print(unsigned indent = 0) override;
};

/*
struct DoWhileLoop : Statement {
  Expression* condition;
  Statement* loop_body;

  DoWhileLoop()

  virtual void print() override;
}; */

struct ScopedStatement : Statement {
  std::vector<Statement *> scope_body;

  explicit ScopedStatement(std::vector<Statement *> &&_body)
      : scope_body(std::move(_body)) {}

  virtual void print(unsigned indent = 0) override;
};

struct ReturnStatement : Statement {
  Expression *return_value; // may be nullptr
  explicit ReturnStatement(Expression *_return_value = nullptr)
      : return_value(_return_value) {}

  virtual void print(unsigned indent = 0) override;
};

struct SwitchCase : Statement {}; // TBD

struct ExpressionStatement : Statement {
  Expression *expr; // may be nullptr
  ExpressionStatement(Expression *_expr = nullptr) : expr(_expr) {}

  virtual void print(unsigned indent = 0) override;
};
