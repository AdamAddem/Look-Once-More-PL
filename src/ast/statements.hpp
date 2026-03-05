#pragma once
#include "types.hpp"
#include <memory>
#include <string>
#include <variant>
#include <vector>

// Note: Each AST Node holds ownership over the expressions and statements it has pointers to
// The reason I don't use unique pointer is twofold
// 1: They are ugly and syntactically bulky

// And more importantly,
// 2: Since I'm forward declaring the expression and statement types,
//    unique pointer throws a fit unless the AST Node defines the destructor, which defeats the whole purpose of unique pointer.
//    In addition, it forces the constructors to be defined in a cpp file, which prevents many of these very trivial constructors from being inlined.
//    It also prevents many of the optional pointers from having a default parameter. (You'll catch me dead before I use std::optional<std::unique_ptr<Expression>>)

namespace AST {

struct Expression;
struct Statement;


struct VarDeclaration {
  Type type;
  std::string ident;
  Expression* expr; //owned, may be null
  unsigned line_number;

  //takes ownership of sub_expression
  VarDeclaration(Type &&_type, std::string &&_ident, const unsigned line_num, Expression *sub_expression = nullptr) noexcept
  : type(std::move(_type)), ident(std::move(_ident)), expr(sub_expression), line_number(line_num){}

  VarDeclaration(VarDeclaration &&other) noexcept
  : type(std::move(other.type)), ident(std::move(other.ident)),
    expr(other.expr), line_number(other.line_number) { other.expr = nullptr; }

  ~VarDeclaration();
};

struct IfStatement {
  Expression *condition; //owned
  Statement *true_branch; //owned
  Statement *false_branch; //owned, may be null
  unsigned line_number;

  //takes ownership of pointers
  IfStatement(Expression *_condition, Statement *_true_branch, const unsigned line_num, Statement *_false_branch = nullptr)
  : condition(_condition), true_branch(_true_branch), false_branch(_false_branch), line_number(line_num) {}

  IfStatement(IfStatement&& other) noexcept
  : condition(other.condition), true_branch(other.true_branch),
  false_branch(other.false_branch), line_number(other.line_number) {
    other.condition = nullptr;
    other.true_branch = nullptr;
    other.false_branch = nullptr;
  }

  ~IfStatement();
};

struct ForLoop {
  Statement *var_statement; //owned, may be null
  Expression *condition; //owned, may be null
  Expression *iteration; //owned, may be null
  Statement *loop_body; //owned
  unsigned line_number;

  //takes ownership of pointers
  ForLoop(Statement *_decl, Expression *_condition, Expression *_iteration, Statement *_loop_body, const unsigned line_num)
  : var_statement(_decl), condition(_condition), iteration(_iteration), loop_body(_loop_body), line_number(line_num) {}

  ForLoop(ForLoop&& other) noexcept
  : var_statement(other.var_statement), condition(other.condition),
  iteration(other.iteration), loop_body(other.loop_body), line_number(other.line_number) {
    other.var_statement = nullptr;
    other.condition = nullptr;
    other.iteration = nullptr;
    other.loop_body = nullptr;
  }

  ~ForLoop();
};

struct WhileLoop {
  Expression *condition; //owned
  Statement *loop_body; //owned
  unsigned line_number;

  //takes ownership of pointers
  WhileLoop(Expression *_condition, Statement *_loop_body, const unsigned line_num)
  : condition(_condition), loop_body(_loop_body), line_number(line_num) {}
  WhileLoop(WhileLoop&& other) noexcept : condition(other.condition), loop_body(other.loop_body), line_number(other.line_number)
  {other.condition = nullptr; other.loop_body = nullptr;}

  ~WhileLoop();
};

struct ScopedStatement {
  std::vector<Statement *> scope_body;
  unsigned line_number;

  ScopedStatement(std::vector<Statement *> &&_body, const unsigned line_num)
  : scope_body(std::move(_body)), line_number(line_num) {}

  ScopedStatement(ScopedStatement&& other) noexcept : scope_body(std::move(other.scope_body)), line_number(other.line_number) {
    for (auto& s : other.scope_body)
      s = nullptr;
  }
  ~ScopedStatement();
};

struct ReturnStatement {
  Expression *return_value; //owned, may be null
  unsigned line_number;

  //takes ownership of pointer
  explicit ReturnStatement(const unsigned line_num, Expression *_return_value = nullptr)
  : return_value(_return_value), line_number(line_num) {}

  ReturnStatement(ReturnStatement&& other) noexcept
  : return_value(other.return_value), line_number(other.line_number) {other.return_value = nullptr;}

  ~ReturnStatement();
};

struct ExpressionStatement {
  Expression *expr; //owned, may be nullptr
  unsigned line_number;

  //takes ownership of pointer
  explicit ExpressionStatement(const unsigned line_num, Expression *_expr = nullptr) : expr(_expr), line_number(line_num) {}
  ExpressionStatement(ExpressionStatement&& other) noexcept : expr(other.expr), line_number(other.line_number) {other.expr = nullptr;}
  ~ExpressionStatement();
};

struct Statement {
  using StatementTypes = std::variant<ExpressionStatement, ReturnStatement, ScopedStatement, WhileLoop, ForLoop, IfStatement, VarDeclaration>;

  explicit Statement(StatementTypes&& t) noexcept : value(std::move(t)) { }
  StatementTypes value;
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