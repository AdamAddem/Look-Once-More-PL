#pragma once
#include <string>
#include <variant>
#include <vector>

enum Operator {

  ADD,
  PRE_INCREMENT,
  POST_INCREMENT,

  SUBTRACT,
  UNARY_MINUS,
  PRE_DECREMENT,
  POST_DECREMENT,

  MULTIPLY,
  DIVIDE,
  POWER,
  MODULUS,

  ASSIGN,
  ADD_ASSIGN,
  SUB_ASSIGN,
  MULT_ASSIGN,
  DIV_ASSIGN,
  POW_ASSIGN,
  MOD_ASSIGN,

  LESS,
  GREATER,
  LESS_EQUAL,
  GREATER_EQUAL,
  EQUAL,
  NOT_EQUAL,

  AND,
  OR,
  XOR,
  NOT,

  BITAND,
  BITOR,
  BITXOR,
  BITNOT,

  CAST,
  CAST_IF,
  UNSAFE_CAST,
  VERY_UNSAFE_CAST,

  ADDRESS_OF,
};

struct Expression {
  virtual ~Expression() = default;
  virtual void print() = 0;
};

struct UnaryExpression : Expression {
  Expression *expr;
  Operator opr;

  UnaryExpression(Expression *_expr, Operator _opr) : expr(_expr), opr(_opr) {}

  virtual void print() override;
};

struct BinaryExpression : Expression {
  Expression *expr_left;
  Expression *expr_right;
  Operator opr;

  BinaryExpression(Expression *_expr_left, Expression *_expr_right,
                   Operator _opr)
      : expr_left(_expr_left), expr_right(_expr_right), opr(_opr) {}

  virtual void print() override;
};

struct CallingExpression : Expression {
  Expression *func;
  std::vector<Expression *> parameters;

  CallingExpression(Expression *_f, std::vector<Expression *> &&_params)
      : func(_f), parameters(std::move(_params)) {}

  virtual void print() override;
};

struct SubscriptExpression : Expression {
  Expression *arr;
  Expression *inside;

  SubscriptExpression(Expression *_arr, Expression *_inside)
      : arr(_arr), inside(_inside) {};

  virtual void print() override;
};

struct IdentifierExpression : Expression {
  std::string ident;

  explicit IdentifierExpression(std::string &&_ident)
      : ident(std::move(_ident)) {}

  virtual void print() override;
};

struct LiteralExpression : Expression {
  using LiteralValue = std::variant<int, float, double, std::string>;
  enum LiteralType { INT, FLOAT, DOUBLE, BOOL, CHAR, STRING };
  LiteralValue value;
  LiteralType type;

  explicit LiteralExpression(LiteralValue &&_v, LiteralType _t)
      : value(std::move(_v)), type(_t) {}

  virtual void print() override;
};

struct TemporaryExpr : Expression { // finish
  virtual void print() override;
};
