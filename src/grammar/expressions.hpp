#pragma once
#include <string>
#include <variant>
#include <vector>

enum class Operator {

	/* Binary Ops */
  ADD,
  SUBTRACT,
  MULTIPLY,
  DIVIDE,
  POWER,
  MODULUS,
	ASSIGN,
	LESS,
	GREATER,
	LESS_EQUAL,
	GREATER_EQUAL,
	AND,
	OR,
	XOR,
	BITAND,
	BITOR,
	BITXOR,
	BITNOT,
	EQUAL,
	NOT_EQUAL,
	/* Binary Ops */


	/* Unary Ops */
  PRE_INCREMENT,
  POST_INCREMENT,
  UNARY_MINUS,
  PRE_DECREMENT,
  POST_DECREMENT,
	ADDRESS_OF,
	NOT,
	/* Unary Ops */

  CAST,
  CAST_IF,
  UNSAFE_CAST,
  VERY_UNSAFE_CAST,

};

struct Expression;

struct UnaryExpression {
  Expression *expr;
  Operator opr;
  UnaryExpression(Expression *_expr, const Operator _opr) : expr(_expr), opr(_opr) {}
};

struct BinaryExpression {
  Expression *expr_left;
  Expression *expr_right;
  Operator opr;

  BinaryExpression(Expression *_expr_left, Expression *_expr_right,
                   const Operator _opr)
      : expr_left(_expr_left), expr_right(_expr_right), opr(_opr) {}
};

struct CallingExpression {
  Expression *func;
  std::vector<Expression *> parameters;

  CallingExpression(Expression *_f, std::vector<Expression *> &&_params)
      : func(_f), parameters(std::move(_params)) {}
};

struct SubscriptExpression {
  Expression *arr;
  Expression *inside;

  SubscriptExpression(Expression *_arr, Expression *_inside)
      : arr(_arr), inside(_inside) {}
};

struct IdentifierExpression {
  std::string ident;

  explicit IdentifierExpression(std::string &&_ident)
      : ident(std::move(_ident)) {}
};

struct LiteralExpression {
  using LiteralValue = std::variant<int, float, double, std::string>;
  enum LiteralType { INT, FLOAT, DOUBLE, BOOL, CHAR, STRING };
  LiteralValue value;
  LiteralType type;

  explicit LiteralExpression(LiteralValue &&_v, const LiteralType _t)
      : value(std::move(_v)), type(_t) {}
};

struct TemporaryExpr { // finish
};

struct Expression {
  std::variant<UnaryExpression, BinaryExpression, CallingExpression,
               SubscriptExpression, IdentifierExpression, LiteralExpression,
               TemporaryExpr>
      value;
};

struct PrintExpressionVisitor {
  void operator()(const UnaryExpression &) const noexcept;
  void operator()(const BinaryExpression &) const noexcept;
  void operator()(const CallingExpression &) const noexcept;
  void operator()(const SubscriptExpression &) const noexcept;
  void operator()(const IdentifierExpression &) const noexcept;
  void operator()(const LiteralExpression &) const noexcept;
  void operator()(const TemporaryExpr &) const noexcept;
};
