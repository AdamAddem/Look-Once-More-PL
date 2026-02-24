#pragma once
#include <string>
#include <variant>
#include <vector>
#include <unordered_map>
#include <utility>



enum class Operator : unsigned {
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
  CAST,
  CAST_IF,
  UNSAFE_CAST,
  PRE_INCREMENT,
  PRE_DECREMENT,
  UNARY_MINUS,
  ADDRESS_OF,
  NOT,
  POST_INCREMENT,
  POST_DECREMENT,
};


inline const std::unordered_map<std::string, Operator> stringToOperator{

	{"+", Operator::ADD}, {"-", Operator::SUBTRACT}, {"*", Operator::MULTIPLY},
	{"/", Operator::DIVIDE}, {"^", Operator::POWER}, {"%", Operator::MODULUS},
	{"=", Operator::ASSIGN}, {"<", Operator::LESS}, {">", Operator::GREATER},
	{"<=", Operator::LESS_EQUAL}, {">=", Operator::GREATER_EQUAL}, {"and", Operator::AND},
	{"or", Operator::OR}, {"xor", Operator::XOR}, {"bitand", Operator::BITAND},
	{"bitor", Operator::BITOR}, {"bitxor", Operator::BITXOR}, {"bitnot", Operator::BITNOT},
	{"eq", Operator::EQUAL}, {"not_eq", Operator::NOT_EQUAL}, {"cast", Operator::CAST},
	{"cast_if", Operator::CAST_IF}, {"unsafe_cast", Operator::UNSAFE_CAST}, {"++", Operator::PRE_INCREMENT},
	{"--", Operator::PRE_DECREMENT}, {"-", Operator::UNARY_MINUS}, {"@", Operator::ADDRESS_OF},
	{"not", Operator::NOT}, {"++", Operator::POST_INCREMENT}, {"--", Operator::POST_DECREMENT},
};

constexpr const char* operatorToString(const Operator e) {
constexpr const char* toString[] = {

	"+","-","*",
	"/","^","%",
	"=","<",">",
	"<=",">=","and",
	"or","xor","bitand",
	"bitor","bitxor","bitnot",
	"eq","not_eq","cast",
	"cast_if","unsafe_cast","++",
	"--","-","@",
	"not","++","--",
};
	return toString[std::to_underlying(e)];
}
constexpr bool isCategoryBINARY_OPS(const Operator e) { return std::to_underlying(e) < 20; }
constexpr bool isCategoryCASTS(const Operator e) { return std::to_underlying(e) >= 20 && std::to_underlying(e) < 23; }
constexpr bool isCategoryPREFIX_OPS(const Operator e) { return std::to_underlying(e) >= 20 && std::to_underlying(e) < 28; }
constexpr bool isCategoryUNARY_OPS(const Operator e) { return std::to_underlying(e) >= 23 && std::to_underlying(e) < 30; }

struct Expression;

struct UnaryExpression {
  Expression *expr;
  Operator opr;
  unsigned line_number;

  UnaryExpression(Expression *_expr, const Operator _opr, const unsigned line_num)
      : expr(_expr), opr(_opr), line_number(line_num) {}
};

struct BinaryExpression {
  Expression *expr_left;
  Expression *expr_right;
  Operator opr;
  unsigned line_number;

  BinaryExpression(Expression *_expr_left, Expression *_expr_right,
                   const Operator _opr, const unsigned line_num)
      : expr_left(_expr_left), expr_right(_expr_right), opr(_opr), line_number(line_num) {}
};

struct CallingExpression {
  Expression *func;
  std::vector<Expression *> parameters;
  unsigned line_number;

  CallingExpression(Expression *_f, std::vector<Expression *> &&_params, const unsigned line_num)
      : func(_f), parameters(std::move(_params)), line_number(line_num) {}
};

struct SubscriptExpression {
  Expression *arr;
  Expression *inside;
  unsigned line_number;

  SubscriptExpression(Expression *_arr, Expression *_inside, const unsigned line_num)
      : arr(_arr), inside(_inside), line_number(line_num) {}
};

struct IdentifierExpression {
  std::string ident;
  unsigned line_number;

  explicit IdentifierExpression(std::string &&_ident, const unsigned line_num)
      : ident(std::move(_ident)), line_number(line_num) {}
};

struct LiteralExpression {
  using LiteralValue = std::variant<int, float, double, std::string>;

  enum LiteralType { INT, FLOAT, DOUBLE, BOOL, CHAR, STRING };

  LiteralValue value;
  LiteralType type;
  unsigned line_number;

  explicit LiteralExpression(LiteralValue &&_v, const LiteralType _t, const unsigned line_num)
      : value(std::move(_v)), type(_t), line_number(line_num) {}
};

struct TemporaryExpr {
  // finish
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

struct ExpressionToStringVisitor {
  std::string operator()(const UnaryExpression &) const noexcept;

  std::string operator()(const BinaryExpression &) const noexcept;

  std::string operator()(const CallingExpression &) const noexcept;

  std::string operator()(const SubscriptExpression &) const noexcept;

  std::string operator()(const IdentifierExpression &) const noexcept;

  std::string operator()(const LiteralExpression &) const noexcept;

  std::string operator()(const TemporaryExpr &) const noexcept;
};