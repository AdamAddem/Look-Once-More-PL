#pragma once
#include "utilities/owned_ptr.hpp"
#include <cassert>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

namespace AST {
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

// Note: Each AST Node holds ownership over the expressions and statements it has pointers to
// The reason I don't use unique pointer is twofold
// 1: They are ugly and syntactically bulky

// And more importantly,
// 2: Since I'm forward declaring the expression and statement types,
//    unique pointer throws a fit unless the AST Node defines the destructor, which partially defeats the very purpose of unique pointer.
//    In addition, it forces the constructors to be defined in a cpp file, which prevents many of these very trivial constructors from being inlined.
//    It also prevents many of the optional pointers from having a default parameter. (You'll catch me dead before I use std::optional<std::unique_ptr<Expression>>)

struct UnaryExpression {
  Expression *expr; //owned
  Operator opr;
  unsigned line_number;

  //takes ownership over pointer
  UnaryExpression(Expression *_expr, const Operator _opr, const unsigned line_num)
  : expr(_expr), opr(_opr), line_number(line_num) { assert(isCategoryUNARY_OPS(opr)); }

  ~UnaryExpression();
};

struct BinaryExpression {
  Expression *expr_left; //owned
  Expression *expr_right; //owned
  Operator opr;
  unsigned line_number;

  //takes ownership over pointers
  BinaryExpression(Expression *_expr_left, Expression *_expr_right, const Operator _opr, const unsigned line_num)
  : expr_left(_expr_left), expr_right(_expr_right), opr(_opr), line_number(line_num) { assert(isCategoryBINARY_OPS(opr)); }

  ~BinaryExpression();
};

struct CallingExpression {
  Expression *func; //owned
  std::vector<Expression *> parameters; //owned
  unsigned line_number;

  //takes ownership over pointers
  CallingExpression(Expression *_f, std::vector<Expression *> &&_params, const unsigned line_num)
  : func(_f), parameters(std::move(_params)), line_number(line_num) {}

  ~CallingExpression();
};

struct SubscriptExpression {
  Expression *arr; //owned
  Expression *inside; //owned
  unsigned line_number;

  //takes ownership over pointers
  SubscriptExpression(Expression *_arr, Expression *_inside, const unsigned line_num)
  : arr(_arr), inside(_inside), line_number(line_num) {}

  ~SubscriptExpression();
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

struct Expression {
  using ExpressionType = std::variant<UnaryExpression, BinaryExpression, CallingExpression, SubscriptExpression, IdentifierExpression, LiteralExpression>;
  ExpressionType value;
};

struct PrintExpressionVisitor {
  void operator()(const UnaryExpression &) const noexcept;
  void operator()(const BinaryExpression &) const noexcept;
  void operator()(const CallingExpression &) const noexcept;
  void operator()(const SubscriptExpression &) const noexcept;
  void operator()(const IdentifierExpression &) const noexcept;
  void operator()(const LiteralExpression &) const noexcept;
};

struct ExpressionToStringVisitor {
  std::string operator()(const UnaryExpression &) const noexcept;
  std::string operator()(const BinaryExpression &) const noexcept;
  std::string operator()(const CallingExpression &) const noexcept;
  std::string operator()(const SubscriptExpression &) const noexcept;
  std::string operator()(const IdentifierExpression &) const noexcept;
  std::string operator()(const LiteralExpression &) const noexcept;
};
}