#pragma once
#include "utilities/owned_ptr.hpp"
#include <cassert>
#include <cstdint>
#include <stdfloat>
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
  EQUAL,
  NOT_EQUAL,
  CAST,
  CAST_IF,
  UNSAFE_CAST,
  PRE_INCREMENT,
  PRE_DECREMENT,
  UNARY_MINUS,
  ADDRESS_OF,
  BITNOT,
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
	{"bitor", Operator::BITOR}, {"bitxor", Operator::BITXOR}, {"eq", Operator::EQUAL},
	{"not_eq", Operator::NOT_EQUAL}, {"cast", Operator::CAST}, {"cast_if", Operator::CAST_IF},
	{"unsafe_cast", Operator::UNSAFE_CAST}, {"++", Operator::PRE_INCREMENT}, {"--", Operator::PRE_DECREMENT},
	{"-", Operator::UNARY_MINUS}, {"@", Operator::ADDRESS_OF}, {"bitnot", Operator::BITNOT},
	{"not", Operator::NOT}, {"++", Operator::POST_INCREMENT}, {"--", Operator::POST_DECREMENT},
};


constexpr const char* operatorToString(const Operator e) {
constexpr const char* toString[] = {

	"+","-","*",
	"/","^","%",
	"=","<",">",
	"<=",">=","and",
	"or","xor","bitand",
	"bitor","bitxor","eq",
	"not_eq","cast","cast_if",
	"unsafe_cast","++","--",
	"-","@","bitnot",
	"not","++","--",
};
	return toString[std::to_underlying(e)];
}
constexpr bool isCategoryBINARY_OPS(const Operator e) { return std::to_underlying(e) < 19; }
constexpr bool isCategoryCASTS(const Operator e) { return std::to_underlying(e) >= 19 && std::to_underlying(e) < 22; }
constexpr bool isCategoryPREFIX_OPS(const Operator e) { return std::to_underlying(e) >= 19 && std::to_underlying(e) < 28; }
constexpr bool isCategoryUNARY_OPS(const Operator e) { return std::to_underlying(e) >= 22 && std::to_underlying(e) < 30; }

struct UnaryExpression;
struct BinaryExpression;
struct CallingExpression;
struct SubscriptExpression;
struct IdentifierExpression;
struct LiteralExpression;


using Expression = std::variant<UnaryExpression, BinaryExpression, CallingExpression, SubscriptExpression, IdentifierExpression, LiteralExpression>;

struct UnaryExpression {
  owned_ptr<Expression> expr;
  Operator opr;
  unsigned line_number;

  UnaryExpression(owned_ptr<Expression> _expr, const Operator _opr, const unsigned line_num)
  : expr(std::move(_expr)), opr(_opr), line_number(line_num) { assert(isCategoryUNARY_OPS(opr)); }

  ~UnaryExpression();
};

struct BinaryExpression {
  owned_ptr<Expression> expr_left;
  owned_ptr<Expression> expr_right;
  Operator opr;
  unsigned line_number;

  BinaryExpression(owned_ptr<Expression> _expr_left, owned_ptr<Expression> _expr_right, const Operator _opr, const unsigned line_num)
  : expr_left(std::move(_expr_left)), expr_right(std::move(_expr_right)), opr(_opr), line_number(line_num) { assert(isCategoryBINARY_OPS(opr)); }

  ~BinaryExpression();
};

struct CallingExpression {
  owned_ptr<Expression> func;
  std::vector<Expression*> parameters;
  unsigned line_number;

  CallingExpression(owned_ptr<Expression> _f, std::vector<Expression*> &&_params, const unsigned line_num)
  : func(std::move(_f)), parameters(std::move(_params)), line_number(line_num) {}

  ~CallingExpression();
};

struct SubscriptExpression {
  owned_ptr<Expression> arr;
  owned_ptr<Expression> inside;
  unsigned line_number;


  SubscriptExpression(owned_ptr<Expression> _arr, owned_ptr<Expression> _inside, const unsigned line_num)
  : arr(std::move(_arr)), inside(std::move(_inside)), line_number(line_num) {}

  ~SubscriptExpression();
};

struct IdentifierExpression {
  std::string ident;
  unsigned line_number;

  IdentifierExpression(std::string &&_ident, const unsigned line_num)
  : ident(std::move(_ident)), line_number(line_num) {}

};

struct LiteralExpression {
  using LiteralValue = std::variant<std::uint64_t, std::float32_t, std::float64_t, std::string>;
  enum LiteralType { INT, UINT, FLOAT, DOUBLE, BOOL, CHAR, STRING };

  LiteralValue value;
  LiteralType type;
  unsigned line_number;

  explicit LiteralExpression(LiteralValue &&_v, const LiteralType _t, const unsigned line_num)
  : value(std::move(_v)), type(_t), line_number(line_num) {}

  [[nodiscard]] std::int64_t getInt() const { return std::bit_cast<std::int64_t>(std::get<uint64_t>(value)); }
  [[nodiscard]] std::uint64_t getUint() const { return std::get<uint64_t>(value); }
  [[nodiscard]] float getFloat() const { return std::get<std::float32_t>(value); }
  [[nodiscard]] double getDouble() const { return std::get<std::float64_t>(value); }
  [[nodiscard]] bool getBool() const { return std::get<uint64_t>(value); }
  [[nodiscard]] char getChar() const { return static_cast<char>(std::get<uint64_t>(value)); }
  [[nodiscard]] std::string getString() const { return std::get<std::string>(value); }
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