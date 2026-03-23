#pragma once
#pragma once
#include "semantic_analysis/types.hpp"
#include "utilities/typedefs.hpp"
#include <cassert>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace LOM::AST {

enum class Operator : u8_t {
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

    {"+", Operator::ADD},
    {"-", Operator::SUBTRACT},
    {"*", Operator::MULTIPLY},
    {"/", Operator::DIVIDE},
    {"^", Operator::POWER},
    {"%", Operator::MODULUS},
    {"=", Operator::ASSIGN},
    {"<", Operator::LESS},
    {">", Operator::GREATER},
    {"<=", Operator::LESS_EQUAL},
    {">=", Operator::GREATER_EQUAL},
    {"and", Operator::AND},
    {"or", Operator::OR},
    {"xor", Operator::XOR},
    {"bitand", Operator::BITAND},
    {"bitor", Operator::BITOR},
    {"bitxor", Operator::BITXOR},
    {"eq", Operator::EQUAL},
    {"not_eq", Operator::NOT_EQUAL},
    {"cast", Operator::CAST},
    {"cast_if", Operator::CAST_IF},
    {"unsafe_cast", Operator::UNSAFE_CAST},
    {"++", Operator::PRE_INCREMENT},
    {"--", Operator::PRE_DECREMENT},
    {"-", Operator::UNARY_MINUS},
    {"@", Operator::ADDRESS_OF},
    {"bitnot", Operator::BITNOT},
    {"not", Operator::NOT},
    {"++", Operator::POST_INCREMENT},
    {"--", Operator::POST_DECREMENT},
};

constexpr const char *operatorToString(const Operator e) {
  constexpr const char *toString[] = {

      "+",      "-",    "*",       "/",           "^",      "%",
      "=",      "<",    ">",       "<=",          ">=",     "and",
      "or",     "xor",  "bitand",  "bitor",       "bitxor", "eq",
      "not_eq", "cast", "cast_if", "unsafe_cast", "++",     "--",
      "-",      "@",    "bitnot",  "not",         "++",     "--",
  };
  return toString[std::to_underlying(e)];
}
constexpr bool isCategoryBINARY_OPS(const Operator e) {
  return std::to_underlying(e) < 19;
}
constexpr bool isCategoryCASTS(const Operator e) {
  return std::to_underlying(e) >= 19 && std::to_underlying(e) < 22;
}
constexpr bool isCategoryPREFIX_OPS(const Operator e) {
  return std::to_underlying(e) >= 19 && std::to_underlying(e) < 28;
}
constexpr bool isCategoryUNARY_OPS(const Operator e) {
  return std::to_underlying(e) >= 22 && std::to_underlying(e) < 30;
}

}


namespace LOM::AST {

struct ASTNode;
struct SyntaxTree {
  explicit SyntaxTree(std::vector<ASTNode> nodes) : nodes(std::move(nodes)) {}
  SyntaxTree() = default;
  SyntaxTree(SyntaxTree&&) noexcept = default;

  std::vector<ASTNode> nodes;
  void print() const noexcept;

};
 void print(std::span<const ASTNode> nodes);


struct ASTNode {

  enum Type : u8_t {
    EMPTY,

                 // HAS <value>    | <Following Nodes>...
    //Statements:
    DECLARATION, // HAS LN         | IDENTIFIER, INIT_EXPR
    IF,          // HAS LN         | CONDITION_EXPR, SCOPED, EMPTY or ELSE_STATEMENT
    FOR,         // HAS LN         | DECLARATION, CONDITION_EXPR, INCREMENT_EXPR, SCOPED
    WHILE,       // HAS LN         | CONDITION_EXPR, SCOPED
    SCOPED,      // HAS NUM        | SUB_STATEMENTS... * NUM
    RETURN,      // HAS LN         | EMPTY or EXPRESSION

    //Expressions:
    UNARY,       // HAS OPERATOR   | EXPRESSION
    BINARY,      // HAS OPERATOR   | LEFT_EXPRESSION, RIGHT_EXPRESSION
    CALLING,     // HAS NUM        | CALLED_EXPRESSION, PARAMETERS... * NUM
    SUBSCRIPT,   // HAS N/A        | ARRAY_EXPRESSION, INSIDE_EXPRESSION
    IDENTIFIER,  // HAS STRING*    |

    //value holds the bitwise representation of their respective type
    INT_LITERAL, UINT_LITERAL, FLOAT_LITERAL, DOUBLE_LITERAL, BOOL_LITERAL, CHAR_LITERAL,

    //HAS STRING*
    STRING_LITERAL
}type;
  u64_t value;

  ASTNode() = default;
  explicit ASTNode(decltype(EMPTY) type, u64_t value = 0) : type(type), value(value) {}
  ASTNode(ASTNode&& other) noexcept : type(other.type), value(other.value) {
    other.value = 0;
  }

  static_assert(sizeof(u64_t) >= sizeof(Operator));
  static_assert(std::unsigned_integral<std::underlying_type_t<Operator>>);
  [[nodiscard]] constexpr Operator
  getOperator() const noexcept {
    assert(type == UNARY || type == BINARY);
    return static_cast<Operator>(value);
  }

  [[nodiscard]] constexpr const std::string*
  getIdentifier() const noexcept {
    assert(type == IDENTIFIER);
    return std::bit_cast<std::string*>(value);
  }

  [[nodiscard]] constexpr i64_t
  getInt() const noexcept {
    assert(type == INT_LITERAL);
    return std::bit_cast<i64_t>(value);
  }

  [[nodiscard]] constexpr u64_t
  getUint() const noexcept {
    assert(type == UINT_LITERAL);
    return value;
  }

  static_assert(sizeof(float) == 4);
  [[nodiscard]] constexpr float
  getFloat() const noexcept {
    assert(type == FLOAT_LITERAL);
    return std::bit_cast<float>(static_cast<u32_t>(value));
  }

  static_assert(sizeof(double) == 8);
  [[nodiscard]] constexpr double
  getDouble() const noexcept {
    assert(type == DOUBLE_LITERAL);
    return std::bit_cast<double>(value);
  }

  [[nodiscard]] constexpr bool
  getBool() const noexcept {
    assert(type == BOOL_LITERAL);
    return value;
  }

  [[nodiscard]] constexpr char
  getChar() const noexcept {
    assert(type == CHAR_LITERAL);
    return static_cast<char>(value);
  }

  static_assert(sizeof(u64_t) >= sizeof(void*));
  [[nodiscard]] constexpr std::string*
  getString() const noexcept {
    assert(type == STRING_LITERAL);
    return std::bit_cast<std::string*>(value);
  }

  constexpr ~ASTNode() {
    if (type == IDENTIFIER ||
        type == STRING_LITERAL)
      delete std::bit_cast<std::string*>(value);
  }

};


}