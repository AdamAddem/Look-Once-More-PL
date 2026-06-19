#pragma once
#pragma once
#include "edenlib/vectors/releasing_vector.hpp"
#include "edenlib/macros.hpp"
#include "edenlib/typedefs.hpp"
#include "semantic_analysis/types.hpp"
#include <cassert>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

namespace LOM::AST {
enum class Operator : u8_t {
  ADD,
  SUBTRACT,
  MULTIPLY,
  DIVIDE,
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
  PRE_INCREMENT,
  PRE_DECREMENT,
  UNARY_MINUS,
  ADDRESS_OF,
  BITNOT,
  NOT,
  POST_INCREMENT,
  POST_DECREMENT,
  ARROW,
  DOT,
};

inline const std::unordered_map<std::string, Operator> stringToOperator{
	  {"+", Operator::ADD}, {"-", Operator::SUBTRACT}, {"*", Operator::MULTIPLY},
          {"/", Operator::DIVIDE}, {"%", Operator::MODULUS}, {"=", Operator::ASSIGN},
          {"<", Operator::LESS}, {">", Operator::GREATER}, {"<=", Operator::LESS_EQUAL},
          {">=", Operator::GREATER_EQUAL}, {"and", Operator::AND}, {"or", Operator::OR},
          {"xor", Operator::XOR}, {"bitand", Operator::BITAND}, {"bitor", Operator::BITOR},
          {"bitxor", Operator::BITXOR}, {"eq", Operator::EQUAL}, {"not_eq", Operator::NOT_EQUAL},
          {"++", Operator::PRE_INCREMENT}, {"--", Operator::PRE_DECREMENT}, {"-", Operator::UNARY_MINUS},
          {"@", Operator::ADDRESS_OF}, {"bitnot", Operator::BITNOT}, {"not", Operator::NOT},
          {"++", Operator::POST_INCREMENT}, {"--", Operator::POST_DECREMENT}, {"->", Operator::ARROW},
          {".", Operator::DOT}
};

constexpr const char* operatorToString(const Operator e) {
  constexpr const char* toString[] = {
    "+","-","*",
    "/","%","=",
    "<",">","<=",
    ">=","and","or",
    "xor","bitand","bitor",
    "bitxor","eq","not_eq",
    "++","--","-",
    "@","bitnot","not",
    "++","--","->", "."
};
  return toString[std::to_underlying(e)];
}
constexpr bool isCategoryBINARY_OPS(const Operator e) { return std::to_underlying(e) < 18; }
constexpr bool isCategoryPREFIX_OPS(const Operator e) { return std::to_underlying(e) >= 18 && std::to_underlying(e) < 24; }
constexpr bool isCategoryPOSTFIX_OPS(const Operator e) { return std::to_underlying(e) >= 24 && std::to_underlying(e) < 28; }
constexpr bool isCategoryUNARY_OPS(const Operator e) { return std::to_underlying(e) >= 18 && std::to_underlying(e) < 28; }


class ASTNode;
struct SyntaxTree {
  explicit SyntaxTree(std::vector<ASTNode> nodes);
  SyntaxTree() = default;
  SyntaxTree(const SyntaxTree&) = delete;
  SyntaxTree(SyntaxTree&&) noexcept = default;

  std::vector<ASTNode> nodes;
  void print(u64_t starting_line_number) const noexcept;
};

// Is this overengineered? Absolutely.
class ASTNode {

  static_assert(alignof(InstantiatedType) == 8);
  static_assert(sizeof(InstantiatedType) == 16);
  alignas(InstantiatedType)
  std::byte data[sizeof(InstantiatedType)];
public:

  enum Type : u8_t {
    EMPTY,
                    // HAS <value> | <Following Nodes...>
    // Statements:
    DECLARATION,    //             | INSTANTIATED_TYPE, IDENTIFIER, INIT_EXPR or EMPTY
    IF,             //             | CONDITION_EXPR, SCOPED, EMPTY or ELSE_STMT
    FOR,            //             | DECLARATION, CONDITION_EXPR, INCREMENT_EXPR, SCOPED
    WHILE,          //             | CONDITION_EXPR, SCOPED
    SCOPED,         // NUM         | SUB_STATEMENTS... * NUM
    RETURN,         //             | EMPTY or EXPRESSION
    EXPR_STMT,      //             | EMPTY or EXPRESSION

    // Expressions:
    MEMBER_ACCESS,  //             | SCOPE_EXPRESSION, MEMBER_EXPRESSION | SCOPE_EXPRESSION may be IDENTIFIER with the name of a module, or an expression producing a custom type
    UNARY,          // OPERATOR    | EXPRESSION
    BINARY,         // OPERATOR    | LEFT_EXPRESSION, RIGHT_EXPRESSION
    CALLING,        // NUM         | CALLED_EXPRESSION, PARAMETERS... * NUM
    SUBSCRIPT,      //             | ARRAY_EXPRESSION, INSIDE_EXPRESSION
    IDENTIFIER,     // char*       |
    CAST,           // const Type* | EXPRESSION

    // value holds the bitwise representation of their respective type
    SIGNED_LITERAL,
    UNSIGNED_LITERAL,
    FLOAT_LITERAL,
    DOUBLE_LITERAL,
    BOOL_LITERAL,
    CHAR_LITERAL,

    //HAS STRING*
    STRING_LITERAL
  };

  // data is purposefully uninitialized here to prevent the expression tree initializing every node when it doesn't need to
  ASTNode() = default;

  constexpr explicit ASTNode(Type type, u64_t value_ = 0) noexcept
  { data[0] = static_cast<std::byte>(type); value() = value_; }
  constexpr explicit ASTNode(InstantiatedType instantiated) noexcept
  { std::construct_at<InstantiatedType>(reinterpret_cast<InstantiatedType *>(data), instantiated); }

  [[nodiscard]] constexpr u64_t&
  value() noexcept
  {return *std::launder(reinterpret_cast<u64_t*>(data + 8));}

  [[nodiscard]] constexpr u64_t
  value() const noexcept
  {return *std::launder(reinterpret_cast<const u64_t*>(data + 8));}

  [[nodiscard]] constexpr Type
  type() const noexcept
  {return static_cast<Type>(data[0]);}

  [[nodiscard]] constexpr u64_t
  line_number() const noexcept {
    assert(not eden::enumBetween(type(), UNARY, STRING_LITERAL) and type() not_eq SCOPED);
    return value();
  }

  [[nodiscard]] constexpr u64_t
  sub_statements() const noexcept {assert(type() == SCOPED); return value();}

  [[nodiscard]] constexpr u64_t
  parameter_count() const noexcept {assert(type() == CALLING); return value();}

  [[nodiscard]] constexpr InstantiatedType
  instance_type() const noexcept {return *std::launder(reinterpret_cast<const InstantiatedType*>(data));}

  [[nodiscard]] constexpr const LOM::Type*
  cast_type() const noexcept {assert(type() == CAST); return std::bit_cast<const LOM::Type*>(value());}

  static_assert(sizeof(u64_t) >= sizeof(Operator));
  static_assert(std::unsigned_integral<std::underlying_type_t<Operator>>);
  [[nodiscard]] constexpr Operator
  operator_val() const noexcept {
    assert(type() == UNARY or type() == BINARY);
    return static_cast<Operator>(value());
  }

  eden_return_nonnull
  [[nodiscard]] constexpr char*
  identifier() const noexcept {
    assert(type() == IDENTIFIER);
    return std::bit_cast<char*>(value());
  }

  [[nodiscard]] constexpr i64_t
  signed_val() const noexcept
  {assert(type() == SIGNED_LITERAL); return std::bit_cast<i64_t>(value()); }

  [[nodiscard]] constexpr u64_t
  unsigned_val() const noexcept
  {assert(type() == UNSIGNED_LITERAL); return value();}

  static_assert(sizeof(float) == sizeof(u32_t));
  [[nodiscard]] constexpr float
  float_val() const noexcept
  {assert(type() == FLOAT_LITERAL); return std::bit_cast<float>(static_cast<u32_t>(value()));}

  static_assert(sizeof(double) == sizeof(u64_t));
  [[nodiscard]] constexpr double
  double_val() const noexcept
  {assert(type() == DOUBLE_LITERAL); return std::bit_cast<double>(value());}

  [[nodiscard]] constexpr bool
  bool_val() const noexcept
  {assert(type() == BOOL_LITERAL); return value();}

  [[nodiscard]] constexpr char
  char_val() const noexcept
  {assert(type() == CHAR_LITERAL); return static_cast<char>(value());}

  static_assert(sizeof(u64_t) >= sizeof(void*));

  eden_return_nonnull
  [[nodiscard]] constexpr char*
  string_val() const noexcept
  {assert(type() == STRING_LITERAL); return std::bit_cast<char*>(value());}

};

inline SyntaxTree::SyntaxTree(std::vector<ASTNode> nodes) : nodes(std::move(nodes)) {}
}