#pragma once
#pragma once
#include "file.hpp"
#include "edenlib/vectors/releasing_vector.hpp"
#include "edenlib/typedefs.hpp"
#include "semantic_analysis/types.hpp"
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


// Is this overengineered? Absolutely.
struct ASTNode {

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
    CAST,           // const Type* | EXPRESSION

    // no value, uses the length_in_file and position_in_file
    IDENTIFIER, SIGNED_LITERAL,

    // holds respective value in nodedata
    UNSIGNED_LITERAL,
    FLOAT_LITERAL,
    DOUBLE_LITERAL,
    BOOL_LITERAL,
    CHAR_LITERAL,
    STRING_LITERAL
  };

  struct NodeData {
    Type type;
    u8_t file_idx;
    u16_t length_in_file;
    u32_t position_in_file;
    union {
      Operator opr;
      u64_t num_substatements;
      u64_t num_parameters;
      const LOM::Type* cast_type;
      i64_t signed_value;
      u64_t unsigned_value;
      f32_t float_value;
      f64_t double_value;
      char char_value;
      bool bool_value;
    };
  };

  static_assert(alignof(InstantiatedType) == 8);
  static_assert(sizeof(InstantiatedType) == 16);
  union {
    NodeData m;
    InstantiatedType declaration_inst_type{eden::flags::do_not_initialize};
  };


  // data is purposefully uninitialized here to prevent the expression tree initializing every node when it doesn't need to
  constexpr ASTNode() noexcept = default;

  constexpr explicit ASTNode(NodeData data) : m(data) {}
  constexpr explicit ASTNode(InstantiatedType instantiated) noexcept
  { declaration_inst_type = instantiated; }

  [[nodiscard]] constexpr u64_t
  sub_statements() const noexcept
  { assert(m.type == SCOPED); return m.num_substatements; }

  [[nodiscard]] constexpr u64_t
  parameter_count() const noexcept
  { assert(m.type == CALLING); return m.num_substatements; }

  [[nodiscard]] constexpr InstantiatedType
  instance_type() const noexcept
  { return declaration_inst_type; }

  [[nodiscard]] constexpr const LOM::Type*
  cast_type() const noexcept
  { assert(m.type == CAST); return m.cast_type; }

  [[nodiscard]] constexpr Operator
  operator_val() const noexcept
  { assert(m.type == UNARY or m.type == BINARY); return m.opr; }

  [[nodiscard]] constexpr std::string_view
  identifier(File const& file) const noexcept
  { assert(m.type == IDENTIFIER); return file.view_at(m.position_in_file, m.length_in_file); }

  [[nodiscard]] constexpr i64_t
  signed_val() const noexcept
  { assert(m.type == SIGNED_LITERAL); return m.signed_value; }

  [[nodiscard]] constexpr u64_t
  unsigned_val() const noexcept
  { assert(m.type == UNSIGNED_LITERAL); return m.unsigned_value; }

  [[nodiscard]] constexpr float
  float_val() const noexcept
  { assert(m.type == FLOAT_LITERAL); return m.float_value; }

  [[nodiscard]] constexpr double
  double_val() const noexcept
  { assert(m.type == DOUBLE_LITERAL); return m.double_value; }

  [[nodiscard]] constexpr bool
  bool_val() const noexcept
  { assert(m.type == BOOL_LITERAL); return m.bool_value; }

  [[nodiscard]] constexpr char
  char_val() const noexcept
  { assert(m.type == CHAR_LITERAL); return m.char_value; }

  [[nodiscard]] constexpr std::string_view
  string_val(File const& file) const noexcept
  { assert(m.type == STRING_LITERAL); return file.view_at(m.position_in_file, m.length_in_file); }

};

inline constexpr ASTNode::NodeData EMPTY_NODE_DATA{
    .type = ASTNode::EMPTY,
    .file_idx = 0,
    .length_in_file = 0,
    .position_in_file = 0,
    .bool_value = false
  };

void print_ast(std::vector<ASTNode> const& nodes, File const&) noexcept;

}