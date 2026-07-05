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


// TODO: Overengineer more.
struct ASTNode {

  enum class NodeType : u8_t {
    EMPTY,
                    // HAS <value>  | <Following Nodes...>
    // Statements:
    DECLARATION,    // HAS_INIT, Qualifiers | IDENTIFIER W/ Type*, INIT_EXPR (if HAS_INIT is true)
    IF,             // HAS_ELSE, NUM        | CONDITION_EXPR, STATEMENTS * NUM, ELSE_STMT (if HAS_ELSE is true)
    WHILE,          // NUM                  | CONDITION_EXPR, STATEMENTS * NUM
    RETURN,         // HAS_VALUE            | EXPRESSION (if HAS_VALUE is true)

    // Expressions:
    MEMBER_ACCESS,  //           | SCOPE_EXPRESSION, MEMBER_EXPRESSION | SCOPE_EXPRESSION may be IDENTIFIER with the name of a module, a variable with custom type, or an expression producing a custom type
    UNARY,          // OPERATOR  | EXPRESSION
    BINARY,         // OPERATOR  | LEFT_EXPRESSION, RIGHT_EXPRESSION
    CALLING,        // NUM       | CALLED_EXPRESSION, PARAMETERS... * NUM
    CAST,           // Type*     | EXPRESSION
    SUBSCRIPT,      //           | ARRAY_EXPRESSION, INDEX_EXPRESSION

    IDENTIFIER,             // value is Type* ONLY if preceeded by declaration, otherwise nothing
    STRING_LITERAL,         // no value, uses the length_in_file and position_in_file
    ESCAPED_STRING_LITERAL, // no value, uses the length_in_file and position_in_file

    // holds respective value in nodedata
    SIGNED_LITERAL,
    UNSIGNED_LITERAL,
    FLOAT_LITERAL,
    DOUBLE_LITERAL,
    BOOL_LITERAL,
    CHAR_LITERAL,
  }; using enum NodeType;

  // structured this way to abuse the common subsequence exception for unions
  // structs dont inherit from CommonData as that would disqualify them from being standard-layout which is a requirement for this to work
#define common_subsequence NodeType type; u8_t file_idx; u16_t length_in_file; u32_t position_in_file;
  struct CommonData       { common_subsequence };

  struct DeclarationData  { common_subsequence bool has_init; Type::Qualifiers qualifiers; };
  struct IfData           { common_subsequence bool has_else; u32_t num_substatements; };
  struct WhileData        { common_subsequence u32_t num_substatements; };
  struct ReturnData       { common_subsequence bool has_value; };

  struct MemberAccessData { common_subsequence };
  struct UnaryData        { common_subsequence Operator opr; };
  struct BinaryData       { common_subsequence Operator opr; };
  struct CallingData      { common_subsequence u32_t num_parameters; };
  struct CastData         { common_subsequence Type const* cast_type; };

  struct IdentifierData   { common_subsequence Type const* decl_type; };
  struct SignedData       { common_subsequence i64_t value; };
  struct UnsignedData     { common_subsequence u64_t value; };
  struct FloatData        { common_subsequence float value; };
  struct DoubleData       { common_subsequence double value; };
  struct BoolData         { common_subsequence bool value; };
  struct CharData         { common_subsequence char value; };
  struct StringData       { common_subsequence };

#undef common_subsequence

  static_assert(alignof(QualifiedType) == 8);
  static_assert(sizeof(QualifiedType) == 16);
  union {
    CommonData m;

    DeclarationData declaration_data;
    IfData if_data;
    WhileData while_data;
    ReturnData return_data;

    MemberAccessData member_access_data;
    UnaryData unary_data;
    BinaryData binary_data;
    CallingData calling_data;
    CastData cast_data;

    IdentifierData identifier_data;
    StringData string_literal_data;

    SignedData signed_data;
    UnsignedData unsigned_data;
    FloatData float_data;
    DoubleData double_data;
    BoolData bool_data;
    CharData char_data;

    QualifiedType declaration_inst_type;
  };

  // data is purposefully uninitialized here to prevent the expression tree initializing every node when it doesn't need to
  constexpr ASTNode() noexcept {}

  // does not initialize anything but type and file idx
  constexpr explicit ASTNode(NodeType type, u8_t file_idx) { m.type = type; m.file_idx = file_idx; }
  constexpr explicit ASTNode(CommonData data) : m(data) {}

  [[nodiscard]] constexpr Type::Qualifiers declaration_qualifiers()          const noexcept { assume_assert(m.type == DECLARATION); return declaration_data.qualifiers; }
  [[nodiscard]] constexpr Type const*      declaration_identifier_val()      const noexcept { assume_assert(m.type == IDENTIFIER); return identifier_data.decl_type; }

  [[nodiscard]] constexpr bool             if_has_else()                     const noexcept { assume_assert(m.type == IF); return if_data.has_else; }
  [[nodiscard]] constexpr u64_t            if_numstatements()                const noexcept { assume_assert(m.type == IF); return if_data.num_substatements; }
  [[nodiscard]] constexpr u64_t            while_numstatements()             const noexcept { assume_assert(m.type == WHILE); return while_data.num_substatements; }
  [[nodiscard]] constexpr bool             return_has_value()                const noexcept { assume_assert(m.type == RETURN); return return_data.has_value; }

  [[nodiscard]] constexpr Operator         unary_operator()                  const noexcept { assume_assert(m.type == UNARY); return unary_data.opr; }
  [[nodiscard]] constexpr Operator         binary_operator()                 const noexcept { assume_assert(m.type == BINARY); return binary_data.opr; }
  [[nodiscard]] constexpr u64_t            parameter_count()                 const noexcept { assume_assert(m.type == CALLING); return calling_data.num_parameters; }
  [[nodiscard]] constexpr Type const*      cast_type()                       const noexcept { assume_assert(m.type == CAST); return cast_data.cast_type; }

  [[nodiscard]] constexpr i64_t            signed_val()                      const noexcept { assume_assert(m.type == SIGNED_LITERAL); return signed_data.value; }
  [[nodiscard]] constexpr u64_t            unsigned_val()                    const noexcept { assume_assert(m.type == UNSIGNED_LITERAL); return unsigned_data.value; }
  [[nodiscard]] constexpr float            float_val()                       const noexcept { assume_assert(m.type == FLOAT_LITERAL); return float_data.value; }
  [[nodiscard]] constexpr double           double_val()                      const noexcept { assume_assert(m.type == DOUBLE_LITERAL); return double_data.value; }
  [[nodiscard]] constexpr bool             bool_val()                        const noexcept { assume_assert(m.type == BOOL_LITERAL); return bool_data.value; }
  [[nodiscard]] constexpr char             char_val()                        const noexcept { assume_assert(m.type == CHAR_LITERAL); return char_data.value; }
  [[nodiscard]] constexpr std::string_view identifier_val(File const& file)  const noexcept { assume_assert(m.type == IDENTIFIER); return file.view_at(m.length_in_file, m.position_in_file); }
  [[nodiscard]] constexpr std::string_view string_val(File const& file)      const noexcept { assume_assert(m.type == STRING_LITERAL or m.type == ESCAPED_STRING_LITERAL); return file.view_at(m.length_in_file, m.position_in_file); }
  [[nodiscard]] constexpr std::string_view original_string(File const& file) const noexcept { return file.view_at(m.length_in_file, m.position_in_file); }
};
static_assert(sizeof(ASTNode) == 16);

inline constexpr ASTNode PLACEHOLDER_NODE{
  ASTNode::CommonData{
    .type = ASTNode::EMPTY,
    .file_idx = 0,
    .length_in_file = 0,
    .position_in_file = 0
  }
};

void print_ast(std::vector<ASTNode> const& nodes, File const&) noexcept;

}