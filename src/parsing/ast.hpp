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
  REF_TO,
  BITNOT,
  NOT,
  POST_INCREMENT,
  POST_DECREMENT,
  ARROW,
  DOT,
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
    "@", "&", "bitnot","not",
    "++","--","->", "."
};
  return toString[std::to_underlying(e)];
}

// TODO: Overengineer more.
struct ASTNode {

  enum class NodeType : u8_t {
    EMPTY,
                    // <Following Nodes...>
    // Statements:
    DECLARATION,    // IDENTIFIER W/ Type*, INIT_EXPR (if HAS_INIT is true)
    IF,             // CONDITION_EXPR, STATEMENTS * NUM, ELSE_STMT (if HAS_ELSE is true)
    WHILE,          // CONDITION_EXPR, STATEMENTS * NUM
    RETURN,         // EXPRESSION (if HAS_VALUE is true)

    // Expressions:
    MEMBER_ACCESS,  // CUSTOM_TYPE_EXPRESSION, MEMBER_EXPRESSION | CUSTOM_TYPE_EXPRESSION may be IDENTIFIER with the name of a variable with custom type, or an expression producing a custom type
    MODULE_ACCESS,  /* */
    UNARY,          // EXPRESSION
    BINARY,         // LEFT_EXPRESSION, RIGHT_EXPRESSION
    CALLING,        // CALLED_EXPRESSION, PARAMETERS... * NUM
    CAST,           // EXPRESSION
    SUBSCRIPT,      // ARRAY_EXPRESSION, INDEX_EXPRESSION

    IDENTIFIER,             // value is Type* ONLY if preceeded by declaration, otherwise nothing
    STRING_LITERAL,
    ESCAPED_STRING_LITERAL,

    SIGNED_LITERAL,
    UNSIGNED_LITERAL,
    FLOAT_LITERAL,
    DOUBLE_LITERAL,
    BOOL_LITERAL,
    CHAR_LITERAL,
  }; using enum NodeType;

  struct DeclarationData  { bool has_init; Type::Qualifiers qualifiers; };
  struct IfData           { bool has_else; u32_t num_substatements; };
  struct WhileData        { u32_t num_substatements; };
  struct ReturnData       { bool has_value; };
  struct MemberAccessData { };
  struct ModuleAccessData { u16_t module_length; u16_t member_length; };  static_assert(not Settings::SUBMODULE_SUPPORT, "Might need to change this when adding submodules.");
  struct UnaryData        { Operator opr; };
  struct BinaryData       { Operator opr; };
  struct CallData         { u32_t num_parameters; };
  struct CastData         { Type const* cast_type; };
  struct SubscriptData    { };
  struct IdentifierData   { Type const* decl_type; };
  struct SignedData       { i64_t value; };
  struct UnsignedData     { u64_t value; };
  struct FloatData        { float value; };
  struct DoubleData       { double value; };
  struct BoolData         { bool value; };
  struct CharData         { char value; };
  struct StringData       {};

  NodeType type; u8_t file_idx; u16_t length_in_file; u32_t position_in_file;
  union {
    u64_t base{};
    DeclarationData   declaration_data;
    IfData            if_data;
    WhileData         while_data;
    ReturnData        return_data;
    MemberAccessData  member_access_data;
    ModuleAccessData  module_access_data;
    UnaryData         unary_data;
    BinaryData        binary_data;
    CallData          call_data;
    CastData          cast_data;
    SubscriptData     subscript_data;
    IdentifierData    identifier_data;
    StringData        string_literal_data;
    SignedData        signed_data;
    UnsignedData      unsigned_data;
    FloatData         float_data;
    DoubleData        double_data;
    BoolData          bool_data;
    CharData          char_data;
  };

  eden_always_inline [[nodiscard]] constexpr Type::Qualifiers declaration_qualifiers()          const noexcept { assume_assert(type == DECLARATION); return declaration_data.qualifiers; }
  eden_always_inline [[nodiscard]] constexpr bool             declaration_has_init()            const noexcept { assume_assert(type == DECLARATION); return declaration_data.has_init; }
  eden_always_inline [[nodiscard]] constexpr Type const*      declaration_identifier_val()      const noexcept { assume_assert(type == IDENTIFIER); return identifier_data.decl_type; }

  eden_always_inline [[nodiscard]] constexpr bool             if_has_else()                     const noexcept { assume_assert(type == IF); return if_data.has_else; }
  eden_always_inline [[nodiscard]] constexpr u64_t            if_numstatements()                const noexcept { assume_assert(type == IF); return if_data.num_substatements; }
  eden_always_inline [[nodiscard]] constexpr u64_t            while_numstatements()             const noexcept { assume_assert(type == WHILE); return while_data.num_substatements; }
  eden_always_inline [[nodiscard]] constexpr bool             return_has_value()                const noexcept { assume_assert(type == RETURN); return return_data.has_value; }

  eden_always_inline [[nodiscard]] constexpr Operator         unary_operator()                  const noexcept { assume_assert(type == UNARY); return unary_data.opr; }
  eden_always_inline [[nodiscard]] constexpr Operator         binary_operator()                 const noexcept { assume_assert(type == BINARY); return binary_data.opr; }
  eden_always_inline [[nodiscard]] constexpr u64_t            parameter_count()                 const noexcept { assume_assert(type == CALLING); return call_data.num_parameters; }
  eden_always_inline [[nodiscard]] constexpr Type const*      cast_type()                       const noexcept { assume_assert(type == CAST); return cast_data.cast_type; }

  eden_always_inline [[nodiscard]] constexpr i64_t            signed_val()                      const noexcept { assume_assert(type == SIGNED_LITERAL); return signed_data.value; }
  eden_always_inline [[nodiscard]] constexpr u64_t            unsigned_val()                    const noexcept { assume_assert(type == UNSIGNED_LITERAL); return unsigned_data.value; }
  eden_always_inline [[nodiscard]] constexpr float            float_val()                       const noexcept { assume_assert(type == FLOAT_LITERAL); return float_data.value; }
  eden_always_inline [[nodiscard]] constexpr double           double_val()                      const noexcept { assume_assert(type == DOUBLE_LITERAL); return double_data.value; }
  eden_always_inline [[nodiscard]] constexpr bool             bool_val()                        const noexcept { assume_assert(type == BOOL_LITERAL); return bool_data.value; }
  eden_always_inline [[nodiscard]] constexpr char             char_val()                        const noexcept { assume_assert(type == CHAR_LITERAL); return char_data.value; }
  eden_always_inline [[nodiscard]] constexpr std::string_view identifier_val(File const& file)  const noexcept { assume_assert(type == IDENTIFIER); return file.view_at(length_in_file, position_in_file); }
  eden_always_inline [[nodiscard]] constexpr std::string_view string_val(File const& file)      const noexcept { assume_assert(type == STRING_LITERAL or type == ESCAPED_STRING_LITERAL); return file.view_at(length_in_file, position_in_file); }
  eden_always_inline [[nodiscard]] constexpr std::string_view original_string(File const& file) const noexcept { return file.view_at(length_in_file, position_in_file); }
  eden_always_inline [[nodiscard]] constexpr u32_t            module_position()                 const noexcept { assume_assert(type == MODULE_ACCESS); return position_in_file - module_access_data.module_length; }

  [[nodiscard]] constexpr std::string_view full_module_access(File const& file) const noexcept {
    assume_assert(type == MODULE_ACCESS);
    return file.view_at(module_access_data.module_length + module_access_data.member_length + 1, module_position());
  }

  [[nodiscard]] constexpr std::string_view module_name(File const& file) const noexcept {
    assume_assert(type == MODULE_ACCESS);
    return file.view_at(module_access_data.module_length, module_position());
  }

  [[nodiscard]] constexpr std::string_view module_member_name(File const& file) const noexcept {
    assume_assert(type == MODULE_ACCESS);
    return file.view_at(module_access_data.member_length, position_in_file + 1);
  }


};
static_assert(alignof(QualifiedType) == 8);
static_assert(sizeof(QualifiedType) == 16);

inline constexpr ASTNode PLACEHOLDER_NODE{
    .type = ASTNode::EMPTY,
    .file_idx = 0,
    .length_in_file = 0,
    .position_in_file = 0,
    .base = 0
};

void print_ast(std::vector<ASTNode> const& nodes, File const&) noexcept;

}