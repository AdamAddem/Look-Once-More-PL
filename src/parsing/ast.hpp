#pragma once
#include "edenlib/macros.hpp"
#include "file.hpp"
#include "edenlib/vectors/vector.hpp"
#include "edenlib/typedefs.hpp"
#include "module/types.hpp"
#include <cassert>
#include <string>
#include <utility>

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

edenNoInlineCold edenNodiscardCXPR
char const* operatorToString(Operator e) noexcept {
  static constexpr char const* toString[] = {
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
    DECLARATION_JUNK_RO,
    DECLARATION_JUNK_RW,
    DECLARATION_RO, // INIT_EXPR
    DECLARATION_RW, // INIT_EXPR
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

    IDENTIFIER,
    STRING_LITERAL,
    ESCAPED_STRING_LITERAL,

    SIGNED_LITERAL,
    UNSIGNED_LITERAL,
    FLOAT_LITERAL,
    DOUBLE_LITERAL,
    BOOL_LITERAL,
    CHAR_LITERAL,
  }; using enum NodeType;

  struct DeclarationData  { TypeID typeID; }; // will have the length and position of the file of the declared identifier
  struct IfData           { bool has_else; u32_t num_substatements; };
  struct WhileData        { u32_t num_substatements; };
  struct ReturnData       { bool has_value; };
  struct MemberAccessData { };
  struct ModuleAccessData { u16_t module_length; u16_t member_length; };  static_assert(not Settings::SUBMODULE_SUPPORT, "Might need to change this when adding submodules.");
  struct UnaryData        { Operator opr; };
  struct BinaryData       { Operator opr; };
  struct CallData         { u32_t num_parameters; };
  struct CastData         { TypeID cast_type; };
  struct SubscriptData    { };
  struct IdentifierData   { };
  struct SignedData       { i64_t value; };
  struct UnsignedData     { u64_t value; };
  struct FloatData        { float value; };
  struct DoubleData       { double value; };
  struct BoolData         { bool value; };
  struct CharData         { char value; };
  struct StringData       { };

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

  edenInlineNodiscardCXPR bool isDeclaration() const noexcept { return eden::enumBetween(type, DECLARATION_JUNK_RO, DECLARATION_RW); }
  edenInlineNodiscardCXPR bool isRWDeclaration() const noexcept { return type == DECLARATION_RW or type == DECLARATION_JUNK_RW; }
  edenInlineNodiscardCXPR bool isJunkDeclaration() const noexcept { return eden::enumBetween(type, DECLARATION_JUNK_RO, DECLARATION_JUNK_RW); }
  edenInlineNodiscardCXPR TypeID  declaration_type()                         const noexcept { edenAssume(isDeclaration()); return declaration_data.typeID; }

  edenInlineNodiscardCXPR bool             if_has_else()                     const noexcept { assert(type == IF); return if_data.has_else; }
  edenInlineNodiscardCXPR u64_t            if_numstatements()                const noexcept { assert(type == IF); return if_data.num_substatements; }
  edenInlineNodiscardCXPR u64_t            while_numstatements()             const noexcept { assert(type == WHILE); return while_data.num_substatements; }
  edenInlineNodiscardCXPR bool             return_has_value()                const noexcept { assert(type == RETURN); return return_data.has_value; }

  edenInlineNodiscardCXPR Operator         unary_operator()                  const noexcept { assert(type == UNARY); return unary_data.opr; }
  edenInlineNodiscardCXPR Operator         binary_operator()                 const noexcept { assert(type == BINARY); return binary_data.opr; }
  edenInlineNodiscardCXPR u64_t            parameter_count()                 const noexcept { assert(type == CALLING); return call_data.num_parameters; }
  edenInlineNodiscardCXPR TypeID           cast_type()                       const noexcept { assert(type == CAST); return cast_data.cast_type; }

  edenInlineNodiscardCXPR i64_t            signed_val()                      const noexcept { assert(type == SIGNED_LITERAL); return signed_data.value; }
  edenInlineNodiscardCXPR u64_t            unsigned_val()                    const noexcept { assert(type == UNSIGNED_LITERAL); return unsigned_data.value; }
  edenInlineNodiscardCXPR float            float_val()                       const noexcept { assert(type == FLOAT_LITERAL); return float_data.value; }
  edenInlineNodiscardCXPR double           double_val()                      const noexcept { assert(type == DOUBLE_LITERAL); return double_data.value; }
  edenInlineNodiscardCXPR bool             bool_val()                        const noexcept { assert(type == BOOL_LITERAL); return bool_data.value; }
  edenInlineNodiscardCXPR char             char_val()                        const noexcept { assert(type == CHAR_LITERAL); return char_data.value; }
  edenInlineNodiscardCXPR std::string_view identifier_val(File const& file)  const noexcept { assert(type == IDENTIFIER); return file.view_at(length_in_file, position_in_file); }
  edenInlineNodiscardCXPR std::string_view string_val(File const& file)      const noexcept { assert(type == STRING_LITERAL or type == ESCAPED_STRING_LITERAL); return file.view_at(length_in_file, position_in_file); }
  edenInlineNodiscardCXPR std::string_view original_string(File const& file) const noexcept { return file.view_at(length_in_file, position_in_file); }
  edenInlineNodiscardCXPR u32_t            module_position()                 const noexcept { assert(type == MODULE_ACCESS); return position_in_file - module_access_data.module_length; }

#define pre assert(type == MODULE_ACCESS);
  edenInlineNodiscardCXPR std::string_view full_module_access(File const& file) const noexcept { pre return file.view_at(module_access_data.module_length + module_access_data.member_length + 1, module_position()); }
#undef pre

#define pre assert(type == MODULE_ACCESS);
  edenInlineNodiscardCXPR std::string_view module_name(File const& file) const noexcept { pre return file.view_at(module_access_data.module_length, module_position()); }
#undef pre

#define pre assert(type == MODULE_ACCESS);
  edenInlineNodiscardCXPR std::string_view module_member_name(File const& file) const noexcept { pre return file.view_at(module_access_data.member_length, position_in_file + 1); }
#undef pre

};

inline constexpr ASTNode PLACEHOLDER_NODE{
    .type = ASTNode::EMPTY,
    .file_idx = 0,
    .length_in_file = 0,
    .position_in_file = 0,
    .base = 0
};

void print_ast(eden::vector<ASTNode> const& nodes, File const&) noexcept;

}