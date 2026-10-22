#pragma once
#include "file.hpp"
#include "module/types.hpp"

#include "edenlib/macros.hpp"
#include "edenlib/vectors/vector.hpp"
#include "edenlib/typedefs.hpp"
#include <cassert>
#include <string>
#include <utility>

namespace LOM::Parser {
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
    MODULE_ACCESS,  //
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

  struct DeclarationData  { TypeID typeID; }; // will have the length and position in the file of the declared identifier
  struct IfData           { u32_t num_substatements; bool has_else; /* byte_t _pad[3]; */  };
  struct WhileData        { u32_t num_substatements; /* byte_t _pad[4]; */ };
  struct ReturnData       { bool has_value; /* byte_t _pad[7]; */ };
  struct MemberAccessData { /* byte_t _pad[8]; */ };
  struct ModuleAccessData { u16_t module_id; u16_t member_offset; /* byte_t _pad[4]; */ };
  struct UnaryData        { Operator opr; /* byte_t _pad[7]; */  };
  struct BinaryData       { Operator opr; /* byte_t _pad[7]; */  };
  struct CallData         { u32_t num_parameters; /* byte_t _pad[4]; */  };
  struct CastData         { TypeID cast_type; };
  struct SubscriptData    { /* byte_t _pad[8]; */ };
  struct IdentifierData   { /* byte_t _pad[8]; */ };
  struct SignedData       { i64_t value; };
  struct UnsignedData     { u64_t value; };
  struct FloatData        { f32_t value; /* byte_t _pad[4]; */  };
  struct DoubleData       { f64_t value; };
  struct BoolData         { bool value; /* byte_t _pad[7]; */ };
  struct CharData         { char value; /* byte_t _pad[7]; */ };
  struct StringData       { /* byte_t _pad[8]; */ };

  NodeType type; u8_t file_idx; u16_t length; u32_t position;
  union {
    u64_t _;
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

  InlineNodiscardCXPR bool isDeclaration() const noexcept { return eden::enumBetween(type, DECLARATION_JUNK_RO, DECLARATION_RW); }
  InlineNodiscardCXPR bool isRWDeclaration() const noexcept { return type == DECLARATION_RW or type == DECLARATION_JUNK_RW; }
  InlineNodiscardCXPR bool isJunkDeclaration() const noexcept { return eden::enumBetween(type, DECLARATION_JUNK_RO, DECLARATION_JUNK_RW); }

  InlineNodiscardCXPR TypeID declaration_type() const noexcept { assert(isDeclaration()); return declaration_data.typeID; }
  InlineNodiscardCXPR std::string_view decl_identifier_val(File const& file)  const noexcept { assert(isDeclaration()); return file.viewAt(length, position); }

  InlineNodiscardCXPR bool             has_else()                        const noexcept { Assume(type == IF); return if_data.has_else; }
  InlineNodiscardCXPR u64_t            if_numstatements()                const noexcept { Assume(type == IF); return if_data.num_substatements; }
  InlineNodiscardCXPR u64_t            while_numstatements()             const noexcept { Assume(type == WHILE); return while_data.num_substatements; }
  InlineNodiscardCXPR bool             return_has_value()                const noexcept { Assume(type == RETURN); return return_data.has_value; }

  InlineNodiscardCXPR Operator         unary_operator()                  const noexcept { Assume(type == UNARY); return unary_data.opr; }
  InlineNodiscardCXPR Operator         binary_operator()                 const noexcept { Assume(type == BINARY); return binary_data.opr; }
  InlineNodiscardCXPR u64_t            parameter_count()                 const noexcept { Assume(type == CALLING); return call_data.num_parameters; }
  InlineNodiscardCXPR TypeID           cast_type()                       const noexcept { Assume(type == CAST); return cast_data.cast_type; }

  InlineNodiscardCXPR i64_t            signed_val()                      const noexcept { Assume(type == SIGNED_LITERAL); return signed_data.value; }
  InlineNodiscardCXPR u64_t            unsigned_val()                    const noexcept { Assume(type == UNSIGNED_LITERAL); return unsigned_data.value; }
  InlineNodiscardCXPR f32_t            float_val()                       const noexcept { Assume(type == FLOAT_LITERAL); return float_data.value; }
  InlineNodiscardCXPR f64_t            double_val()                      const noexcept { Assume(type == DOUBLE_LITERAL); return double_data.value; }
  InlineNodiscardCXPR bool             bool_val()                        const noexcept { Assume(type == BOOL_LITERAL); return bool_data.value; }
  InlineNodiscardCXPR char             char_val()                        const noexcept { Assume(type == CHAR_LITERAL); return char_data.value; }
  InlineNodiscardCXPR std::string_view identifier_val(File const& file)  const noexcept { Assume(type == IDENTIFIER); return file.viewAt(length, position); }
  InlineNodiscardCXPR std::string_view string_val(File const& file)      const noexcept { Assume(type == STRING_LITERAL or type == ESCAPED_STRING_LITERAL); return file.viewAt(length, position); }

  InlineNodiscardCXPR std::string_view original_string(File const& file) const noexcept { return file.viewAt(length, position); }

#define pre Assume(type == MODULE_ACCESS);
  InlineNodiscardCXPR Module const& accessed_module() const noexcept { pre return getModule(module_access_data.module_id); }

  InlineNodiscardCXPR u32_t module_member_position() const noexcept { pre return position + module_access_data.member_offset; }
  InlineNodiscardCXPR u16_t module_member_length()   const noexcept { pre return length - module_access_data.member_offset; }

  InlineNodiscardCXPR std::string_view module_name(File const& file) const noexcept { pre return file.viewAt(length, position); }

  InlineNodiscardCXPR std::string_view module_member_name(File const& file) const noexcept { pre return file.viewAt(module_member_length(), module_member_position()); }
#undef pre

};

inline constexpr ASTNode PLACEHOLDER_NODE{
    .type = ASTNode::EMPTY,
    .file_idx = 0,
    .length = 0,
    .position = 0,
    ._ = 0
};

}