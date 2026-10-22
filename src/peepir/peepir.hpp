#pragma once
#include "edenlib/typedefs.hpp"
#include "edenlib/vectors/vector.hpp"

#include "error.hpp"
#include "file.hpp"
#include "generic_tu.hpp"
#include "module/table_and_module.hpp"

namespace LOM::Parser {
struct TU;
}

namespace LOM::PeepIR {

NodiscardCXPR static char
charToEscapeSequenceEquivalent(char c) {
  switch (c) {
  case 'n':   return '\n';
  case 't':   return '\t';
  case 'b':   return '\b';
  case 'r':   return '\r';
  case 'f':   return '\f';
  case '\\':  return '\\';
  case '"':   return '"';
  case '\'':  return '\'';
  case '0':   return '\0';
  case 'v':   return '\v';
  default:    return c;
  }
}

struct Instruction {
  enum class InstructionType : u8_t {
    NOOP,

    GLOBAL, FUNCTION,

    MODULE_GLOBAL,
    MODULE_FUNCTION,
    TYPE_VARIABLE,

    LOCAL,

    I8_LITERAL, I16_LITERAL, I32_LITERAL, I64_LITERAL,
    U8_LITERAL, U16_LITERAL, U32_LITERAL, U64_LITERAL,
    FLOAT_LITERAL, DOUBLE_LITERAL,
    BOOL_LITERAL, CHAR_LITERAL,

    STRING_LITERAL, ESCAPED_STRING_LITERAL,

    ADD, FADD,
    SUB, FSUB,
    MULT, FMULT,
    UDIV, SDIV, FDIV,
    UMOD, SMOD, FMOD,
    ASSIGN,
    UCAST_ASSIGN, SCAST_ASSIGN,
    ULESS, SLESS, FLESS,
    UGTR, SGTR, FGTR,
    ULEQ, SLEQ, FLEQ,
    UGEQ, SGEQ, FGEQ,
    EQ, NEQ,

    AND, OR,
    BITAND, BITOR, BITXOR,

    SUBSCRIPT,

    PRE_INC, FPRE_INC,
    PRE_DEC, FPRE_DEC,
    ADDRESS_OF,
    NEGATE, FNEGATE,
    BITNOT,
    POST_INC, FPOST_INC,
    POST_DEC, FPOST_DEC,
    DEREFERENCE,

    UCAST, SCAST, FCAST, PCAST,

    CALL
  };
  using enum InstructionType;

  struct ModuleAccessData    { u16_t module_id; u16_t member_id; /* byte_t _pad[4]; */ };
  struct TypeAccessData      { u32_t custom_type_id; u16_t custom_type_module_id; u16_t member_id; /* byte_t _pad[2]; */ };
  struct LocalData           { u32_t idx; /* byte_t _pad[4]; */ };
  struct SignedLiteralData   { i64_t value; };
  struct UnsignedLiteralData { u64_t value; };
  struct FloatLiteralData    { f32_t value; /* byte_t _pad[4]; */ };
  struct DoubleLiteralData   { f64_t value; };
  struct BoolLiteralData     { bool  value; /* byte_t _pad[7]; */ };
  struct CharLiteralData     { char  value; /* byte_t _pad[7]; */ };
  struct CastAssignData      { u32_t bitwidth; /* byte_t _pad[4]; */ };
  struct CastData            { TypeID destination_typeID; };
  struct SubscriptData       { TypeID array_typeID;  };
  struct DereferenceData     { TypeID dereference_typeID; };
  struct CallData            { u32_t num_parameters; /* byte_t _pad[4]; */ };

  InstructionType type; u8_t file_idx; u16_t length; u32_t position;
  union {
    u64_t _{};
    ModuleAccessData    module_member_data;
    TypeAccessData      type_member_data;
    LocalData           local_data;
    SignedLiteralData   signed_literal_data;
    UnsignedLiteralData unsigned_literal_data;
    FloatLiteralData    float_literal_data;
    DoubleLiteralData   double_literal_data;
    BoolLiteralData     bool_literal_data;
    CharLiteralData     char_literal_data;
    CastAssignData      cast_assign_data;
    CastData            cast_data;
    SubscriptData       subscript_data;
    DereferenceData     dereference_data;
    CallData            call_data;
  };

  InlineCXPR Instruction() noexcept {}
  InlineCXPR Instruction(InstructionType type, u8_t file_idx, u16_t length, u32_t position) noexcept
  : type(type), file_idx(file_idx), length(length), position(position) {}

  InlineCXPR explicit Instruction(InstructionType type) noexcept : type(type) {}
  InlineNodiscardCXPR bool is_literal() const noexcept { return eden::enumBetween(type, I8_LITERAL, ESCAPED_STRING_LITERAL); }

#define pre assert(is_literal());
  constexpr void adjust_literal(u64_t bitwidth, bool make_signed) noexcept { pre
    switch (bitwidth) {
    case 8:   type = make_signed ? I8_LITERAL : U8_LITERAL; return;
    case 16:  type = make_signed ? I16_LITERAL : U16_LITERAL; return;
    case 32:  type = make_signed ? I32_LITERAL : U32_LITERAL; return;
    case 64:  type = make_signed ? I64_LITERAL : U64_LITERAL; return;
    default:
      std::unreachable();
    }
  }
#undef pre

// expensive and poopy
#define pre Assume(type == ESCAPED_STRING_LITERAL);
  NodiscardCXPR std::string escaped_string_value(File file) const noexcept { pre
    std::string res;
    auto const orig = original_string(file);
    res.reserve(orig.size() + 1);
    for (sz_t i{}; i < orig.size(); ++i) {
      char c = orig[i];
      if (c == '\\') {
        ++i;
        c = charToEscapeSequenceEquivalent(orig[i]);
      }
      res.push_back(c);
    }
    return res;
  }
#undef pre

#define pre Assume(type != MODULE_FUNCTION and type != MODULE_GLOBAL); // stupid, but they both only store the length and pos of the
  InlineNodiscardCXPR std::string_view original_string(File file) const noexcept { pre return file.viewAt(length, position); }
#undef pre

#define pre Assume(type == TYPE_VARIABLE);
  InlineNodiscardCXPR TypeID custom_typeID() const noexcept { pre return TypeID{ .derived = Type::CUSTOM, .module_id = type_member_data.custom_type_module_id, .id = type_member_data.custom_type_id }; }
#undef pre

};
static_assert(alignof(Instruction) == 8);
static_assert(sizeof(Instruction) == 16);

class alignas(8) Block {
  struct br_data  { i16_t next_block_offset{}; };
  struct brc_data { i16_t true_block_offset{}; i16_t false_block_offset{}; };
public:

  u16_t num_instructions{};
  enum class Terminator : u16_t {NONE, BR, BRC, RET} // when done peeping, there should be no ret besides the last block
  terminator_type{Terminator::NONE};

  constexpr Block() = default;

  union {
    brc_data brc{};
    br_data br;
  };

  constexpr void
  end_with_brc(i16_t true_block_offset, i16_t false_block_offset) noexcept {
    auto const not_ret = terminator_type != Terminator::RET;
    terminator_type = not_ret ? Terminator::BRC : terminator_type;
    brc.true_block_offset = true_block_offset;
    brc.false_block_offset = false_block_offset;
  }

  constexpr void
  end_with_br(i16_t next_block_offset) noexcept {
    auto const not_ret = terminator_type != Terminator::RET;
    terminator_type = not_ret ? Terminator::BR : terminator_type;
    br.next_block_offset = next_block_offset;
  }

  constexpr void end_with_ret() noexcept
  { terminator_type = Terminator::RET; }

};
static_assert(sizeof(Block) == 8);

struct Function {
  eden::vector16<Instruction> instructions;
  eden::vector16<Block> blocks;

}; static_assert(sizeof(Function) == FUNCTION_SIZE); static_assert(alignof(Function) == FUNCTION_ALIGN);

struct TU {
  GENERIC_TU_DEF
  eden::vector<Function> functions;
}; static_assert(sizeof(TU) == TU_SIZE); static_assert(alignof(TU) == TU_ALIGN);

void printPeep(TU const&);

// Transmutates parsed_tu into a peeped_tu
[[nodiscard]] bool lowerToPeep(Parser::TU& parsed_tu);

};