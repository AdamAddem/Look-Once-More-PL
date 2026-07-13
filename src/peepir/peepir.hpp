#pragma once
#include "edenlib/typedefs.hpp"
#include "error.hpp"
#include "file.hpp"
#include "semantic_analysis/symbol_table.hpp"

namespace LOM::Parser {
struct TU;
}

namespace LOM::PeepIR {

[[nodiscard]] constexpr char
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
    TYPE_VARIABLE,      // value is CustomType const*, aux_value is order of variable in type

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
    EQ, NEQ, AND, OR,
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
  }type;
  using enum InstructionType;

  // structured this way to abuse the common subsequence exception for unions
  // structs dont inherit from CommonData as that would disqualify them from being standard-layout which is a requirement for this to work
#define common_subsequence InstructionType type; u8_t file_idx; u16_t length_in_file; u32_t position_in_file;
  struct CommonData          { common_subsequence };

  struct ModuleSymbolData    { common_subsequence u32_t dot_pos; u32_t idx; };
  struct TypeMemberData      { InstructionType type; u8_t file_idx; u16_t idx_in_type; u32_t position_in_file; CustomType const* custom_type; };
  struct LocalData           { common_subsequence u32_t idx; };

  struct SignedLiteralData   { common_subsequence i64_t  value; };
  struct UnsignedLiteralData { common_subsequence u64_t  value; };
  struct FloatLiteralData    { common_subsequence float  value; };
  struct DoubleLiteralData   { common_subsequence double value; };
  struct BoolLiteralData     { common_subsequence bool   value; };
  struct CharLiteralData     { common_subsequence char   value; };

  struct CastAssignData      { common_subsequence u32_t bitwidth; };
  struct CastData            { common_subsequence Type const* destination_type; };
  struct SubscriptData       { common_subsequence ArrayType const* array_type;  };
  struct DereferenceData     { common_subsequence Type const* dereference_type; };
  struct CallData            { common_subsequence u32_t num_parameters; };

#undef common_subsequence

  static_assert(alignof(QualifiedType) == 8);
  static_assert(sizeof(QualifiedType) == 16);
  union {
    CommonData m;
    ModuleSymbolData          module_symbol_data;
    TypeMemberData            type_member_data;
    LocalData                 local_data;
    SignedLiteralData         signed_literal_data;
    UnsignedLiteralData       unsigned_literal_data;
    FloatLiteralData          float_literal_data;
    DoubleLiteralData         double_literal_data;
    BoolLiteralData           bool_literal_data;
    CharLiteralData           char_literal_data;
    CastAssignData            cast_assign_data;
    CastData                  cast_data;
    SubscriptData             subscript_data;
    DereferenceData           dereference_data;
    CallData                  call_data;
  };


  explicit constexpr Instruction(CommonData data) : m(data) {}
  explicit constexpr Instruction(InstructionType type, u8_t file_idx) { m.type = type; m.file_idx = file_idx; }
  eden_always_inline [[nodiscard]] constexpr bool is_literal() const noexcept { return eden::enumBetween(type, I8_LITERAL, U64_LITERAL); }

  constexpr void
  adjust_literal(u64_t bitwidth, bool make_signed) noexcept {
    assert(is_literal());
    switch (bitwidth) {
    case 8:   type = make_signed ? I8_LITERAL : U8_LITERAL; return;
    case 16:  type = make_signed ? I16_LITERAL : U16_LITERAL; return;
    case 32:  type = make_signed ? I32_LITERAL : U32_LITERAL; return;
    case 64:  type = make_signed ? I64_LITERAL : U64_LITERAL; return;
    default:
      std::unreachable();
    }
  }

  [[nodiscard]] constexpr std::string
  escaped_string_value(File file) const noexcept {
    assume_assert(type == ESCAPED_STRING_LITERAL);
    std::string res;
    auto const orig = original_string(file);
    res.reserve(orig.size() + 1);

    for (auto i{0uz}; i < orig.size(); ++i) {
      char c = orig[i];
      if (c == '\\') {
        ++i;
        c = charToEscapeSequenceEquivalent(orig[i]);
      }
      res.push_back(c);
    }

    return res;
  }

  eden_always_inline [[nodiscard]] constexpr std::string_view original_string(File file) const noexcept { return file.view_at(m.length_in_file, m.position_in_file); }
};

class Block {
  struct br_data {  u32_t next_block_idx; };
  struct brc_data { u32_t true_block_idx; u32_t false_block_idx; };
public:

  u32_t first_instruction_idx;
  enum class Terminator : u32_t {NONE, BR, BRC, RET} //when done peeping, there should be no ret besides the last block
  terminator_type;

  union {
    br_data br;
    brc_data brc;
  };

  constexpr void
  set_brc(u32_t true_block_idx, u32_t false_block_idx) noexcept {
    if (terminator_type not_eq Terminator::RET) {
      terminator_type = Terminator::BRC;
      brc = {true_block_idx, false_block_idx};
    }
  }

  constexpr void
  set_br(u32_t next_block_idx) noexcept {
    if (terminator_type not_eq Terminator::RET) {
      terminator_type = Terminator::BR;
      br.next_block_idx = next_block_idx;
    }
  }

  constexpr void
  set_ret() noexcept {
    if (terminator_type == Terminator::NONE)
      terminator_type = Terminator::RET;
  }
};

struct Function {
  bool is_public;
  u8_t file_idx;
  // char _pad[2];
  u32_t name_len;
  const char* name_ptr;

  FunctionType const* type;
  std::vector<Type const*> locals;
  std::vector<Instruction> instructions;
  std::vector<Block> blocks;

  eden_always_inline [[nodiscard]] std::string_view nameof() const noexcept { return {name_ptr, name_len}; }
};

struct TU {
  std::vector<File> source_files;
  Module* module;
  std::vector<std::string_view> imports;
  std::vector<Function> functions;
  std::string_view name;
};

void printPeep(TU const&);

// Populates tu and returns whether an error was encountered.
[[nodiscard]] bool lowerToPeep(TU& tu, Parser::TU&& parsed_tu);

};