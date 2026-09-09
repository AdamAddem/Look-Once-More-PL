#pragma once
#include "edenlib/typedefs.hpp"
#include "edenlib/vectors/vector.hpp"

#include "error.hpp"
#include "file.hpp"
#include "module/table_and_module.hpp"

namespace LOM::Parser {
struct TU;
}

namespace LOM::PeepIR {

edenNodiscardCXPR static char
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
  };
  using enum InstructionType;

  // structured this way to abuse the common subsequence exception for unions
  // structs dont inherit from CommonData as that would disqualify them from being standard-layout which is a requirement for this to work
  InstructionType type; u8_t file_idx; u16_t length_in_file; u32_t position_in_file;
  struct ModuleMemberData    { u16_t module_id; u16_t member_id; };
  struct TypeMemberData      { u32_t custom_type_id; u16_t custom_type_module_id; u16_t member_id; };

  struct LocalData           { u32_t idx; /* byte_t _extra[4]; */  };

  struct SignedLiteralData   { i64_t value; };
  struct UnsignedLiteralData { u64_t value; };
  struct FloatLiteralData    { f32_t value; /* byte_t _extra[4]; */  };
  struct DoubleLiteralData   { f64_t value; };
  struct BoolLiteralData     { bool  value; /* byte_t _extra[7]; */  };
  struct CharLiteralData     { char  value; /* byte_t _extra[7]; */  };

  struct CastAssignData      { u32_t bitwidth; /* byte_t _extra[4]; */  };
  struct CastData            { TypeID destination_typeID; };
  struct SubscriptData       { TypeID array_typeID;  };
  struct DereferenceData     { TypeID dereference_typeID; };
  struct CallData            { u32_t num_parameters; /* byte_t _extra[4]; */ };
#undef common_subsequence

  union {
    ModuleMemberData    module_member_data;
    TypeMemberData      type_member_data;
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

  edenInlineCXPR explicit Instruction() noexcept {}
  edenInlineCXPR explicit Instruction(InstructionType type, u8_t file_idx, u16_t length_in_file, u32_t position_in_file) noexcept
  : type(type), file_idx(file_idx), length_in_file(length_in_file), position_in_file(position_in_file) {}

  edenInlineCXPR explicit Instruction(InstructionType type) noexcept { this->type = type; }
  edenInlineNodiscardCXPR bool is_literal() const noexcept { return eden::enumBetween(type, I8_LITERAL, U64_LITERAL); }

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

#define pre assert(type == ESCAPED_STRING_LITERAL);
  edenNodiscardCXPR std::string escaped_string_value(File file) const noexcept { pre
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
#undef pre

#define pre assert(type not_eq TYPE_VARIABLE and type not_eq MODULE_GLOBAL and type not_eq MODULE_FUNCTION);
  edenInlineNodiscardCXPR std::string_view original_string(File file) const noexcept { pre return file.view_at(length_in_file, position_in_file); }
#undef pre

#define pre assert(type == MODULE_GLOBAL);
  edenInlineNodiscardCXPR std::string_view module_variable_name() const noexcept { pre return getModule(module_member_data.module_id).getVariable(module_member_data.member_id).nameof(); }
#undef pre

#define pre assert(type == MODULE_FUNCTION);
  edenInlineNodiscardCXPR std::string_view module_function_name() const noexcept { pre return getModule(module_member_data.module_id).getFunction(module_member_data.member_id).nameof(); }
#undef pre

#define pre assert(type == MODULE_GLOBAL or type == MODULE_FUNCTION);
  edenInlineNodiscardCXPR std::string_view module_name() const noexcept { pre return getModule(module_member_data.module_id).nameof(); }
#undef pre

#define pre assert(type == TYPE_VARIABLE);
  edenInlineNodiscardCXPR TypeID custom_typeID() const noexcept { pre return TypeID{ .derived = Type::CUSTOM, .module_id = type_member_data.custom_type_module_id, .id = type_member_data.custom_type_id }; }
#undef pre

};
static_assert(alignof(Instruction) == 8);
static_assert(sizeof(Instruction) == 16);

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
//byte_t _pad[2];
  u32_t name_len;
  const char* name_ptr;

  TypeID typeID;
  eden::vector<TypeID> locals;
  eden::vector<Instruction> instructions;
  eden::vector<Block> blocks;

  edenAlwaysInline [[nodiscard]] std::string_view nameof() const noexcept { return {name_ptr, name_len}; }
};

struct TU {
  eden::vector<File> source_files;
  eden::vector<Function> functions;
  std::string_view name;
  Module* module;
};

void printPeep(TU const&);

// Populates tu and returns whether an error was encountered.
[[nodiscard]] bool lowerToPeep(TU& tu, Parser::TU&& parsed_tu);

};