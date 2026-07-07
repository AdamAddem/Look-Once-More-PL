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
  enum Type : u8_t {
    NOOP,

    GLOBAL, FUNCTION,   // value is char* to name, aux_value is name length

    MODULE_GLOBAL,      // value is StabilizedModule, aux_value is order of global in module
    MODULE_FUNCTION,    // value is StabilizedModule, aux_value is order of function in module
    TYPE_VARIABLE,      // value is CustomType const*, aux_value is order of variable in type

    LOCAL,              // value is local idx

    // value contains bitwise representation of value
    I8_LITERAL, I16_LITERAL, I32_LITERAL, I64_LITERAL,
    U8_LITERAL, U16_LITERAL, U32_LITERAL, U64_LITERAL,
    FLOAT_LITERAL, DOUBLE_LITERAL,
    BOOL_LITERAL, CHAR_LITERAL,

    STRING_LITERAL,         // value is char* to string, aux_value is string length
    ESCAPED_STRING_LITERAL, // value is char* to string, aux_value is string length

    // value indeterminate
    ADD, FADD,
    SUB, FSUB,
    MULT, FMULT,
    UDIV, SDIV, FDIV,
    UMOD, SMOD, FMOD,
    ASSIGN,
    UCAST_ASSIGN, SCAST_ASSIGN, // value is the bitwidth to cast right side to
    ULESS, SLESS, FLESS,
    UGTR, SGTR, FGTR,
    ULEQ, SLEQ, FLEQ,
    UGEQ, SGEQ, FGEQ,
    EQ, NEQ, AND, OR,
    BITAND, BITOR, BITXOR,

    SUBSCRIPT, // value is ArrayType const*

    // value indeterminate
    PRE_INC, FPRE_INC,
    PRE_DEC, FPRE_DEC,
    ADDRESS_OF,
    NEGATE, FNEGATE,
    BITNOT,
    POST_INC, FPOST_INC,
    POST_DEC, FPOST_DEC,
    DEREFERENCE, // value contains pointed type

    // value contains destination type
    UCAST, SCAST, FCAST, PCAST,

    CALL // value equals number of parameters
  }type;

  u8_t file_idx;
  // char _pad[2];
  u32_t aux_value;
  u64_t value;

  constexpr Instruction(Type type, u64_t value) noexcept
  : type(type), value(value)
  { assert( not eden::enumBetween(type, MODULE_GLOBAL, TYPE_VARIABLE) ); }

  constexpr Instruction(Type type, std::string_view str) noexcept
  : type(type), aux_value(static_cast<u32_t>(str.length())), value(std::bit_cast<u64_t>(str.data()))
  { assert(type == GLOBAL or type == FUNCTION or type == STRING_LITERAL or type == ESCAPED_STRING_LITERAL); }

  constexpr Instruction(Type type, Module const* module, u32_t order) noexcept
  : type(type), aux_value(order), value(std::bit_cast<u64_t>(module))
  { assert( eden::enumBetween(type, MODULE_GLOBAL, MODULE_FUNCTION) ); }

  constexpr Instruction(Type type, CustomType const* table, u32_t order) noexcept
  : type(type), aux_value(order), value(std::bit_cast<u64_t>(table))
  { assume_assert( type == TYPE_VARIABLE ); }

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
  escaped_string_value() const noexcept {
    assume_assert(type == ESCAPED_STRING_LITERAL);
    std::string res;
    auto const orig = original_string();
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

  eden_always_inline [[nodiscard]] constexpr std::string_view             global_name()          const noexcept { assume_assert(type == GLOBAL); return {std::bit_cast<const char*>(value), aux_value}; }
  eden_always_inline [[nodiscard]] constexpr std::string_view             function_name()        const noexcept { assume_assert(type == FUNCTION); return {std::bit_cast<const char*>(value), aux_value}; }

  eden_always_inline [[nodiscard]] constexpr StabilizedModule             module()               const noexcept { assert(eden::enumBetween(type, MODULE_GLOBAL, MODULE_FUNCTION));  return std::bit_cast<const Module*>(value); }
  eden_always_inline [[nodiscard]] constexpr CustomType const*            custom_type()          const noexcept { assert(type == TYPE_VARIABLE);  return std::bit_cast<CustomType const*>(value); }
  eden_always_inline [[nodiscard]] constexpr u16_t                        module_global_id()     const noexcept { assume_assert(type == MODULE_GLOBAL); return static_cast<u16_t>(aux_value); }
  eden_always_inline [[nodiscard]] constexpr SymbolTable::Variable const& module_global()        const noexcept { assume_assert(type == MODULE_GLOBAL); return *module().getVariable(module_global_id()); }
  eden_always_inline [[nodiscard]] constexpr u16_t                        module_function_id()   const noexcept { assume_assert(type == MODULE_FUNCTION); return static_cast<u16_t>(aux_value); }
  eden_always_inline [[nodiscard]] constexpr SymbolTable::Function const& module_function()      const noexcept { assume_assert(type == MODULE_FUNCTION); return *module().getFunction(module_function_id()); }
  eden_always_inline [[nodiscard]] constexpr u16_t                        type_variable_id()     const noexcept { assume_assert(type == TYPE_VARIABLE); return static_cast<u16_t>(aux_value); }
  eden_always_inline [[nodiscard]] constexpr SymbolTable::Variable const& type_variable()        const noexcept { assume_assert(type == TYPE_VARIABLE); return *custom_type()->member_table()->getVariable(type_variable_id()); }

  eden_always_inline [[nodiscard]] constexpr u64_t                        local_idx()            const noexcept { assume_assert(type == LOCAL); return value; }

  eden_always_inline [[nodiscard]] constexpr i64_t                        int_value()            const noexcept { assert(eden::enumBetween(type, I8_LITERAL, I64_LITERAL)); return std::bit_cast<i64_t>(value); }
  eden_always_inline [[nodiscard]] constexpr u64_t                        uint_value()           const noexcept { assert(eden::enumBetween(type, U8_LITERAL, U64_LITERAL)); return value; }
  eden_always_inline [[nodiscard]] constexpr float                        float_value()          const noexcept { assume_assert(type == FLOAT_LITERAL); return std::bit_cast<float>(static_cast<u32_t>(value)); }
  eden_always_inline [[nodiscard]] constexpr double                       double_value()         const noexcept { assume_assert(type == DOUBLE_LITERAL); return std::bit_cast<double>(value); }
  eden_always_inline [[nodiscard]] constexpr bool                         bool_value()           const noexcept { assume_assert(type == BOOL_LITERAL); return value; }
  eden_always_inline [[nodiscard]] constexpr char                         char_value()           const noexcept { assume_assert(type == CHAR_LITERAL); return static_cast<char>(value); }
  eden_always_inline [[nodiscard]] constexpr std::string_view             string_value()         const noexcept { assume_assert(type == STRING_LITERAL); return {std::bit_cast<const char*>(value), aux_value}; }

  eden_always_inline [[nodiscard]] constexpr ArrayType const*             array_type()           const noexcept { assume_assert(type == SUBSCRIPT); return std::bit_cast<ArrayType const*>(value); }

  eden_always_inline [[nodiscard]] constexpr u64_t                        num_params()           const noexcept { assume_assert(type == CALL); return value; }
  eden_always_inline [[nodiscard]] constexpr LOM::Type const*             dereference_type()     const noexcept { assume_assert(type == DEREFERENCE); return std::bit_cast<LOM::Type const*>(value); }
  eden_always_inline [[nodiscard]] constexpr u64_t                        cast_assign_bitwidth() const noexcept { assume_assert(type == UCAST_ASSIGN or type == SCAST_ASSIGN); return value; }
  eden_always_inline [[nodiscard]] constexpr LOM::Type const*             cast_type()            const noexcept { assert(eden::enumBetween(type, UCAST, PCAST)); return std::bit_cast<LOM::Type const*>(value); }

  eden_always_inline [[nodiscard]] constexpr std::string_view             original_string() const noexcept { return {std::bit_cast<const char*>(value), aux_value}; }
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
  eden::swap_vector<Module*> imports;
  std::vector<Function> functions;
};

void printPeep(TU const&);

// Populates tu and returns whether an error was encountered.
[[nodiscard]] bool lowerToPeep(TU& tu, Parser::TU&& parsed_tu);

};