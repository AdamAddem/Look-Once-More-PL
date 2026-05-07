#pragma once
#include "edenlib/releasing_vector.hpp"
#include "edenlib/typedefs.hpp"
#include "semantic_analysis/symbol_table.hpp"

namespace LOM::Parser {
struct TU;
}

namespace LOM::PeepMIR {

struct Instruction {
  enum Type : u8_t {
    NOOP,
    GLOBAL, //value is char* to global name
    FUNCTION, //value is char* to function name
    IMPORTED_GLOBAL, //value is char* to global name (module1.module2.name)
    IMPORTED_FUNCTION, //value is char* to function name (module1.module2.name)

    LOCAL, //value is local idx

    //same as AST
    I8_LITERAL, I16_LITERAL, I32_LITERAL, I64_LITERAL,
    U8_LITERAL, U16_LITERAL, U32_LITERAL, U64_LITERAL,
    FLOAT_LITERAL,
    DOUBLE_LITERAL,
    BOOL_LITERAL,
    CHAR_LITERAL,
    STRING_LITERAL,

    //value indeterminate
    ADD, FADD,
    SUB, FSUB,
    MULT, FMULT,
    UDIV, SDIV, FDIV,
    UMOD, SMOD, FMOD,
    ASSIGN,
    UCAST_ASSIGN, SCAST_ASSIGN, //contains the bitwidth to cast right side to
    ULESS, SLESS, FLESS,
    UGTR, SGTR, FGTR,
    ULEQ, SLEQ, FLEQ,
    UGEQ, SGEQ, FGEQ,
    EQ, NEQ, AND, OR,
    BITAND, BITOR, BITXOR,

    //value indeterminate
    PRE_INC, FPRE_INC,
    PRE_DEC, FPRE_DEC,
    ADDRESS_OF,
    NEGATE, FNEGATE,
    BITNOT,
    POST_INC, FPOST_INC,
    POST_DEC, FPOST_DEC,
    DEREFERENCE, //contains pointed type

    //value contains destination type
    UCAST, SCAST, FCAST, PCAST,

    CALL // value equals number of parameters
  }type;
  u64_t value;

  constexpr Instruction(Type type, u64_t value) noexcept
  : type(type), value(value) {}

  [[nodiscard]] constexpr bool
  is_literal() const noexcept {return eden::enumBetween(type, I8_LITERAL, U64_LITERAL);}

  constexpr void
  adjust_literal(u64_t bitwidth, bool make_signed) noexcept {
    assert(is_literal());
    switch (bitwidth) {
    case 8:
      type = make_signed ? I8_LITERAL : U8_LITERAL; return;
    case 16:
      type = make_signed ? I16_LITERAL : U16_LITERAL; return;
    case 32:
      type = make_signed ? I32_LITERAL : U32_LITERAL; return;
    case 64:
      type = make_signed ? I64_LITERAL : U64_LITERAL; return;
    default:
      std::unreachable();
    }
  }

  [[nodiscard]] constexpr i64_t
  int_value() const noexcept
  {assert(eden::enumBetween(type, I8_LITERAL, I64_LITERAL)); return std::bit_cast<i64_t>(value);}

  [[nodiscard]] constexpr u64_t
  uint_value() const noexcept
  {assert(eden::enumBetween(type, U8_LITERAL, U64_LITERAL)); return value;}

  [[nodiscard]] constexpr float
  float_value() const noexcept
  {assume_assert(type == FLOAT_LITERAL); return std::bit_cast<float>(static_cast<u32_t>(value));}

  [[nodiscard]] constexpr double
  double_value() const noexcept
  {assume_assert(type == DOUBLE_LITERAL); return std::bit_cast<double>(value);}

  [[nodiscard]] constexpr bool
  bool_value() const noexcept
  {assume_assert(type == BOOL_LITERAL); return value;}

  [[nodiscard]] constexpr char
  char_value() const noexcept
  {assume_assert(type == CHAR_LITERAL); return static_cast<char>(value);}

  eden_return_nonnull
  [[nodiscard]] constexpr char*
  string_value() const noexcept
  {assume_assert(type == STRING_LITERAL); return std::bit_cast<char*>(value);}

  eden_return_nonnull
  [[nodiscard]] constexpr char*
  global_name() const noexcept
  {assume_assert(type == GLOBAL); return std::bit_cast<char*>(value);}

  eden_return_nonnull
  [[nodiscard]] constexpr char*
  imported_global_name() const noexcept
  {assume_assert(type == IMPORTED_GLOBAL); return std::bit_cast<char*>(value);}

  [[nodiscard]] constexpr u64_t
  local_idx() const noexcept
  {assume_assert(type == LOCAL); return value;}

  eden_return_nonnull
  [[nodiscard]] constexpr char*
  function_name() const noexcept
  {assume_assert(type == FUNCTION); return std::bit_cast<char*>(value);}

  eden_return_nonnull
  [[nodiscard]] constexpr char*
  imported_function_name() const noexcept
  {assume_assert(type == IMPORTED_FUNCTION); return std::bit_cast<char*>(value);}

  [[nodiscard]] constexpr u64_t
  num_params() const noexcept
  {assume_assert(type == CALL); return value;}

  eden_return_nonnull
  [[nodiscard]] constexpr const LOM::Type*
  dereference_type() const noexcept
  {assume_assert(type == DEREFERENCE); return std::bit_cast<const LOM::Type*>(value);}

  [[nodiscard]] constexpr u64_t
  cast_assign_bitwidth() const noexcept
  {assume_assert(type == UCAST_ASSIGN or type == SCAST_ASSIGN); return value;}

  [[nodiscard]] constexpr const LOM::Type*
  cast_type() const noexcept
  {assert(eden::enumBetween(type, UCAST, PCAST)); return std::bit_cast<const LOM::Type*>(value);}

};

class Block {
  struct br_data {
    u32_t next_block_idx;
  };
  struct brc_data {
    u32_t true_block_idx; u32_t false_block_idx;
  };
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
    if (terminator_type == Terminator::NONE) {
      terminator_type = Terminator::BRC;
      brc = {true_block_idx, false_block_idx};
    }
  }

  constexpr void
  set_br(u32_t next_block_idx) noexcept {
    if (terminator_type == Terminator::NONE) {
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
  std::string_view name;
  const FunctionType* type;
  std::vector<const Type*> locals;
  std::vector<Instruction> instructions;
  std::vector<Block> blocks;
  bool is_public;
};

struct TU {
  explicit TU(const Parser::TU&) noexcept;

  std::string_view name;
  Module* table;

  std::unordered_map<std::string_view, Module*> imports;
  std::vector<Function> functions;
};

void printPeep(TU&);

[[nodiscard]] TU lowerToPeep(Parser::TU &&);

};