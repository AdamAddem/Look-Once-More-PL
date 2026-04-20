#pragma once
#include "edenlib/typedefs.hpp"
#include "edenlib/releasing_vector.hpp"
#include "semantic_analysis/symbol_table.hpp"
#include "settings.hpp"

namespace LOM::Parser {
struct TU;
}

namespace LOM::PeepMIR {
using released_string = eden::releasing_string::released_span;

template <class T>
using released_span = eden::releasing_vector<T>::released_span;

template<class T>
using released_ptr = eden::releasing_vector<T>::released_ptr;

struct Instruction {
  enum Type : u8_t {
    NOOP,
    GLOBAL, //value is char*
    LOCAL, //value is local idx
    FUNCTION, //value is char*

    //look you've read my codebase you get the memo by now
    INT_LITERAL, UINT_LITERAL, FLOAT_LITERAL, DOUBLE_LITERAL, BOOL_LITERAL, CHAR_LITERAL, STRING_LITERAL,

    //value indeterminate for operators
    ADD, FADD,
    SUB, FSUB,
    MULT, FMULT,
    UDIV, SDIV, FDIV,
    UMOD, SMOD, FMOD,
    ASSIGN,
    ULESS, SLESS, FLESS,
    UGTR, SGTR, FGTR,
    ULEQ, SLEQ, FLEQ,
    UGEQ, SGEQ, FGEQ,
    EQ, NEQ, AND, OR,
    BITAND, BITOR, BITXOR,
    PRE_INC, FPRE_INC,
    PRE_DEC, FPRE_DEC,
    ADDRESS_OF,
    NEGATE, FNEGATE,
    BITNOT,
    POST_INC, FPOST_INC,
    POST_DEC, FPOST_DEC,

    //value contains bitwidth to extend / truncate to
    UCAST, SCAST, FCAST,

    CALL // value equals number of parameters
  }type;
  u64_t value;

  constexpr Instruction(Type type, u64_t value) noexcept
  : type(type), value(value) {}

  [[nodiscard]] constexpr i64_t
  int_value() const noexcept
  {assume_assert(type == INT_LITERAL); return std::bit_cast<i64_t>(value);}

  [[nodiscard]] constexpr u64_t
  uint_value() const noexcept
  {assume_assert(type == UINT_LITERAL); return value;}

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

  [[nodiscard]] constexpr char*
  string_value() const noexcept
  {assume_assert(type == STRING_LITERAL); return std::bit_cast<char*>(value);}

  [[nodiscard]] constexpr char*
  global_name() const noexcept
  {assume_assert(type == GLOBAL); return std::bit_cast<char*>(value);}

  [[nodiscard]] constexpr u64_t
  local_idx() const noexcept
  {assume_assert(type == LOCAL); return value;}

  [[nodiscard]] constexpr char*
  function_name() const noexcept
  {assume_assert(type == FUNCTION); return std::bit_cast<char*>(value);}

  [[nodiscard]] constexpr u64_t
  num_params() const noexcept
  {assume_assert(type == CALL); return value;}

  [[nodiscard]] constexpr eden::releasing_string::released_ptr
  release_string_value() noexcept {
    assert(type == GLOBAL or type == FUNCTION or type == STRING_LITERAL);
    auto x = eden::releasing_string::released_ptr(std::bit_cast<char*>(value));
    value = 0;
    return x;
  }
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
  released_string name;
  const FunctionType* type;
  released_span<const Type*> locals;
  released_ptr<Instruction> instructions;
  released_span<Block> blocks;
};

struct TU {
  explicit TU(TypeContext&& types) noexcept
  : types(std::move(types)) {}
  TU(TU&& other) noexcept = default;

  TypeContext types;
  released_ptr<const Type*> globals;
  released_span<Instruction> global_instructions;
  std::vector<Function> functions;

  ~TU() {
    globals.destroy_and_deallocate();
    global_instructions.destroy_and_deallocate();
  }
};

TU lowerToPeep(Parser::TU &&);

};