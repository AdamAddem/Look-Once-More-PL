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

    //literals contain exactly what you think they would
    INT_LITERAL, UINT_LITERAL, FLOAT_LITERAL, DOUBLE_LITERAL, BOOL_LITERAL, CHAR_LITERAL, STRING_LITERAL,

    //value not determined yet for the operators
    ADD, SUB, MULT, DIV, MOD,
    ASSIGN,
    LESS, GTR, LEQ, GEQ,
    AND, OR, BITAND, BITOR, BITXOR, BITNOT,
    EQ, NEQ,
    PRE_INC, PRE_DEC, ADDRESS_OF, NEGATE,
    POST_INC, POST_DEC,

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
  enum class Terminator : u32_t {BR, BRC, RET} //when done peeping, there should be no ret besides the last block
  terminator_type;

  union {
    br_data br;
    brc_data brc;
  };

  constexpr void
  set_brc(u32_t true_block_idx, u32_t false_block_idx) noexcept {
    terminator_type = Terminator::BRC;
    brc = {true_block_idx, false_block_idx};
  }

  constexpr void
  set_br(u32_t next_block_idx) noexcept {
    terminator_type = Terminator::BR;
    br.next_block_idx = next_block_idx;
  }

  constexpr void
  set_ret() noexcept
  {terminator_type = Terminator::RET;}
};

struct Function {
  released_string name;
  const FunctionType* type;
  released_span<const Type*> locals;
  released_ptr<Instruction> instructions;
  released_span<Block> blocks;
};

struct TU {
  struct Global {
    const Type* type;
    released_string name;
  };

  explicit TU(TypeContext&& types) noexcept
  : types(std::move(types)) {}

  TypeContext types;
  std::vector<Global> globals;
  std::vector<Function> functions;
};

TU lowerToPeep(Parser::TU &&);

};