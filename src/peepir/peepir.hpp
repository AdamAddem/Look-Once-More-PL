#pragma once
#include "../edenlib/vectors/releasing_vector.hpp"
#include "edenlib/typedefs.hpp"
#include "semantic_analysis/symbol_table.hpp"
#include <unordered_map>

namespace LOM::Parser {
struct TU;
}

namespace LOM::PeepMIR {

struct Instruction {
  enum Type : u8_t {
    NOOP,

    GLOBAL,             // value is char* to global name
    FUNCTION,           // value is char* to function name

    MODULE_GLOBAL,      // value is StabilizedModule, aux_value is order of global in module
    MODULE_FUNCTION,    // value is StabilizedModule, aux_value is order of function in module
    TYPE_VARIABLE,      // value is const CustomType*, aux_value is order of variable in type

    LOCAL,              //value is local idx

    // same as AST
    I8_LITERAL, I16_LITERAL, I32_LITERAL, I64_LITERAL,
    U8_LITERAL, U16_LITERAL, U32_LITERAL, U64_LITERAL,
    FLOAT_LITERAL,
    DOUBLE_LITERAL,
    BOOL_LITERAL,
    CHAR_LITERAL,
    STRING_LITERAL,

    // value indeterminate
    ADD, FADD,
    SUB, FSUB,
    MULT, FMULT,
    UDIV, SDIV, FDIV,
    UMOD, SMOD, FMOD,
    ASSIGN,
    UCAST_ASSIGN, SCAST_ASSIGN, // contains the bitwidth to cast right side to
    ULESS, SLESS, FLESS,
    UGTR, SGTR, FGTR,
    ULEQ, SLEQ, FLEQ,
    UGEQ, SGEQ, FGEQ,
    EQ, NEQ, AND, OR,
    BITAND, BITOR, BITXOR,

    // value indeterminate
    PRE_INC, FPRE_INC,
    PRE_DEC, FPRE_DEC,
    ADDRESS_OF,
    NEGATE, FNEGATE,
    BITNOT,
    POST_INC, FPOST_INC,
    POST_DEC, FPOST_DEC,
    DEREFERENCE, // contains pointed type

    // value contains destination type
    UCAST, SCAST, FCAST, PCAST,

    CALL // value equals number of parameters
  }type;
  u32_t aux_value;
  u64_t value;

  constexpr Instruction(Type type, u64_t value) noexcept
  : type(type), value(value)
  { assert( not eden::enumBetween(type, MODULE_GLOBAL, TYPE_VARIABLE) ); }

  constexpr Instruction(Type type, const Module* module, u32_t order) noexcept
  : type(type), aux_value(order), value(std::bit_cast<u64_t>(module))
  { assert( eden::enumBetween(type, MODULE_GLOBAL, MODULE_FUNCTION) ); }

  constexpr Instruction(Type type, const CustomType* table, u32_t order) noexcept
  : type(type), aux_value(order), value(std::bit_cast<u64_t>(table))
  { assume_assert( type == TYPE_VARIABLE ); }

  [[nodiscard]] constexpr bool
  is_literal() const noexcept
  { return eden::enumBetween(type, I8_LITERAL, U64_LITERAL); }

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
  { assert(eden::enumBetween(type, I8_LITERAL, I64_LITERAL)); return std::bit_cast<i64_t>(value); }

  [[nodiscard]] constexpr u64_t
  uint_value() const noexcept
  { assert(eden::enumBetween(type, U8_LITERAL, U64_LITERAL)); return value; }

  [[nodiscard]] constexpr float
  float_value() const noexcept
  { assume_assert(type == FLOAT_LITERAL); return std::bit_cast<float>(static_cast<u32_t>(value)); }

  [[nodiscard]] constexpr double
  double_value() const noexcept
  { assume_assert(type == DOUBLE_LITERAL); return std::bit_cast<double>(value); }

  [[nodiscard]] constexpr bool
  bool_value() const noexcept
  { assume_assert(type == BOOL_LITERAL); return value; }

  [[nodiscard]] constexpr char
  char_value() const noexcept
  { assume_assert(type == CHAR_LITERAL); return static_cast<char>(value); }

  eden_return_nonnull
  [[nodiscard]] constexpr char*
  string_value() const noexcept
  { assume_assert(type == STRING_LITERAL); return std::bit_cast<char*>(value); }

  eden_return_nonnull
  [[nodiscard]] constexpr char*
  global_name() const noexcept
  { assume_assert(type == GLOBAL); return std::bit_cast<char*>(value); }

  [[nodiscard]] constexpr u64_t
  local_idx() const noexcept
  { assume_assert(type == LOCAL); return value; }

  eden_return_nonnull
  [[nodiscard]] constexpr char*
  function_name() const noexcept
  { assume_assert(type == FUNCTION); return std::bit_cast<char*>(value); }

  [[nodiscard]] constexpr const CustomType*
  custom_type() const noexcept
  { assert(type == TYPE_VARIABLE);  return std::bit_cast<const CustomType*>(value); }

  [[nodiscard]] constexpr u16_t
  type_variable_id() const noexcept
  { assume_assert(type == TYPE_VARIABLE); return static_cast<u16_t>(aux_value); }

  [[nodiscard]] constexpr const SymbolTable::Variable&
  type_variable() const noexcept
  { assume_assert(type == TYPE_VARIABLE); return *custom_type()->member_table()->getVariable(type_variable_id()); }

  //[[nodiscard]] constexpr u16_t
  //type_function_id() const noexcept
  //{ assume_assert(type == TYPE_FUNCTION); return static_cast<u16_t>(aux_value); }

  //[[nodiscard]] constexpr const SymbolTable::Function&
  //type_function() const noexcept
  //{ assume_assert(type == TYPE_FUNCTION); return *member_owned_type()->member_table()->getFunction(type_function_id()); }

  [[nodiscard]] constexpr StabilizedModule
  module() const noexcept
  { assert(eden::enumBetween(type, MODULE_GLOBAL, MODULE_FUNCTION));  return std::bit_cast<const Module*>(value); }

  [[nodiscard]] constexpr u16_t
  module_global_id() const noexcept
  { assume_assert(type == MODULE_GLOBAL); return static_cast<u16_t>(aux_value); }

  [[nodiscard]] constexpr const SymbolTable::Variable&
  module_global() const noexcept
  { assume_assert(type == MODULE_GLOBAL); return *module().getVariable(module_global_id()); }

  [[nodiscard]] constexpr u16_t
  module_function_id() const noexcept
  { assume_assert(type == MODULE_FUNCTION); return static_cast<u16_t>(aux_value); }

  [[nodiscard]] constexpr const SymbolTable::Function&
  module_function() const noexcept
  { assume_assert(type == MODULE_FUNCTION); return *module().getFunction(module_function_id()); }

  [[nodiscard]] constexpr u64_t
  num_params() const noexcept
  { assume_assert(type == CALL); return value; }

  eden_return_nonnull
  [[nodiscard]] constexpr const LOM::Type*
  dereference_type() const noexcept
  { assume_assert(type == DEREFERENCE); return std::bit_cast<const LOM::Type*>(value); }

  [[nodiscard]] constexpr u64_t
  cast_assign_bitwidth() const noexcept
  { assume_assert(type == UCAST_ASSIGN or type == SCAST_ASSIGN); return value; }

  [[nodiscard]] constexpr const LOM::Type*
  cast_type() const noexcept
  { assert(eden::enumBetween(type, UCAST, PCAST)); return std::bit_cast<const LOM::Type*>(value); }

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