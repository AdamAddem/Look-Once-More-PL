#pragma once
#include "edenlib/typedefs.hpp"
#include "semantic_analysis/symbol_table.hpp"
#include "settings.hpp"

#include <unordered_map>

namespace LOM::Parser {
struct TU;
}

namespace LOM::PeepMIR {

struct Instruction {
  enum : u8_t {
    GLOBAL, LOCAL, FUNCTION,
    INT_LITERAL, UINT_LITERAL, FLOAT_LITERAL, DOUBLE_LITERAL, BOOL_LITERAL, CHAR_LITERAL, STRING_LITERAL,

    ADD, SUB, MULT, DIV,
    ASSIGN,
    CMP_LESS, CMP_GTR, CMP_LEQ, CMP_GEQ,
    AND, OR, BITAND, BITOR, BITXOR, BITNOT,
    EQ, NEQ,
    ADDRESS_OF,
  }op;
  u64_t value;

  [[nodiscard]] i64_t getInt() const noexcept                  {return std::bit_cast<i64_t>(value);}
  [[nodiscard]] u64_t getUint() const noexcept                 {return value;}
  [[nodiscard]] float getFloat() const noexcept                {return std::bit_cast<float>(static_cast<u32_t>(value));}
  [[nodiscard]] double getDouble() const noexcept              {return std::bit_cast<double>(value);}
  [[nodiscard]] bool getBool() const noexcept                  {return value;}
  [[nodiscard]] char getChar() const noexcept                  {return static_cast<char>(value);}
  [[nodiscard]] char* getString() const noexcept               {return std::bit_cast<char*>(value); }
};

struct Block {
  std::vector<Instruction> instructions;
  enum class Terminator : u8_t {
    BR, BRC, RET
  }terminator;
};

struct Function {
  const FunctionType* type;
  std::vector<const Type*> locals;
  std::vector<Block> blocks;

  explicit Function(const FunctionType* function_type) : type(function_type) {}
};

struct TU {
  struct Global {
    std::string name;
  };
  SymbolTable table;

  std::unordered_map<std::string, Function> functions;
};

TU lowerToPeep(Parser::TU &&);

};