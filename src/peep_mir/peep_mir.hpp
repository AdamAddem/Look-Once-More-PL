#pragma once
#include "settings.hpp"
#include "semantic_analysis/symbol_table.hpp"
#include "utilities/typedefs.hpp"

#include <unordered_map>

namespace LOM::Parser {
struct ParsedTU;
}

namespace LOM::PeepMIR {

struct Instruction {
  enum : u8_t {
    GLOBAL, LOCAL, FUNCTION, LITERAL, //data state

    ADD, SUB, MULT, DIV,
    ASSIGN,
    CMP_LESS, CMP_GTR, CMP_LEQ, CMP_GEQ,
    AND, OR, BITAND, BITOR, BITXOR, BITNOT,
    EQ, NEQ,
    ADDRESS_OF,
  }op;
  u64_t value;

  [[nodiscard]] i64_t getInt() const                      {return std::bit_cast<i64_t>(value);}
  [[nodiscard]] u64_t getUint() const                     {return value;}
  [[nodiscard]] float getFloat() const                    {return std::bit_cast<float>(static_cast<u32_t>(value));}
  [[nodiscard]] double getDouble() const                  {return std::bit_cast<double>(value);}
  [[nodiscard]] bool getBool() const                      {return value;}
  [[nodiscard]] char getChar() const                      {return static_cast<char>(value);}
  [[nodiscard]] std::string* getString() const            {return std::bit_cast<std::string*>(value); }
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

struct PeepTU {
  struct Global {
    std::string name;
  };
  SymbolTable table;

  std::unordered_map<std::string, Function> functions;
};

PeepTU lowerToPeep(Parser::ParsedTU &&);

};