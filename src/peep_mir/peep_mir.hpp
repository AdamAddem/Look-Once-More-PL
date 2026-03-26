#pragma once
#include "settings.hpp"
#include "semantic_analysis/types.hpp"
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
  std::vector<Block> blocks; //if empty, then it is an external function

  explicit Function(const FunctionType* function_type)
  : type(function_type) {}
};

struct PeepTU {
  struct Global {
    std::string name;
    InstantiatedType type;
  };

  std::vector<Global> globals;
  std::unordered_map<std::string, Function> functions;

};

inline PeepTU lowerToPeep(Parser::ParsedTU &&) { assert(false); }

};