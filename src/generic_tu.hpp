#pragma once
#include "edenlib/vectors/vector.hpp"
#include "file.hpp"
#include "module/table_and_module.hpp"

// has to be a macro instead of a struct to exploit the union common subsequence rule,
// allowing us to place Parser::TU and PeepIR::TU within an untagged union and access these members willy nilly
#define GENERIC_TU_DEF \
  Module module; \
  eden::vector<File> source_files; \
  std::string_view name;

namespace LOM {

struct GenericTU {
  GenericTU() = delete;
  ~GenericTU() = delete;
  GENERIC_TU_DEF
};

// By making Parser::TU and PeepIR::TU the same size and alignment, we can transmutate Parser::TU into PeepIR::TU within the same memory
static constexpr auto TU_SIZE = 192uz;
static constexpr auto TU_ALIGN = 8uz;

// By making Parser::Function and PeepIR::Function the same size and alignment, we can reuse Parser::TU::functions as PeepIR::TU::functions w/ the same memory
static constexpr auto FUNCTION_SIZE = 96uz;
static constexpr auto FUNCTION_ALIGN = 8uz;

}