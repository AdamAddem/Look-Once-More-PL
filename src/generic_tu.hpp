#pragma once
#include "edenlib/vectors/vector.hpp"
#include "file.hpp"
#include "module/table_and_module.hpp"

// is a macro instead of a struct to exploit the union common subsequence rule,
// allowing us to place Parser::TU and PeepIR::TU within an untagged union and access these members willy nilly
// (could just put these in the GenericTU struct and have both TUs have a GenericTI, but then we'd have to access everything thru that object first and thats annoying)
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
static constexpr sz_t TU_SIZE = 192;
static constexpr sz_t TU_ALIGN = 8;

// By making Parser::Function and PeepIR::Function the same size and alignment, we can reuse Parser::TU::functions as PeepIR::TU::functions w/ the same memory
static constexpr sz_t FUNCTION_SIZE = 96;
static constexpr sz_t FUNCTION_ALIGN = 8;

}