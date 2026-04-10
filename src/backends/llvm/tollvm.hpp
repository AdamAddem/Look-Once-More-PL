#pragma once
#include "backends/codegen.hpp"

#include <cassert>
#include <filesystem>

namespace LOM::PeepMIR {
  struct TU;
  struct Function;
}

namespace LOM::ToLLVM {

inline std::unique_ptr<Backend> codegen(const PeepMIR::TU&, const std::filesystem::path &) {
  assert(false);
  return nullptr;
}

}
