#pragma once
#include "backends/codegen.hpp"

#include <cassert>
#include <filesystem>

namespace LOM::PeepMIR {
  struct PeepTU;
  struct Function;
}

namespace LOM::ToLLVM {

inline std::unique_ptr<Backend> codegen(const PeepMIR::PeepTU&, const std::filesystem::path &) {assert(false);}

}
