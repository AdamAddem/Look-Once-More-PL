#pragma once
#include "backends/codegen.hpp"

#include <cassert>
#include <filesystem>

namespace LOM::PeepMIR {
  struct TU;
}

namespace LOM::ToLLVM {

std::unique_ptr<Backend> codegen(PeepMIR::TU&&, const std::filesystem::path &);

}
