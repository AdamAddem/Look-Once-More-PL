#pragma once
#include "backends/codegen.hpp"

#include <cassert>
#include <filesystem>

namespace LOM::PeepIR {
  struct TU;
}

namespace LOM::ToLLVM {

std::unique_ptr<Backend> codegen(PeepIR::TU&&, const std::filesystem::path &);

}
