#pragma once
#include "backends/codegen.hpp"
#include <filesystem>
#include <string>

namespace Validation {
  struct ValidatedTU;
  struct ValidatedFunction;
}

namespace ToLLVM {

std::unique_ptr<Backend> codegen(const Validation::ValidatedTU&, const std::filesystem::path &file);

}
