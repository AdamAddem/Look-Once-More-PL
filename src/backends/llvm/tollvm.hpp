#pragma once
#include <string>

namespace Validation {
  struct ValidatedTU;
}

namespace ToLLVM {

void compile(Validation::ValidatedTU &&, const std::string &filename);

}
