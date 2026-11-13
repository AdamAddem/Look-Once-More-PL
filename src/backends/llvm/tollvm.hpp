#pragma once
#include "backends/codegen.hpp"

namespace LOM::ToLLVM {
std::unique_ptr<Backend> codegen(PeepIR::TU&);
static constexpr std::string_view ir_path = "build/llvm_ir/";
}
