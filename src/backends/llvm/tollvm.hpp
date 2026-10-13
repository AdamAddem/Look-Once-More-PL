#pragma once
#include "backends/codegen.hpp"

#include <filesystem>

namespace LOM::ToLLVM {
std::unique_ptr<Backend> codegen(PeepIR::TU&, const std::filesystem::path &);
static constexpr std::string_view ir_path = "build/llvm_ir/";
}
