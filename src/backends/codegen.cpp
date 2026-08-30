#include "codegen.hpp"
#include "settings.hpp"
#include "llvm/tollvm.hpp"

#include <cassert>
#include <iostream>
#include <utility>

namespace LOM {

std::unique_ptr<Backend> Backend::codegen(PeepIR::TU&& vtu, std::filesystem::path const& file) {
  return ToLLVM::codegen(std::move(vtu), file);
}

//.... it works?
void Backend::linkObjects(eden::vector<std::filesystem::path> const& obj_paths) {
  if constexpr (Settings::external_compiler.empty())
    throw std::runtime_error("Linking objects is currently unsupported without clang or gcc.");

  std::string compiler(Settings::external_compiler);

  for (auto const& file : obj_paths) {
    compiler.push_back(' ');
    compiler.append(file.string());
  }

  compiler += " -o ";
  compiler += std::string_view("build/");
  compiler += Settings::getExecutableName();
  compiler += Settings::getExternFlags();

  if (system(compiler.c_str())) {
    std::cerr << "Error calling compiler with command: " << compiler;
    std::quick_exit(1);
  }
}

}