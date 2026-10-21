#include "codegen.hpp"
#include "settings.hpp"
#include "llvm/tollvm.hpp"

#include <cassert>
#include <iostream>
#include <utility>

namespace LOM {

std::unique_ptr<Backend> Backend::codegen(PeepIR::TU& ptu) {
  return ToLLVM::codegen(ptu);
}

//.... it works?
void Backend::linkObjects() {
  if constexpr (Settings::external_compiler.empty())
    throw std::runtime_error("Linking objects is currently unsupported without clang or gcc.");

  std::string command(Settings::external_compiler);
  command += " build/obj/*";
  command += " -o ";
  command += std::string_view("build/");
  command += Settings::getExecutableName();
  command += Settings::getExternFlags();

  if (system(command.c_str())) {
    std::cerr << "Error calling compiler with command: " << command;
    std::quick_exit(1);
  }
}

}