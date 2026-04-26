#include "codegen.hpp"
#include "settings.hpp"
#include "llvm/tollvm.hpp"

#include <cassert>
#include <iostream>
#include <utility>

using namespace LOM;

std::unique_ptr<Backend> Backend::codegen(PeepMIR::TU&& vtu, const std::filesystem::path& file) {
  return ToLLVM::codegen(std::move(vtu), file);
}


//i mean.... it works?
void Backend::linkObjects(const std::vector<std::filesystem::path>& obj_paths) {
#ifdef __clang__
  std::string compiler{"clang"};
#elif defined(__GNUC__)
  std::string compiler{"gcc"};
#else
  throw std::runtime_error("Linking objects is currently not supported without clang or gcc. You're probably on windows, in which case, godspeed.");
#endif

  for (const auto& file : obj_paths) {
    compiler.push_back(' ');
    compiler.append(file.string());
  }

  const std::string executable = "build/" + Settings::getExecutableName();
  compiler.append(" -o ");
  compiler.append(executable);

  if (system(compiler.c_str())) {
    std::cerr << "Error calling compiler with command: " << compiler;
    std::quick_exit(1);
  }
}