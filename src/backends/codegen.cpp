#include "codegen.hpp"
#include "llvm/tollvm.hpp"
#include "settings.hpp"

#include <cassert>
#include <iostream>

std::unique_ptr<Backend> Backend::codegen(const Validation::ValidatedTU& vtu, const std::string& filename) {
  switch (Settings::chosenBackend()) {
  case Settings::Backend::LLVM:
    return ToLLVM::codegen(vtu, filename);

  default:
    assert(false && "Unknown backend type in codegen.cpp");
  }
}


//i mean.... it works?
void Backend::linkObjects(const std::vector<std::filesystem::path>& obj_paths) {
#ifdef __clang__
  std::string compiler{"clang"};
#elif defined(__GNUC__)
  std::string compiler{"gcc"};
#endif
  for (const auto& file : obj_paths) {
    compiler.push_back(' ');
    compiler.append(file);
  }

  const std::string executable = Settings::getBuildLocation() + Settings::getExecutableName();
  compiler.append(" -o ");
  compiler.append(executable);

  if (system(compiler.c_str())) {
    std::cerr << "Error calling compiler with command: " << compiler;
    std::quick_exit(1);
  }

  std::cout << "Compilation succeded!\n";
}