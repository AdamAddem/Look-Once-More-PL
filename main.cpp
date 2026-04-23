#include "ast/ast.hpp"
#include "build_system/build.hpp"
#include "edenlib/lifetime_observer.hpp"
#include "settings.hpp"

#include <print>

int main(const int argc, const char* argv[]) {
  if(argc < 2)
    std::println("LookOnceMore: Arguments required."), std::quick_exit(0);

  LOM::Settings::setArgs(argc, argv);
  LOM::build();

  /*
  const bool output_asm = Settings::doOutputASM();
  const bool output_ir = Settings::doOutputIR();
  const bool output_obj = Settings::doOutputOBJ() or Settings::doLinking();
  for (auto& filename : filepaths) {
    const auto compiled = processLOMFile(filename);
    if (output_asm)
      compiled->createASMFile(filename);
    if (output_ir)
      compiled->createIRFile(filename);
    if (output_obj)
      filename = compiled->createObjectFile(filename);
  }

  if (Settings::doLinking())
    Backend::linkObjects(filepaths); */

  return 0;
}
