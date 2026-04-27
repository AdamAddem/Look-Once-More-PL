#include "build_system/build.hpp"
#include "edenlib/lifetime_observer.hpp"
#include "settings.hpp"
#include "src/parsing/ast.hpp"

#include <print>

int main(const int argc, const char* argv[]) {
  if(argc < 2)
    std::println("LookOnceMore: Arguments required."), std::quick_exit(0);

  LOM::Settings::setArgs(argc, argv);
  LOM::build();

  return 0;
}
