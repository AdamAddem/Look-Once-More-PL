#include "build_system/build.hpp"
#include "settings.hpp"


int main(int argc, const char* argv[]) {
  if (argc < 2) {
    std::printf("LookOnceMore: Arguments required.");
    return 1;
  }

  LOM::Settings::setArgs(argc, argv);
  LOM::build();

  return 0;
}
