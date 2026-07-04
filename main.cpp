#include "build_system/build.hpp"
#include "settings.hpp"

#include <print>
int main(int argc, const char* argv[]) {
  if (argc < 2) {
    std::println("LookOnceMore: Arguments required.");
    return 1;
  }

  LOM::Settings::setArgs(argc, argv);

#ifdef PROFILE
  for (auto i{0uz}; i<100000; ++i) {
    LOM::build();
    LOM::reset_state();
  }
#else
  LOM::build();
#endif

  return 0;
}
