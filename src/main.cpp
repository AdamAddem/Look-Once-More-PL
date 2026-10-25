#include "build_system/build.hpp"
#include "settings.hpp"
#include <print>

int main(int argc, char const* argv[]) {

  try {
    LOM::Settings::setArgs(argc, argv);
    static constexpr auto iterations = LOM::Settings::do_profile ? 100'000 : 1;
    for (sz_t i{}; i<iterations; ++i) LOM::build();
  }
  catch (std::exception const& e) {
    std::println(stderr, "LookOnceMore: {}", e.what());
    return 1;
  }

  if constexpr (not LOM::Settings::do_profile) {
    if(LOM::Settings::do_run) {
      system("clear");
      std::string cmd{"./build/"}; cmd += LOM::Settings::getExecutableName().data();
      system(cmd.data());
    }
  }

  return 0;
}
