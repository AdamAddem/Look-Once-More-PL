#include "build_system/build.hpp"
#include "settings.hpp"

#include <filesystem>
#include <print>
#include <chrono>

int main(int argc, char const* argv[]) {
  if (argc < 2) {
    std::println("LookOnceMore: Arguments required.");
    return 1;
  }

  try {
    LOM::Settings::setArgs(argc, argv);
    static constexpr auto iterations = LOM::Settings::do_profile ? 100'000 : 1;
    for (sz_t i{}; i<iterations; ++i) LOM::build();
  }
  catch (std::exception const& e) {
    std::println(stderr, "LookOnceMore: {}", e.what());
    return 1;
  }

  return 0;
}
