#include "build_system/build.hpp"
#include "settings.hpp"

#include <print>
#include <chrono>
int main(int argc, const char* argv[]) {
  if (argc < 2) {
    std::println("LookOnceMore: Arguments required.");
    return 1;
  }

  LOM::Settings::setArgs(argc, argv);

#ifdef PROFILE
  auto begin_time = std::chrono::high_resolution_clock::now();
  for (auto i{0uz}; i<100'000; ++i) {
    LOM::build();
    LOM::reset_state();
  }
  auto end_time = std::chrono::high_resolution_clock::now();
  std::println("Full: {} | {} | {} | {}",
    end_time - begin_time,
    std::chrono::duration_cast<std::chrono::microseconds>(end_time - begin_time),
    std::chrono::duration_cast<std::chrono::milliseconds>(end_time - begin_time),
    std::chrono::duration_cast<std::chrono::seconds>(end_time - begin_time)
  );
#else
  LOM::build();
#endif

  return 0;
}
