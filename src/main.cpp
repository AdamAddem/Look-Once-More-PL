#include "build_system/build.hpp"
#include "settings.hpp"

#include <chrono>
#include <filesystem>
#include <print>
int main(int argc, char const* argv[]) {
  if (argc < 2) {
    std::println("LookOnceMore: Arguments required.");
    return 1;
  }

  LOM::Settings::setArgs(argc, argv);

  if (LOM::Settings::do_output_obj)
    std::filesystem::create_directories("build/obj");

#ifdef PROFILE
  namespace chno = std::chrono;
  auto const begin_time = chno::high_resolution_clock::now();
  for (sz_t i{}; i<100'000; ++i) LOM::build();
  auto const end_time = chno::high_resolution_clock::now();
  std::println("Full: {} | {} ",
    end_time - begin_time,
    chno::duration_cast<chno::microseconds>(end_time - begin_time),
    chno::duration_cast<chno::milliseconds>(end_time - begin_time)
  );
#else
  try {
    LOM::build();
  }
  catch (std::exception const& e) {
    std::println(stderr, "{}", e.what());
  }
#endif

  return 0;
}
