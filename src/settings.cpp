#include "settings.hpp"
#include "edenlib/typedefs.hpp"
#include "edenlib/shortened_macros.hpp"

#include <cassert>
#include <filesystem>
#include <fstream>
#include <unordered_map>
#include <utility>
#include <print>
#include <string_view>

namespace {

enum class Args {
  OUTPUT_PARSER,
  OUTPUT_PEEP,
  OUTPUT_VALIDATION,
  OUTPUT_LLVMIR,
  OUTPUT_ASM,
  OUTPUT_OBJ,
  EXECUTABLE_NAME,
  BUILD,
  O0,
  O1,
  O2,
  O3,
};

std::unordered_map<std::string_view, Args> const stringToArgs{
	      {"--emit-parser", Args::OUTPUT_PARSER}, {"--emit-peep", Args::OUTPUT_PEEP},
              {"--emit-llvm", Args::OUTPUT_LLVMIR}, {"--emit-asm", Args::OUTPUT_ASM}, {"--emit-obj", Args::OUTPUT_OBJ},
              {"-o", Args::EXECUTABLE_NAME},{"build", Args::BUILD},
              {"-O0", Args::O0}, {"-O1", Args::O1}, {"-O2", Args::O2}, {"-O3", Args::O3},
};

constexpr std::string_view hello_world = {
R"(
__C puts(str: ref u8) i32;

main: () i32 {
  __C::puts("Hello, World!");
  return 0;
}
)"

};

}

namespace LOM::Settings {

static bool output_parser_flag{false};      bool const& do_output_parser = output_parser_flag;
static bool output_peep_flag{false};        bool const& do_output_peep = output_peep_flag;
static bool output_validation_flag{false};  bool const& do_output_validation = output_validation_flag;
static bool output_llvmir_flag{false};      bool const& do_output_llvmir = output_llvmir_flag;
static bool output_asm_flag{false};         bool const& do_output_asm = output_asm_flag;
static bool output_obj_flag{false};         bool const& do_output_obj = output_obj_flag;
static bool do_linking_flag{true};          bool const& do_linking = do_linking_flag;
static bool do_build_flag{false};           bool const& do_build = do_build_flag;
static u8_t optimization_level{0};
static std::string_view output_name;
static std::string extern_flags{" "};

std::string_view getExecutableName() noexcept   { return output_name; }
std::string_view getExternFlags() noexcept      { return extern_flags; }
u8_t getOptimizationLevel() noexcept            { return optimization_level; }

namespace fs = std::filesystem;
void setArgs(int _argc, const char** argv) {
  auto const argc = (sz_t) _argc;
  if (std::string_view(argv[1]) == "init") {
    fs::create_directory("build");
    fs::create_directory("extern");
    fs::create_directory("src");
    std::ofstream main_lom_file("src/main.lom");
    main_lom_file << hello_world;
    main_lom_file.close();
    std::quick_exit(0);
  }

  for (sz_t i{1}; i < argc; ++i) {
    auto const arg_view = std::string_view(argv[i]);
    auto const arg_iter = stringToArgs.find(arg_view);
    if (arg_iter == stringToArgs.end()) {
      if (arg_view != "--link-flags") throw std::runtime_error(std::format("Unrecognized argument {}.", arg_view));

      while (++i < argc) { extern_flags += argv[i]; extern_flags.push_back(' '); }
      break;
    }

    auto const arg = arg_iter->second;
    switch (arg) {
    case Args::OUTPUT_PARSER:     output_parser_flag = true;      break;
    case Args::OUTPUT_PEEP:       output_peep_flag = true;        break;
    case Args::OUTPUT_VALIDATION: output_validation_flag = true;  break;
    case Args::OUTPUT_LLVMIR:     output_llvmir_flag = true;      break;
    case Args::OUTPUT_ASM:        output_asm_flag = true;         break;
    case Args::OUTPUT_OBJ:        output_obj_flag = true;         break;
    case Args::EXECUTABLE_NAME:
      if (++i == argc)              throw std::runtime_error("Expected executable name after -o.");
      if (not output_name.empty())  throw std::runtime_error("Multiple output names specified, maybe don't do that :).");
      output_name = argv[i];
      break;

    case Args::BUILD: do_build_flag = true;   break;
    case Args::O0:    optimization_level = 0; break;
    case Args::O1:    optimization_level = 1; break;
    case Args::O2:    optimization_level = 2; break;
    case Args::O3:    optimization_level = 3; break;

    default:
      Unreachable("Invalid argument.");
    }

  }
  if (output_name.empty())
#ifdef _WIN32
    output_name = "lom.exe";
#else
    output_name = "lom.out";
#endif

  do_linking_flag = not output_obj_flag bitand not output_asm_flag bitand not output_llvmir_flag bitand not output_parser_flag bitand not output_peep_flag;
  output_obj_flag = output_obj_flag bitor do_linking_flag;
}

}