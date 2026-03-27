#include "settings.hpp"

#include <cassert>
#include <iostream>
#include <unordered_map>
#include <utility>

enum class Args : unsigned {
  OUTPUT_LEXER,
  OUTPUT_PARSER,
  OUTPUT_VALIDATION,
  OUTPUT_LLVMIR,
  OUTPUT_ASM,
  OUTPUT_OBJ,
  EXECUTABLE_NAME,
  BUILD_LOCATION,
  O0,
  O1,
  O2,
  O3,
};

inline const std::unordered_map<std::string, Args> stringToArgs{
	    {"-emit-lexer", Args::OUTPUT_LEXER}, {"-emit-parser", Args::OUTPUT_PARSER}, {"-validate", Args::OUTPUT_VALIDATION},
            {"-emit-llvm", Args::OUTPUT_LLVMIR}, {"-emit-asm", Args::OUTPUT_ASM}, {"-emit-obj", Args::OUTPUT_OBJ},
            {"-o", Args::EXECUTABLE_NAME},{"-build-location", Args::BUILD_LOCATION},
            {"-O0", Args::O0}, {"-O1", Args::O1}, {"-O2", Args::O2},
            {"-O3", Args::O3},
};

using namespace LOM;


static bool output_lexer{false};
static bool output_parser{false};
static bool output_validation{false};
static bool output_llvmir{false};
static bool output_asm{false};
static bool output_obj{false};
static auto chosen_backend{Settings::Backend::LLVM};
static uint8_t optimization_level{0};

static std::string output_name;

static std::string build_location{"lom_build/"};


bool Settings::doOutputLexer()                   { return output_lexer; }
bool Settings::doOutputParser()                  { return output_parser; }
bool Settings::doOutputValidation()              { return output_validation; }
bool Settings::doOutputIR()                      { return output_llvmir; }
bool Settings::doOutputASM()                     { return output_asm; }
bool Settings::doOutputOBJ()                     { return output_obj; }
bool Settings::doLinking()                       { return !output_obj && !output_asm && !output_llvmir; }
auto Settings::chosenBackend() -> Backend        { return chosen_backend; }

const std::string& Settings::getExecutableName() { return output_name; }
const std::string& Settings::getBuildLocation()  { return build_location; }
uint8_t Settings::getOptimizationLevel()         { return optimization_level; }

std::vector<std::filesystem::path> Settings::setArgs(const unsigned argc, const char* argv[]) {
  using Filepath = std::filesystem::path;
  std::vector<Filepath> filepaths;
  for (auto i{1uz}; i < argc; ++i) {
    using namespace Settings;
    if (!stringToArgs.contains(argv[i])) {
      Filepath filepath = argv[i];
      if (filepath.extension() != ".lom")  {
        if (filepath.has_extension())
          std::cout << "File extension must be .lom: ";
        else
          std::cout << "Unrecognized argument: ";

        std::cout << filepath << std::endl;
        std::quick_exit(1);
      }

      filepaths.emplace_back(std::move(filepath));
      continue;
    }

    const auto arg = stringToArgs.at(argv[i]);
    switch (arg) {
    case Args::OUTPUT_LEXER:
      output_lexer = true; break;
    case Args::OUTPUT_PARSER:
      output_parser = true; break;
    case Args::OUTPUT_VALIDATION:
      output_validation = true; break;
    case Args::OUTPUT_LLVMIR:
      output_llvmir = true; break;
    case Args::OUTPUT_ASM:
      output_asm = true; break;
    case Args::OUTPUT_OBJ:
      output_obj = true; break;
    case Args::EXECUTABLE_NAME:
      if (++i == argc) {
        std::cout << "Expected executable name after -o" << std::endl;
        std::quick_exit(1);
      }
      if (!output_name.empty()) {
        std::cout << "Multiple output names specified, don't do that" << std::endl;
        std::quick_exit(1);
      }

      output_name = argv[i];
      ++i; break;
    case Args::BUILD_LOCATION:
      if (++i == argc) {
        std::cout << "Expected build location name after -build-location" << std::endl;
        std::quick_exit(1);
      }
      build_location = argv[i];
      ++i; break;
    case Args::O0:
      optimization_level = 0; break;
    case Args::O1:
      optimization_level = 1; break;
    case Args::O2:
      optimization_level = 2; break;
    case Args::O3:
      optimization_level = 3; break;

    default:
      std::unreachable();
    }

  }
  if (output_name.empty())
#ifdef _WIN32
      output_name = "lom.exe";
#else
      output_name = "lom.out";
#endif

  return filepaths;
}
