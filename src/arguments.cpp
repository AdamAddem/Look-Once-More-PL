#include "arguments.hpp"

#include <cassert>
#include <iostream>

static bool output_lexer{false};
static bool output_parser{false};
static bool output_validation{false};
static bool output_llvmir{false};
static bool output_asm{false};
static uint8_t optimization_level{0};

#ifdef _WIN22
static std::string output_name{"lom_program.exe"};
#else
static std::string output_name{"lom_program.out"};
#endif

static std::string build_location{"lom_build/"};

bool Arguments::doOutputLexer() { return output_lexer; }
bool Arguments::doOutputParser() { return output_parser; }
bool Arguments::doOutputValidation() { return output_validation; }
bool Arguments::doOutputIR() { return output_llvmir; }
bool Arguments::doOutputAsm() { return output_asm; }
const std::string& Arguments::getExecutableName() { return output_name; }
const std::string& Arguments::getBuildLocation() { return build_location; }
uint8_t Arguments::getOptimizationLevel() { return optimization_level; }

std::vector<std::filesystem::path> Arguments::setArgs(const unsigned argc, const char* argv[]) {
  using Filepath = std::filesystem::path;
  std::vector<Filepath> filepaths;
  for (auto i{1uz}; i < argc; ++i) {
    using namespace Arguments;
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
      assert(false);
    }

  }

  return filepaths;
}
