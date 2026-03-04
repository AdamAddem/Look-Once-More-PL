#pragma once
#include <cstdint>
#include <filesystem>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace Arguments {
enum class Args : unsigned {
  OUTPUT_LEXER,
  OUTPUT_PARSER,
  OUTPUT_VALIDATION,
  OUTPUT_LLVMIR,
  OUTPUT_ASM,
  EXECUTABLE_NAME,
  BUILD_LOCATION,
  O0,
  O1,
  O2,
  O3,
};


bool doOutputLexer();
bool doOutputParser();
bool doOutputValidation();
bool doOutputIR();
bool doOutputAsm();
const std::string& getExecutableName();
const std::string& getBuildLocation();
uint8_t getOptimizationLevel();


inline const std::unordered_map<std::string, Args> stringToArgs{
	  {"-emit-lexer", Args::OUTPUT_LEXER}, {"-emit-parser", Args::OUTPUT_PARSER}, {"-validate", Args::OUTPUT_VALIDATION},
          {"-emit-llvm", Args::OUTPUT_LLVMIR}, {"-emit-asm", Args::OUTPUT_ASM}, {"-o", Args::EXECUTABLE_NAME},
          {"-build-location", Args::BUILD_LOCATION},
          {"-O0", Args::O0}, {"-O1", Args::O1}, {"-O2", Args::O2},
          {"-O3", Args::O3},
};


constexpr const char* argsToString(const Args e) {
  constexpr const char* toString[] = {
    "-emit-lexer","-emit-parser","-validate",
    "-emit-llvm","-emit-asm","-o",
    "-build-location",
    "-O0","-O1","-O2",
    "-O3",
  };

  return toString[std::to_underlying(e)];
}

std::vector<std::filesystem::path> setArgs(unsigned argc, const char* argv[]);

}