#pragma once
#include <cstdint>
#include <filesystem>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace Settings {

enum class Backend {
  LLVM,
};

bool doOutputLexer();
bool doOutputParser();
bool doOutputValidation();
bool doOutputIR();
bool doOutputASM();
bool doOutputOBJ();
bool doLinking();
Backend chosenBackend();

const std::string& getExecutableName();
const std::string& getBuildLocation();
uint8_t getOptimizationLevel();

std::vector<std::filesystem::path> setArgs(unsigned argc, const char* argv[]);

}