#pragma once
#include <cstdint>
#include <filesystem>
#include <string>
#include <vector>

namespace LOM::Settings {

enum class Backend {
  LLVM,
};

[[nodiscard]] bool doOutputLexer() noexcept;
[[nodiscard]] bool doOutputParser() noexcept;
[[nodiscard]] bool doOutputPeep()noexcept;
[[nodiscard]] bool doOutputValidation()noexcept;
[[nodiscard]] bool doOutputIR()noexcept;
[[nodiscard]] bool doOutputASM()noexcept;
[[nodiscard]] bool doOutputOBJ()noexcept;
[[nodiscard]] bool doLinking()noexcept;
[[nodiscard]] Backend chosenBackend() noexcept ;

const std::string& getExecutableName();
const std::string& getBuildLocation();
uint8_t getOptimizationLevel();

std::vector<std::filesystem::path> setArgs(unsigned argc, const char* argv[]);

static constexpr unsigned short MAX_FUNCTION_PARAMETERS = 4;
}