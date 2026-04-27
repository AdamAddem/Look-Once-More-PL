#pragma once
#include <cstdint>
#include <string>

namespace LOM::Settings {

[[nodiscard]] bool doOutputLexer() noexcept;
[[nodiscard]] bool doOutputParser() noexcept;
[[nodiscard]] bool doOutputPeep()noexcept;
[[nodiscard]] bool doOutputValidation()noexcept;
[[nodiscard]] bool doOutputIR()noexcept;
[[nodiscard]] bool doOutputASM()noexcept;
[[nodiscard]] bool doOutputOBJ()noexcept;
[[nodiscard]] bool doLinking()noexcept;
[[nodiscard]] bool doBuild() noexcept;

const std::string& getExecutableName();
uint8_t getOptimizationLevel();

void setArgs(unsigned argc, const char* argv[]);

static constexpr unsigned short MAX_FUNCTION_PARAMETERS = 10;
}