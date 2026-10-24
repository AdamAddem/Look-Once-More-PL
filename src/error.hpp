#pragma once
#include "file.hpp"

#include <string>

namespace LOM::Lexer {
struct Token;
}

namespace LOM {

edenNoInlineCold [[nodiscard]] std::string getFileErrors(File file);
edenNoInlineCold [[nodiscard]] bool doesFileHaveErrors(File file);
edenNoInlineCold void reportError(File file, u16_t length, u32_t position, std::string error_message);
edenNoInlineCold void reportError(File file, Lexer::Token token, std::string error_message);
edenNoInlineCold void reportError(File file, std::string_view file_substr, std::string error_message);

}