#pragma once
#include "file.hpp"

#include <string>
#include <filesystem>

namespace LOM::Lexer {
struct Token;
}

namespace LOM {

eden_noinline_cold [[nodiscard]] std::string get_file_errors(File file);
eden_noinline_cold void report_error(File file, u16_t length, u32_t position, std::string error_message);
eden_noinline_cold void report_error(File file, Lexer::Token token, std::string error_message);
eden_noinline_cold void report_error(File file, std::string_view file_substr, std::string error_message);

}