#pragma once
#include "file.hpp"

#include <string>
#include <filesystem>

namespace LOM::Lexer {
struct Token;
}

namespace LOM {

void report_error(File const& file, u16_t length, u32_t position, std::string error_message);
void report_error(File const& file, Lexer::Token token, std::string error_message);
void report_error(File const& file, std::string_view file_substr, std::string error_message);

[[nodiscard]] std::string get_file_errors(std::string_view file_path);
[[nodiscard]] bool does_file_have_errors(std::string_view file_path);

}