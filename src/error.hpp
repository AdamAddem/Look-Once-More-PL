#pragma once
#include "file.hpp"

#include <string>
#include <filesystem>

namespace LOM::Lexer {
struct Token;
}

namespace LOM {

void report_lexing_error(    File const& file, u32_t position, u16_t length, std::string error_message);
void report_parsing_error(   File const& file, Lexer::Token token, std::string error_message);
void report_validation_error(File const& file, std::string error_message);
void report_backend_error(   File const& file, std::string error_message);

std::string get_file_errors(std::string_view file_path);

}