#include "error.hpp"
#include "lexing/lex.hpp"

#include <vector>

namespace {
struct Error {
  std::string message;
  std::string_view file_text;
  u32_t position;
  u16_t length;
  enum class Stage : u16_t { LEXING, PARSING, VALIDATION, BACKEND } stage;
};
}
namespace LOM {
// filepath -> vector of error's
static std::unordered_map<std::string_view, std::vector<Error>> file_to_errors_map;


bool does_file_have_errors(std::string_view file_path) {
  return file_to_errors_map.contains(file_path);
}

std::string get_file_errors(std::string_view file_path) {
  std::string error_messages;
  auto const error_iter = file_to_errors_map.find(file_path);
  if (error_iter == file_to_errors_map.end()) return error_messages;

  auto const& error_vec = error_iter->second;
  error_messages.reserve(error_vec.size() * 32); // assume an arbitrary 32 chars per error message
  error_messages.append("Errors within file:"); error_messages.append(file_path); error_messages.append("\n\n");
  for (auto& error : error_vec) {
    error_messages.append(error.message);
    error_messages.append("\n| ");
    error_messages.append(error.file_text.substr(error.position, error.length));
    error_messages.append(" |\n");
  }

  return error_messages;
}



// TODO: make thread safe
void report_lexing_error(File const& file, u32_t position, u16_t length, std::string error_message) {
  file_to_errors_map[file.path()].emplace_back(
      std::move(error_message),
      file.contents(),
      position,
      length,
      Error::Stage::LEXING
    );
}

void report_parsing_error(File const& file, Lexer::Token token, std::string error_message) {
  file_to_errors_map[file.path()].emplace_back(
      std::move(error_message),
      file.contents(),
      token.position,
      token.length,
      Error::Stage::PARSING
    );
}

void report_validation_error(File const& file, std::string error_message) {
  file_to_errors_map[file.path()].emplace_back(
      std::move(error_message),
      file.contents(),
      0, 0, // temporary pls fix
      Error::Stage::VALIDATION
    );
}

void report_backend_error(File const& file, std::string error_message) {
  file_to_errors_map[file.path()].emplace_back(
      std::move(error_message),
      file.contents(),
      0, 0, // temporary pls fix
      Error::Stage::BACKEND
    );
}

}