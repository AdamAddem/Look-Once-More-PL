#include "error.hpp"
#include "lexing/lex.hpp"

#include <vector>

namespace {
struct Error {
  std::string message;
  std::string_view file_text;
  u16_t length;
  u32_t position;
};

// filepath -> errors
std::unordered_map<std::string_view, std::vector<Error>> file_to_errors_map;

}

// TODO: make all of this thread safe
namespace LOM {

bool file_has_errors(std::string_view file_path) {
  return file_to_errors_map.contains(file_path);
}

std::string get_file_errors(std::string_view file_path) {
  std::string error_messages;
  auto const error_iter = file_to_errors_map.find(file_path);
  if (error_iter == file_to_errors_map.end()) return error_messages;

  auto const& error_vec = error_iter->second;
  error_messages.reserve(error_vec.size() * 32); // assume an arbitrary 32 chars per error message
  error_messages.append("Errors within file: "); error_messages.append(file_path); error_messages.append("\n\n");
  for (auto& error : error_vec) {
    error_messages.append(error.message);
    auto const beginning_line_idx = error.file_text.substr(0, error.position).find_last_of('\n');
    auto const ending_line_idx = error.file_text.substr(error.position).find_first_of('\n');
    auto const ln = std::count(error.file_text.begin(), error.file_text.begin() + error.position, '\n') + 1;
    auto const entire_line = error.file_text.substr(beginning_line_idx + 1, (error.position + ending_line_idx) - beginning_line_idx);

    error_messages.append(
      std::format("\n{}:{}", ln, entire_line)
      );


    error_messages.append(
      std::format("{}{}\n",
        std::string(error.position - beginning_line_idx + 1, ' '),
        std::string(error.length, '^'))
      );
  }

  return error_messages;
}


void report_error(File const& file, u16_t length, u32_t position, std::string error_message) {
  file_to_errors_map[file.path()].emplace_back(
      std::move(error_message),
      file.contents(),
      length, position
    );
}

void report_error(File const& file, Lexer::Token token, std::string error_message) {
  file_to_errors_map[file.path()].emplace_back(
      std::move(error_message),
      file.contents(),
      token.length, token.position
    );
}

void report_error(File const& file, std::string_view file_substr, std::string error_message) {
  auto const [len, pos] = file.len_and_pos_from_view(file_substr);
  file_to_errors_map[file.path()].emplace_back(
      std::move(error_message),
      file.contents(),
      len, pos
    );
}

}