#include "error.hpp"
#include "lexing/lex.hpp"

#include <print>
#include <vector>

namespace {
struct Error {
  std::string message;
  // char _pad[2];
  u16_t length;
  u32_t position;
};

// file_text -> errors
// Maps file_text pointer assuming that all file text is unique
std::unordered_map< const char*, std::vector<Error> > file_to_errors_map;

}

// TODO: make all of this thread safe
namespace LOM {

eden_noinline_cold
std::string get_file_errors(File file) {
  std::string error_messages;
  auto const file_text = file.get_text();
  auto const file_path = file.path();
  auto const error_iter = file_to_errors_map.find(file_text.data());
  if (error_iter == file_to_errors_map.end()) return error_messages;

  auto const& error_vec = error_iter->second;
  error_messages.append("Errors within file: "); error_messages.append(file_path); error_messages.append("\n\n");
  for (auto& error : error_vec) {
    error_messages.append(error.message);
    auto const beginning_line_idx = file_text.substr(0, error.position).find_last_of('\n');
    auto ending_line_idx = file_text.substr(error.position).find_first_of('\n');
    if (ending_line_idx == std::string_view::npos) ending_line_idx = file_text.substr(error.position).find_first_of('\0');
    auto const ln = std::count(file_text.begin(), file_text.begin() + error.position, '\n') + 1;
    auto const entire_line = file_text.substr(beginning_line_idx + 1, (error.position + ending_line_idx - 1) - beginning_line_idx);

    error_messages.append(
      std::format("\n{}:{}\n", ln, entire_line)
      );

    error_messages.append(
      std::format("{} {}\n",
        std::string(error.position - beginning_line_idx, ' '),
        std::string(error.length, '^'))
      );
  }

  return error_messages;
}

eden_noinline_cold
void report_error(File file, u16_t length, u32_t position, std::string error_message) {
  file_to_errors_map[file.get_text().data()].emplace_back(
      std::move(error_message),
      length, position
    );
}

eden_noinline_cold
void report_error(File file, Lexer::Token token, std::string error_message) {
  file_to_errors_map[file.get_text().data()].emplace_back(
      std::move(error_message),
      token.length, token.position
    );
}

eden_noinline_cold
void report_error(File file, std::string_view file_substr, std::string error_message) {
  auto const [len, pos] = file.len_and_pos_from_view(file_substr);
  file_to_errors_map[file.get_text().data()].emplace_back(
      std::move(error_message),
      len, pos
    );
}

}