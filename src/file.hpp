#pragma once
#include "edenlib/owned.hpp"

#include "edenlib/typedefs.hpp"
#include <cassert>
#include <cstring>
#include <filesystem>
#include <format>
#include <fstream>
#include <string>

namespace LOM {
class File {
  std::string_view text; // purposefully leaks atm
public:
  explicit File(std::filesystem::path const& file_path) {
    std::ifstream stream(file_path); if (not stream.is_open()) throw std::runtime_error(std::format("LookOnceMore: {} file not found.", file_path.native()));

    auto const file_size = static_cast<std::streamsize>(std::filesystem::file_size(file_path));
    auto const file_path_size = file_path.native().size();

    //TODO: Find prettier way to do this
    auto const buff_length = file_size + file_path_size + 2; // +2 for the extra null terminators
    auto const file_buff = new char[buff_length];
    stream.read(file_buff, file_size);
    file_buff[file_size] = '\0';

    std::strncpy(file_buff + file_size + 1, file_path.c_str(), file_path_size + 1);
    text = {file_buff, buff_length};
  }

  eden_always_inline [[nodiscard]] std::string_view get_text() const noexcept { return text; }

  // somewhat expensive, only use in error reporting
  [[nodiscard]] std::string_view
  path() const noexcept {
    auto const file_path_start = text.find_last_of('\0', text.size() - 2) + 1;
    assert(file_path_start not_eq std::string::npos);

    return {
      text.data() + file_path_start,
      text.size() - file_path_start - 1
      };
  }

  [[nodiscard]] std::string_view
  view_at(u16_t length, u32_t position) const noexcept
  { return {text.data() + position, static_cast<u64_t>(length)}; }

  [[nodiscard]] std::pair<u16_t, u32_t>
  len_and_pos_from_view(std::string_view view_from_file) const noexcept {
    return {
      (u16_t) view_from_file.length(),
      view_from_file.data() - text.data()
    };
  }

};

// returns pair<length, position>
[[nodiscard]] constexpr std::pair<u16_t, u32_t>
combine_spans(u32_t leftmost_pos, u16_t rightmost_len, u32_t rightmost_pos) noexcept {
  return {
    u16_t(rightmost_pos - leftmost_pos + rightmost_len),
    leftmost_pos
  };
}

}