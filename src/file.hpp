#pragma once
#include "edenlib/shortened_macros.hpp"
#include "edenlib/typedefs.hpp"
#include <cassert>
#include <cstring>
#include <filesystem>
#include <format>
#include <fstream>
#include <string>
#include <print>
#include <chrono>

namespace LOM {
class File {
  std::string_view text;
public:
  static constexpr char EOF_CHAR = '\0';

  // used for comptime testing
  consteval explicit File(std::string_view text) : text(text) {}

  explicit File(std::filesystem::path const& file_path) {
    std::ifstream stream(file_path); if (not stream.is_open()) throw std::runtime_error(std::format("Error reading {}. (Sorry!)", file_path.native()));

    auto const file_size = std::filesystem::file_size(file_path);
    auto const file_path_size = file_path.native().size();

    // TODO: Find prettier way to do this
    auto const buff_length = file_size + file_path_size + 2; // +2 for the extra null terminators
    auto const file_buff = new char[buff_length];
    stream.read(file_buff, file_size);
    file_buff[file_size] = EOF_CHAR;

    std::strncpy(file_buff + file_size + 1, file_path.c_str(), file_path_size + 1);
    text = {file_buff, buff_length};
  }

  // probably not a good idea to expose these two
  File() noexcept = default;
  File(File const&) noexcept = default;
  constexpr File& operator=(File const&) noexcept = default;

  InlineNodiscardCXPR std::string_view getText() const noexcept { return text; }

  // somewhat expensive, only use in error reporting
  NodiscardCXPR std::string_view
  path() const noexcept {
    auto const file_path_start = text.find_last_of('\0', text.size() - 2) + 1;
    assert(file_path_start not_eq std::string::npos);

    return {
      text.data() + file_path_start,
      text.size() - file_path_start - 1
      };
  }

  InlineNodiscardCXPR std::string_view
  viewAt(u16_t length, u32_t position) const noexcept
  { return {text.data() + position, (u64_t) length}; }

  InlineNodiscardCXPR std::pair<u16_t, u32_t>
  lenPosFromView(std::string_view view_from_file) const noexcept {
    return {
      (u16_t) view_from_file.length(),
      view_from_file.data() - text.data()
    };
  }

};

// Frees all text associated with this file
// only call after all compilation has finished
inline void free_file(File file) noexcept {
  delete[] file.getText().data();
}

}