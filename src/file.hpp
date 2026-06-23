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
  eden::owned_stringview text;
public:
  explicit File(std::filesystem::path const& file_path) {
    std::ifstream stream(file_path); if (not stream.is_open()) throw std::runtime_error(std::format("LookOnceMore: {} file not found.", file_path.native()));

    auto const file_size = static_cast<std::streamsize>(std::filesystem::file_size(file_path));

    //TODO: Find prettier way to do this
    auto const buff_length = file_size + file_path.native().size() + 2; // + 2 for the null terminator after file contents and the one after filepath
    auto const file_buff = new char[buff_length];
    stream.read(file_buff, file_size);
    file_buff[file_size] = '\0';

    std::strncpy(file_buff + file_size + 1, file_path.c_str(), file_path.native().size());
    text.reset(file_buff, buff_length);

  }

  [[nodiscard]] std::string_view
  contents() const noexcept { return std::string_view(text); }

  // somewhat expensive, only use in error reporting
  [[nodiscard]] std::string_view
  path() const noexcept {
    auto const file_path_start = contents().find_last_of('\0', text.size() - 1) + 1;
    assert(file_path_start not_eq std::string::npos);

    return {
      text.get() + file_path_start,
        text.size() - file_path_start - 1
        };
  }

  [[nodiscard]] std::string_view
  view_at(u32_t position, u16_t length) const noexcept
  { return {text.get() + position, static_cast<u64_t>(length)}; }

};

}