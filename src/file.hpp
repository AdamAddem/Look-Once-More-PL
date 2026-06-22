#pragma once
#include <cassert>
#include <string>
#include <filesystem>
#include <fstream>
#include <format>
#include "edenlib/typedefs.hpp"

namespace LOM {
class File {
  std::string text;
public:
  explicit File(std::filesystem::path const& file_path) {
    std::ifstream stream(file_path); if (stream.is_open()) throw std::runtime_error(std::format("LookOnceMore: {} File not found.", file_path.native()));

    auto const file_size = static_cast<std::streamsize>(std::filesystem::file_size(file_path));
    text.reserve(file_size + file_path.native().size() + 2); // + 2 for the null terminator after file contents and the one after filepath
    stream.read(text.data(), file_size);
    text.push_back('\0');
    text.append(file_path.native());
  }

  File(File const&) = delete;
  File(File &&) noexcept = default;

  [[nodiscard]] std::string const&
  contents() const noexcept { return text; }

  // somewhat expensive, only use in error reporting
  [[nodiscard]] std::string_view
  path() const noexcept {
    auto const file_path_start = text.find_last_of('\0', text.size() - 1) + 1;
    assert(file_path_start not_eq std::string::npos);

    return {
      text.data() + file_path_start,
        text.size() - file_path_start - 1
        };
  }

  [[nodiscard]] std::string_view
  view_at(u32_t position, u16_t length) const noexcept
  { return {text.data() + position, static_cast<u64_t>(length)}; }

};

}