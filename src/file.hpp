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
  eden::owned_stringview text; // purposefully leaks atm
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
    text.reset(file_buff, buff_length);

  }

  [[nodiscard]] std::string_view
  contents() const noexcept { return std::string_view(text); }

  // somewhat expensive, only use in error reporting
  [[nodiscard]] std::string_view
  path() const noexcept {

    auto const file_path_start = contents().find_last_of('\0', text.size() - 2) + 1;

    assert(file_path_start not_eq std::string::npos);

    std::string_view v =
    {
        text.get() + file_path_start,
        text.size() - file_path_start - 1
        };

    return v;
  }

  [[nodiscard]] std::string_view
  view_at(u32_t position, u16_t length) const noexcept
  { return {text.get() + position, static_cast<u64_t>(length)}; }

};

}