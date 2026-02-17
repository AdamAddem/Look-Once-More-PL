#pragma once
#include <string>
#include <vector>

using Types = std::vector<std::string>;

void printType(const Types &type) noexcept;
[[nodiscard]] bool are_types_convertible(const std::string& from, const std::string& to) noexcept;
[[nodiscard]] bool are_types_convertible(const std::string& from, const Types& to) noexcept;
[[nodiscard]] bool are_types_convertible(const Types& from, const Types& to) noexcept;
