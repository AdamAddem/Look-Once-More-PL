#include "types.hpp"
#include <algorithm>
#include <cassert>
#include <iostream>

bool are_types_convertible(const std::string &from,
                           const std::string &to) noexcept {
  return from == to; // temporary, change when convertible types become a thing
}

bool are_types_convertible(const std::string &from, const Types &to) noexcept {
  for (auto &t : to) {
    if (are_types_convertible(from, t))
      return true;
  }

  return false;
}

bool are_types_convertible(const Types &from, const Types &to) noexcept {
  return from == to;
}

void printType(const Types &type) noexcept {
  assert(!type.empty());
  if (type.size() == 1) {
    std::cout << type.front();
    return;
  }

  std::cout << " <";
  for (auto &t : type) {
    if (t.empty()) [[unlikely]]
      std::cout << "devoid";
    else
      std::cout << t;
    std::cout << ", ";
  }

  std::cout << "\b\b >";
}