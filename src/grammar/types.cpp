#include "types.hpp"
#include <iostream>
#include <stdexcept>
#include <unordered_set>

VariantType::VariantType(std::vector<StrictType> &&_types, bool _devoid)
    : types(std::move(_types)), devoid(_devoid) {
  std::unordered_set<std::string> seen;

  // should make types unique
  std::erase_if(types, [&seen](StrictType &type) {
    return !seen.insert(type.type_name).second;
  });

  if ((types.size() + devoid) < 2)
    throw std::runtime_error("Expected two or more unique types");
}

void printType(const Type &type) {
  if (std::holds_alternative<StrictType>(type)) {
    std::cout << std::get<StrictType>(type).type_name;
    return;
  }

  std::cout << "<";
  if (std::get<VariantType>(type).devoid)
    std::cout << "devoid, ";

  for (const auto &subtype : std::get<VariantType>(type).types)
    std::cout << subtype.type_name << ", ";

  std::cout << ">";
}
