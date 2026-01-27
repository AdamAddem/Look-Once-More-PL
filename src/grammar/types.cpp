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

bool VariantType::operator==(const VariantType &other) const {
  return (types == other.types) && (devoid == other.devoid);
}

bool VariantType::isConvertibleTo(const VariantType &other) const {
  return *this == other;
}

bool VariantType::canBeConvertedToSubtype(const StrictType &type) const {
  for (const auto &subtype : types) {
    if (subtype.isConvertibleTo(type))
      return true;
  }

  return false;
}

bool StrictType::operator==(const StrictType &other) const {
  return type_name == other.type_name;
}

bool StrictType::isConvertibleTo(const StrictType &other) const {
  return *this == other; // temporary. does not perform any conversion checking,
                         // just checks if exactly equal
}

// conversion from strict->strict, strict->variant, variant->variant supported.
bool convertibleFromTo(const Type &from, const Type &to) {

  if (const auto f = std::get_if<StrictType>(&from)) {
    if (const auto t = std::get_if<StrictType>(&to))
      return f->isConvertibleTo(*t);
    else
      return std::get<VariantType>(to).canBeConvertedToSubtype(*f);
  }

  if (auto t = std::get_if<VariantType>(&to))
    return std::get<VariantType>(from).isConvertibleTo(*t);

  return false; // false if Variant->Strict
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

  std::cout << "\b\b";

  std::cout << ">";
}
