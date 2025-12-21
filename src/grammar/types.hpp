#pragma once
#include <string>
#include <variant>
#include <vector>

struct StrictType {
  std::string type_name;
  explicit StrictType(std::string &&_typename)
      : type_name(std::move(_typename)) {}
  StrictType(const StrictType &) = default;
  StrictType &operator=(const StrictType &) = default;
  StrictType(StrictType &&other) noexcept
      : type_name(std::move(other.type_name)) {}
};

struct VariantType {
  std::vector<StrictType> types;
  bool devoid;

  explicit VariantType(std::vector<StrictType> &&_types, bool _devoid);
  VariantType(const VariantType &) = default;
  VariantType(VariantType &&other) noexcept
      : types(std::move(other.types)), devoid(other.devoid) {}
};
using Type = std::variant<StrictType, VariantType>;

void printType(const Type &type);
