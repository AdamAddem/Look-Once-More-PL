#pragma once
#include "utilities/general_enums.hpp"
#include "utilities/owned_ptr.hpp"

#include <cassert>
#include <string>
#include <utility>
#include <variant>
#include <vector>



namespace AST {

class Type;
using Types = std::vector<Type>;

class Type {
  struct Normal {};
  struct Pointer {};
  struct Variant {};
  struct Function {};
public:
  static constexpr Normal normal{};
  static constexpr Pointer pointer{};
  static constexpr Variant variant{};
  static constexpr Function function{};

  //normal and pointer use std::string
  //variant and function use Types
  std::variant<std::string, Types> type_name;
  nullable_owned_ptr<Type> subtype;

  struct Details {
    bool is_mutable{false};
    bool arithmetic{false};
    bool callable{false};
    bool array{false};
  }details;

  constexpr explicit Type(Normal) {}
  constexpr explicit Type(Normal, std::string primitive_typename, const bool is_mutable) : type_name(std::in_place_type<std::string>) {
    switch (primitive_typename.front()) { // this is so dumb
    case 'i':
    case 'u':
    case 'f':
      details.arithmetic = true;
    default:
      break;
    }
    details.is_mutable = is_mutable;

    type_name.emplace<std::string>(std::move(primitive_typename));
  }
  constexpr explicit Type(Normal, std::string type_name, Details typedetails)
  : type_name(std::move(type_name)), details(typedetails) {}

  constexpr explicit Type(Pointer, PointerType ptr_type, nullable_owned_ptr<Type> subtype): subtype(std::move(subtype)) {
    switch (ptr_type) {
    case PointerType::RAW:
      type_name.emplace<std::string>("raw"); return;
    case PointerType::UNIQUE:
      type_name.emplace<std::string>("unique"); return;
    case PointerType::VAGUE:
      type_name.emplace<std::string>("vague"); return;

    default: assert(false);
    }
  }

  constexpr explicit Type(Function, Types parameter_types, Type return_type) : type_name(std::move(parameter_types)) {
    std::get<Types>(type_name).emplace_back(std::move(return_type));
    details.callable = true;
  }

  constexpr explicit Type(Variant) : type_name(std::in_place_type<Types>) {}
  constexpr explicit Type(Variant, Type initial_type): type_name(std::in_place_type<Types>)
  { std::get<Types>(type_name).emplace_back(std::move(initial_type)); }

  explicit constexpr Type(Variant, Types subtypes) : type_name(std::move(subtypes)) {}

  constexpr Type(const Type& other) : type_name(other.type_name), details(other.details) {
    if (other.subtype)
      subtype = new Type(*other.subtype);
  }
  constexpr Type& operator=(const Type& other) {
    type_name = other.type_name;
    if (other.subtype)
      subtype = new Type(*other.subtype);
    details = other.details;
    return *this;
  }
  constexpr Type(Type&& other) noexcept : type_name(std::move(other.type_name)), subtype(std::move(other.subtype)), details(other.details) {}
  constexpr Type& operator=(Type&& other) noexcept { type_name = std::move(other.type_name); subtype = std::move(other.subtype); details = other.details; return *this; }

  constexpr ~Type() {subtype.destroy();}

  [[nodiscard]] bool isVariant() const noexcept {return std::holds_alternative<Types>(type_name);}
  [[nodiscard]] bool isDevoid() const noexcept {return std::holds_alternative<std::string>(type_name) && std::get<std::string>(type_name).empty();}
  [[nodiscard]] bool isBool() const noexcept {return std::holds_alternative<std::string>(type_name) && (std::get<std::string>(type_name) == "bool");}
  [[nodiscard]] const std::string& getTypename() const noexcept {return std::get<std::string>(type_name);}
  [[nodiscard]] std::string takeTypename() noexcept {return std::get<std::string>(std::move(type_name));}

  [[nodiscard]] const Types& getTypes() const noexcept {return std::get<Types>(type_name);}
  [[nodiscard]] Types takeTypes() noexcept {return std::get<Types>(std::move(type_name));}
  [[nodiscard]] unsigned numVariantTypes() const noexcept {return std::get<Types>(type_name).size();}
  void addTypeToVariantList(Type new_type) noexcept { std::get<Types>(type_name).emplace_back(std::move(new_type)); }


  //these two are so fucked. just come up with official rules for type conversion already
  [[nodiscard]] bool operator==(const Type& other) const noexcept;
  [[nodiscard]] bool convertible_to(const Type& other) const noexcept;

  [[nodiscard]] std::string toString() const;
};

static constexpr Type devoid_type{Type::normal, "", Type::Details{.arithmetic = false, .callable = false, .array = false}};
static constexpr Type int_literal_type{Type::normal, "i32", Type::Details{.arithmetic = true, .callable = false, .array = false}};
static constexpr Type float_literal_type{Type::normal, "f32", Type::Details{.arithmetic = true, .callable = false, .array = false}};
static constexpr Type double_literal_type{Type::normal, "f64", Type::Details{.arithmetic = true, .callable = false, .array = false}};
static constexpr Type char_literal_type{Type::normal, "char", Type::Details{.arithmetic = false, .callable = false, .array = false}};
static constexpr Type string_literal_type{Type::normal, "string", Type::Details{.arithmetic = false, .callable = false, .array = false}};
static constexpr Type bool_literal_type{Type::normal, "bool", Type::Details{.arithmetic = false, .callable = false, .array = false}};

}