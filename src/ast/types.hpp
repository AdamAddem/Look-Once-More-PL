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
  struct Primitive {};
  struct Pointer {};
  struct Variant {};
  struct Function {};
public:
  static constexpr Normal normal{};
  static constexpr Primitive primitive{};
  static constexpr Pointer pointer{};
  static constexpr Variant variant{};
  static constexpr Function function{};

  //normal and pointer use std::string
  //variant and function use Types
  std::variant<std::string, Types> type_name;
  nullable_owned_ptr<Type> subtype;

  struct Details {
    bool is_mutable : 1 = false;
    bool arithmetic : 1 = false;
    bool is_primitive : 1 = false;
    bool callable : 1 = false;
    bool array : 1 = false;
  }details;

  constexpr explicit Type(Normal) {}
  constexpr explicit Type(Primitive, PrimitiveType prim, const bool is_mutable) : type_name(std::in_place_type<std::string>) {
    details.is_primitive = true;
    switch (prim) {
      case PrimitiveType::I8:
      case PrimitiveType::I16:
      case PrimitiveType::I32:
      case PrimitiveType::I64:
      case PrimitiveType::U8:
      case PrimitiveType::U16:
      case PrimitiveType::U32:
      case PrimitiveType::U64:
      case PrimitiveType::F32:
      case PrimitiveType::F64:
        details.arithmetic = true;

    default:
      break;
    }

    details.is_mutable = is_mutable;
    switch (prim) { // this is very dumb
    case PrimitiveType::I8:
      type_name.emplace<std::string>("i8");
      return;
    case PrimitiveType::I16:
      type_name.emplace<std::string>("i16");
      return;
    case PrimitiveType::I32:
      type_name.emplace<std::string>("i32");
      return;
    case PrimitiveType::I64:
      type_name.emplace<std::string>("i64");
      return;
    case PrimitiveType::U8:
      type_name.emplace<std::string>("u8");
      return;
    case PrimitiveType::U16:
      type_name.emplace<std::string>("u16");
      return;
    case PrimitiveType::U32:
      type_name.emplace<std::string>("u32");
      return;
    case PrimitiveType::U64:
      type_name.emplace<std::string>("u64");
      return;
    case PrimitiveType::F32:
      type_name.emplace<std::string>("f32");
      return;
    case PrimitiveType::F64:
      type_name.emplace<std::string>("f64");
      return;
    case PrimitiveType::BOOL:
      type_name.emplace<std::string>("bool");
      return;
    case PrimitiveType::CHAR:
      type_name.emplace<std::string>("char");
      return;
    case PrimitiveType::STRING:
      type_name.emplace<std::string>("string");
      return;
    case PrimitiveType::DEVOID:
      type_name.emplace<std::string>("");
      return;
    case PrimitiveType::RAW:
      type_name.emplace<std::string>("raw");
      return;
    case PrimitiveType::UNIQUE:
      type_name.emplace<std::string>("unique");
      return;
    case PrimitiveType::VAGUE:
      type_name.emplace<std::string>("vague");
      return;
    default:
      assert(false);
    }
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


  [[nodiscard]] bool isIntegral() const noexcept {
    if (!details.is_primitive) return false;

    switch (std::get<std::string>(type_name).front()) {// so dumb
    case 'i':
    case 'u':
      return true;

    default:
      return false;
    }
  }

  [[nodiscard]] bool isFloat() const noexcept {
    if (!details.is_primitive) return false;

    switch (std::get<std::string>(type_name).front()) {// so dumb
    case 'f':
    case 'd':
      return true;

    default:
      return false;
    }
  }

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

static constexpr Type devoid_type{Type::primitive, PrimitiveType::DEVOID, false};
static constexpr Type int_literal_type{Type::primitive, PrimitiveType::I64, false};
static constexpr Type float_literal_type{Type::primitive, PrimitiveType::F32, false};
static constexpr Type double_literal_type{Type::primitive, PrimitiveType::F64, false};
static constexpr Type char_literal_type{Type::primitive, PrimitiveType::CHAR, false};
static constexpr Type string_literal_type{Type::primitive, PrimitiveType::STRING, false};
static constexpr Type bool_literal_type{Type::primitive, PrimitiveType::BOOL, false};

}