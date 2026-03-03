#pragma once
#include <string>
#include <utility>
#include <variant>
#include <vector>



namespace AST {
struct Type;
using Types = std::vector<Type>;

struct Type {
private:
  struct Normal {};
  struct Variant {};
  struct Function {};
public:
  static constexpr Normal normal{};
  static constexpr Variant variant{};
  static constexpr Function function{};

  std::variant<std::string, Types> type_name;
  bool is_mutable;
  Type* subtype; //heap allocated

  constexpr explicit Type(Normal) : is_mutable(false), subtype(nullptr) {}
  constexpr explicit Type(Variant) : type_name(Types()), is_mutable(false), subtype(nullptr) {}
  constexpr explicit Type(Variant, Type initial_type) : type_name(Types({std::move(initial_type)})), is_mutable(false), subtype(nullptr) {}

  constexpr explicit Type(Function) : is_mutable(false), subtype(nullptr) {}
  constexpr explicit Type(Function, std::string function_name) : type_name(std::move(function_name)), is_mutable(false), subtype(nullptr) {}

  constexpr Type(std::string type_name, const bool is_mutable, Type* subtype = nullptr) :
  type_name(std::move(type_name)), is_mutable(is_mutable), subtype(subtype) {}

  constexpr Type(std::vector<Type> subtypes, const bool is_mutable) :
  type_name(std::move(subtypes)), is_mutable(is_mutable), subtype(nullptr) {}

  [[nodiscard]] bool isVariant() const noexcept {return std::holds_alternative<Types>(type_name);}
  [[nodiscard]] bool isDevoid() const noexcept {return std::holds_alternative<std::string>(type_name) && std::get<std::string>(type_name).empty();}
  [[nodiscard]] Type asImmutable() const noexcept;
  [[nodiscard]] std::string getTypename() const noexcept {return std::get<std::string>(type_name);}
  [[nodiscard]] std::string takeTypename() noexcept {return std::get<std::string>(std::move(type_name));}

  [[nodiscard]] Types getTypes() const noexcept {return std::get<Types>(type_name);}
  [[nodiscard]] Types takeTypes() noexcept {return std::get<Types>(std::move(type_name));}
  [[nodiscard]] unsigned numVariantTypes() const noexcept {return std::get<Types>(type_name).size();}
  void addTypeToVariantList(Type new_type) noexcept { std::get<Types>(type_name).emplace_back(std::move(new_type)); }



  //these two are so fucked. just come up with official rules for type conversion already jesus christ
  [[nodiscard]] bool operator==(const Type& other) const noexcept;
  [[nodiscard]] bool convertible_to(const Type& other) const noexcept;

  [[nodiscard]] std::string toString() const;
};

static constexpr Type devoid_type{"", false};
static constexpr Type int_literal_type = {"i32", false};
static constexpr Type float_literal_type = {"f32", false};
static constexpr Type double_literal_type = {"f64", false};
static constexpr Type bool_literal_type = {"bool", false};
static constexpr Type char_literal_type = {"char", false};
static constexpr Type string_literal_type = {"string", false};
}