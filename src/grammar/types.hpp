#pragma once
#include <string>
#include <utility>
#include <variant>
#include <vector>
#include <iostream>




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

  [[nodiscard]] Type asImmutable() const noexcept {
    auto c = *this;
    c.is_mutable = false;
    return c;
  }

  [[nodiscard]] std::string getTypename() const noexcept {return std::get<std::string>(type_name);}
  [[nodiscard]] std::string takeTypename() noexcept {return std::get<std::string>(std::move(type_name));}

  [[nodiscard]] Types getTypes() const noexcept {return std::get<Types>(type_name);}
  [[nodiscard]] Types takeTypes() noexcept {return std::get<Types>(std::move(type_name));}
  [[nodiscard]] unsigned numVariantTypes() const noexcept {return std::get<Types>(type_name).size();}
                void addTypeToVariantList(Type new_type) noexcept {
    std::get<Types>(type_name).emplace_back(std::move(new_type));
  }



  [[nodiscard]] bool operator==(const Type& other) const noexcept {
    if (std::holds_alternative<std::string>(type_name)) {
      if (!std::holds_alternative<std::string>(other.type_name))
        return false;

      return std::get<std::string>(type_name) == std::get<std::string>(other.type_name);
    }

    if (!std::holds_alternative<Types>(other.type_name))
      return false;

    const auto myvariant = std::get<Types>(type_name);
    const auto othervariant = std::get<Types>(other.type_name);
    const auto mysz = myvariant.size();
    if (mysz != othervariant.size())
      return false;

    for (auto i{0uz}; i<mysz; ++i) {
      if (myvariant[i] != othervariant[i])
        return false;
    }

    return true;
  }

  //this entire thing is so fucked. just come up with official rules for type conversion already jesus christ
  [[nodiscard]] bool convertible_to(const Type& other) const noexcept {
    const auto my_variant = std::get_if<Types>(&type_name);
    const auto other_variant = std::get_if<Types>(&other.type_name);
    if (my_variant) {
      if (other_variant) {
        const auto mysize = my_variant->size();
        if (mysize != other_variant->size())
          return false;

        for (auto i{0uz}; i<mysize; ++i) { //currently, for variants to be convertible, every subtype must be exactly equal and declared in the same order
          if (my_variant->at(i) != other_variant->at(i))
            return false;
        }

        return true;
      }

      return false; //implicit conversion from variant -> type not supported
    }

    const auto& my_typename = std::get<std::string>(type_name);
    if (!other_variant)
      return my_typename == std::get<std::string>(other.type_name);

    for (const auto& t : *other_variant) {
      if (std::get<std::string>(t.type_name) == my_typename)
        return true;
    }

    return false;
  }


  [[nodiscard]] std::string toString() const {
    std::string retval;
    if (is_mutable)
      retval.append("mut ");
    if (subtype != nullptr) {
      if (getTypename().empty())
        throw std::runtime_error("Printing for references not supported!");

      retval.append(getTypename());
      retval.append(" -> ");
      retval.append(subtype->toString());
      return retval;
    }

    if (std::holds_alternative<std::string>(type_name)) {
      retval.append(std::get<std::string>(type_name));
    }
    else {
      const auto& types = std::get<Types>(type_name);

      retval.push_back('<');
      for (const auto& type : types) {
        retval.append(type.toString());
        retval.append(", ");
      }
      retval.pop_back();
      retval.pop_back();
      retval.push_back('>');
    }
     return retval;
  }

  void print() const {
    std::cout << toString();
  }
};

static constexpr Type devoid_type{"", false};
static constexpr Type int_literal_type = {"i32", false};
static constexpr Type float_literal_type = {"f32", false};
static constexpr Type double_literal_type = {"f64", false};
static constexpr Type bool_literal_type = {"bool", false};
static constexpr Type char_literal_type = {"char", false};
static constexpr Type string_literal_type = {"string", false};