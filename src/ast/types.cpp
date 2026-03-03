#include "types.hpp"

#include <stdexcept>

using namespace AST;

[[nodiscard]] Type Type::asImmutable() const noexcept {
    auto c = *this;
    c.is_mutable = false;
    return c;
  }

  [[nodiscard]] bool Type::operator==(const Type& other) const noexcept {
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
  [[nodiscard]] bool Type::convertible_to(const Type& other) const noexcept {
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


  [[nodiscard]] std::string Type::toString() const {
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

