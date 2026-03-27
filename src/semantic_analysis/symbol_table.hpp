#pragma once
#include "types.hpp"
#include "utilities/assume_assert.hpp"
#include "utilities/typedefs.hpp"

#include <unordered_map>
#include <variant>
#include <vector>

namespace LOM {

class SymbolTable final {
public:
  struct Variable {
    InstantiatedType type;
    std::string name;
  };
private:
  class Function {
    u64_t num_parameters;
    std::vector<Variable> locals;
    const FunctionType* type;
  public:

    explicit Function(std::span<Variable> parameters, const FunctionType* type)
    : num_parameters(parameters.size()), type(type) {
      assume_assert(parameters.size() less_eq Settings::MAX_FUNCTION_PARAMETERS);
      for (const auto& var : parameters)
        locals.emplace_back(var);
    }

    [[nodiscard]] auto
    parameters() const noexcept{return std::span(locals).subspan(0, num_parameters);}

    [[nodiscard]] const Type*
    returnType() const noexcept {return type->returnType();}

    [[nodiscard]] bool
    containsVariable(const std::string& name) const noexcept {
      auto curr = locals.crbegin();
      const auto end = locals.crend();
      while (curr not_eq end) {
        if (curr->name eq name)
          return true;

        ++curr;
      }

      return false;
    }

    [[nodiscard]] bool
    addVariable(std::string name, InstantiatedType type) noexcept {
      if (containsVariable(name))
          return false;

      locals.emplace_back(type, std::move(name));
      return true;
    }

    [[nodiscard]] InstantiatedType
    getVariable(const std::string& name) const noexcept {
      assume_assert(containsVariable(name));
      auto curr = locals.crbegin();
      const auto end = locals.crend();
      while (curr not_eq end) {
        if (curr->name eq name)
          return curr->type;

        ++curr;
      }

      std::unreachable();
    }
  };

  using Symbol = std::variant<InstantiatedType, Function>;
  std::unordered_map<std::string, Symbol> globals;
  Function* current_scope{nullptr};

  TypeContext types;
public:

  SymbolTable() = default;
  SymbolTable(SymbolTable&& other) noexcept = default;

  void addFunction(
    const std::string &name,
    std::span<Variable> parameters,
    const Type* return_type) noexcept;

  [[nodiscard]] const Type*
  addRawPointer(InstantiatedType subtype) noexcept
  {return types.addRawPointer(subtype);}

  [[nodiscard]] const Type*
  addUniquePointer(InstantiatedType subtype) noexcept
  {return types.addUniquePointer(subtype);}

  [[nodiscard]] const Type*
  addVariant(std::vector<const Type*> subtypes, bool nullable) noexcept
  {return types.addVariant(std::move(subtypes), nullable);}

  void addVariable(std::string name, InstantiatedType type) {
    if (current_scope)
      return addLocalVariable(std::move(name), type);

    return addGlobalVariable(std::move(name), type);
  }
  void addGlobalVariable(std::string name, InstantiatedType type) {
    assume_assert(not globals.contains(name));
    globals.emplace(std::move(name), type);
  }
  void addLocalVariable(std::string name, InstantiatedType type) const noexcept {
    assume_assert(current_scope not_eq nullptr);
    const bool res = current_scope->addVariable(std::move(name), type);
    assume_assert(res);
  }

  [[nodiscard]] bool
  containsVariable(const std::string &name) const noexcept {
    if (current_scope && current_scope->containsVariable(name))
      return true;
    if (globals.contains(name))
      return std::holds_alternative<InstantiatedType>(globals.at(name));
    return false;
  }

  [[nodiscard]] InstantiatedType
  closestVariable(const std::string &name) noexcept {
    if (current_scope and current_scope->containsVariable(name))
      return current_scope->getVariable(name);

    assume_assert(globals.contains(name));
    assume_assert(std::holds_alternative<InstantiatedType>(globals[name]));
    return std::get<InstantiatedType>(globals.at(name));
  }

  [[nodiscard]] bool
  containsFunction(const std::string &name) noexcept {
    if (globals.contains(name))
      return std::holds_alternative<Function>(globals[name]);
    return false;
  }

  [[nodiscard]] auto
  parametersOfFunction(const std::string& name) noexcept {
    assume_assert(containsFunction(name));
    return std::get<Function>(globals[name]).parameters();
  }

  [[nodiscard]] const Type*
  returnTypeOfFunction(const std::string& name) noexcept {
    assume_assert(containsFunction(name));
    return std::get<Function>(globals[name]).returnType();
  }

  [[nodiscard]] bool
  containsSymbol(const std::string &name) noexcept {return containsVariable(name) or containsFunction(name);}

  [[nodiscard]] bool
  isClosestSymbolAVariable(const std::string &name) noexcept {
    if (containsVariable(name))
      return true;

    assume_assert(containsFunction(name));
    return false;
  }

  void enterFunctionScope(const std::string& function_name) noexcept {
    assume_assert(current_scope eq nullptr);
    assume_assert(containsFunction(function_name));
    current_scope = &std::get<Function>(globals[function_name]);
  }
  void leaveFunctionScope() noexcept {
    assume_assert(current_scope);
    current_scope = nullptr;
  }

  [[nodiscard]] const Type*
  returnTypeOfCurrentFunction() const noexcept {
    assume_assert(current_scope);
    return current_scope->returnType();
  }
};

}