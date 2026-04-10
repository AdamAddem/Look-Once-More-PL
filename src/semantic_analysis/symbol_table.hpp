#pragma once
#include "edenlib/assume_assert.hpp"
#include "edenlib/releasing_vector.hpp"
#include "edenlib/typedefs.hpp"
#include "types.hpp"

#include <cstring>
#include <unordered_map>
#include <variant>
#include <vector>

namespace LOM {

class SymbolTable final {
public:
  struct Variable {
    InstantiatedType type;
    eden::owned_stringview name;

    Variable() = default;
    Variable(InstantiatedType type, eden::releasing_string::released_ptr name) noexcept
    : type(type), name(std::move(name)) {}
    Variable(Variable&& other) noexcept = default;
    Variable& operator=(Variable&& other) noexcept = default;
    ~Variable() {
      eden::releasing_string::destroy_and_deallocate(
        eden::releasing_string::released_ptr(name.release()));
    }
  };

  class Function {
    u64_t num_parameters;
    std::vector<Variable> locals;
    const FunctionType* type;

    friend class SymbolTable;
    explicit Function(std::span<Variable> parameters, const FunctionType* type)
   : num_parameters(parameters.size()), type(type) {
      assert(parameters.size() <= Settings::MAX_FUNCTION_PARAMETERS);
      for (auto& var : parameters)
        locals.emplace_back(std::move(var));
    }
  public:
    Function(const Function&) = delete;
    Function(Function&&) = default;

    [[nodiscard]] std::span<const Variable>
    parameters() const noexcept
    {return std::span(locals).subspan(0, num_parameters);}

    [[nodiscard]] const Type*
    returnType() const noexcept
    {return type->returnType();}

    [[nodiscard]] bool
    containsVariable(const char* name) const noexcept {
      auto curr = locals.crbegin();
      const auto end = locals.crend();
      while (curr not_eq end) {
        if (std::strcmp(curr->name.get(), name) == 0)
          return true;
        ++curr;
      }

      return false;
    }

    [[nodiscard]] bool
    addVariable(eden::releasing_string::released_ptr name, InstantiatedType variable_type) noexcept {
      if (containsVariable(name.get()))
          return false;

      locals.emplace_back(variable_type, std::move(name));
      return true;
    }

    [[nodiscard]] InstantiatedType
    getVariable(const char* name) const noexcept {
      assert(containsVariable(name));
      auto curr = locals.crbegin();
      const auto end = locals.crend();
      while (curr not_eq end) {
        if (std::strcmp(curr->name.get(), name) == 0)
          return curr->type;
        ++curr;
      }

      std::unreachable();
    }
  };

private:
  using Symbol = std::variant<InstantiatedType, Function>;
  std::unordered_map<std::string_view, Symbol> globals;
  Function* current_scope{nullptr};
  TypeContext types;

public:

  SymbolTable() = default;
  SymbolTable(SymbolTable&& other) noexcept = default;

  void addFunction(
    std::string_view name,
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

  void addGlobalVariable(std::string_view name, InstantiatedType type) {
    assert(not globals.contains(name));
    globals.emplace(name, type);
  }
  void addLocalVariable(eden::releasing_string::released_ptr name, InstantiatedType type) const noexcept {
    assume_assert(current_scope not_eq nullptr);
    const bool res = current_scope->addVariable(std::move(name), type);
    assume_assert(res);
  }

  [[nodiscard]] bool
  containsVariable(const char* name) const noexcept {
    if (current_scope && current_scope->containsVariable(name))
      return true;
    if (globals.contains(name))
      return std::holds_alternative<InstantiatedType>(globals.at(name));
    return false;
  }

  [[nodiscard]] InstantiatedType
  closestVariable(const char* name) noexcept {
    if (current_scope and current_scope->containsVariable(name))
      return current_scope->getVariable(name);

    assert(globals.contains(name));
    assert(std::holds_alternative<InstantiatedType>(globals[name]));
    return std::get<InstantiatedType>(globals.at(name));
  }

  [[nodiscard]] bool
  containsFunction(std::string_view name) noexcept {
    if (globals.contains(name))
      return std::holds_alternative<Function>(globals[name]);
    return false;
  }

  [[nodiscard]] auto
  parametersOfFunction(std::string_view name) noexcept {
    assert(containsFunction(name));
    return std::get<Function>(globals[name]).parameters();
  }

  [[nodiscard]] const Type*
  returnTypeOfFunction(std::string_view name) noexcept {
    assert(containsFunction(name));
    return std::get<Function>(globals[name]).returnType();
  }

  [[nodiscard]] const Function&
  getFunction(std::string_view name) noexcept {
    assert(globals.contains(name));
    assert(std::holds_alternative<Function>(globals[name]));
    return std::get<Function>(globals[name]);
  }

  [[nodiscard]] bool
  containsSymbol(const char* name) noexcept
  {return containsVariable(name) or containsFunction(name);}

  [[nodiscard]] bool
  isClosestSymbolAVariable(const char* name) noexcept {
    if (containsVariable(name))
      return true;

    assert(containsFunction(name));
    return false;
  }

  const FunctionType*
  enterFunctionScope(std::string_view function_name) noexcept {
    assume_assert(current_scope == nullptr);
    assert(containsFunction(function_name));
    current_scope = &std::get<Function>(globals[function_name]);
    return current_scope->type;
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