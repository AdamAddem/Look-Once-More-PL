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

class Module final {
public:
  struct Variable {
    InstantiatedType type;
    eden::releasing_string::released_span name;

    Variable() = default;
    Variable(InstantiatedType type, eden::releasing_string::released_span name) noexcept
    : type(type), name(std::move(name)) {}
    Variable(Variable&& other) noexcept = default;
    ~Variable() {name.destroy_and_deallocate();}
  };

private:
  class Function {
    std::vector<Variable> locals;
    const FunctionType* type;

    friend class Module;
    explicit Function(std::span<Variable> parameters, const FunctionType* type)
   : type(type) {
      assert(parameters.size() == type->numParameters());
      for (auto& var : parameters)
        locals.emplace_back(std::move(var));
    }
  public:
    Function(const Function&) = delete;
    Function(Function&&) = default;

    [[nodiscard]] std::span<const Variable>
    parameters() const noexcept
    {return std::span(locals).subspan(0, type->numParameters());}

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

    void
    addVariable(eden::releasing_string::released_ptr name, InstantiatedType variable_type) noexcept {
      assert(not containsVariable(name.get()));
      locals.emplace_back(variable_type, std::move(name));
    }

    [[nodiscard]] std::pair<InstantiatedType, u32_t>
    getVariable(const char* name) const noexcept {
      assert(containsVariable(name));
      sz_t n = locals.size();
      while (n-- not_eq 0) {
        if (std::strcmp(locals[n].name.get(), name) == 0)
          return {locals[n].type, n};
      }

      std::unreachable();
    }
  };

  class PrivateFunction : public Function {};
  class PrivateGlobal : public InstantiatedType {};

  using Symbol = std::variant<InstantiatedType, Function, PrivateGlobal, PrivateFunction>;
  std::unordered_map<std::string_view, Symbol> symbols;
  Function* current_scope{nullptr};
  TypeContext types;

public:
  Module() noexcept = default;
  Module(Module&&) noexcept = default;

  //once called, this table may not be used to create any new types.
  //the addFunction, addRawPointer, addUniquePointer, and addVariant methods must not be called.
  [[nodiscard]] TypeContext
  takeTypeContext() noexcept
  {return std::move(types);}

  const FunctionType* addFunction(
    const eden::owned_stringview& name,
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

  void addGlobalVariable(std::string_view name, InstantiatedType type, bool is_public = false) noexcept {
    assert(not symbols.contains(name));
    if (is_public) {
      symbols.emplace(std::piecewise_construct,
      std::forward_as_tuple(name),
      std::forward_as_tuple(std::in_place_type<InstantiatedType>, type));
      return;
    }

    symbols.emplace( std::piecewise_construct,
    std::forward_as_tuple(name),
    std::forward_as_tuple(std::in_place_type<PrivateGlobal>, type));
  }

  void addLocalVariable(eden::releasing_string::released_ptr name, InstantiatedType type) const noexcept {
    assume_assert(current_scope not_eq nullptr);
    current_scope->addVariable(std::move(name), type);
  }

  [[nodiscard]] bool
  containsLocal(const char* name) noexcept {
    assume_assert(current_scope);
    return current_scope->containsVariable(name);
  }

  [[nodiscard]] bool
  containsGlobal(const char* name) noexcept {
    if (not symbols.contains(name))
      return false;
    return std::holds_alternative<InstantiatedType>(symbols[name]) or std::holds_alternative<PrivateGlobal>(symbols[name]);
  }

  [[nodiscard]] bool
  containsPublicGlobal(const char* name) noexcept {
    if (not symbols.contains(name))
      return false;
    return std::holds_alternative<InstantiatedType>(symbols[name]);
  }

  [[nodiscard]] auto
  getLocal(const char* name) noexcept {
    assume_assert(current_scope);
    return current_scope->getVariable(name);
  }

  [[nodiscard]] InstantiatedType
  getGlobal(const char* name) noexcept {
    if (std::holds_alternative<InstantiatedType>(symbols[name]))
      return std::get<InstantiatedType>(symbols[name]);
    if (std::holds_alternative<PrivateGlobal>(symbols[name]))
      return std::get<PrivateGlobal>(symbols[name]);
    std::unreachable();
  }

  [[nodiscard]] InstantiatedType
  getPublicGlobal(const char* name) noexcept {
    assert(std::holds_alternative<InstantiatedType>(symbols[name]));
    return std::get<InstantiatedType>(symbols[name]);
  }

  [[nodiscard]] bool
  containsFunction(const char* name) noexcept {
    if (symbols.contains(name))
      return std::holds_alternative<Function>(symbols[name]) or std::holds_alternative<PrivateFunction>(symbols[name]);
    return false;
  }

  [[nodiscard]] bool
  containsPublicFunction(const char* name) noexcept {
    if (symbols.contains(name))
      return std::holds_alternative<Function>(symbols[name]);
    return false;
  }

  [[nodiscard]] const FunctionType*
  typeOfFunction(const char* name) noexcept {
    assert(containsFunction(name));
    return std::get<Function>(symbols[name]).type;
  }

  [[nodiscard]] std::pair<std::span<const Variable>, const Type*>
  getFunction(const char* name) noexcept {
    assert(symbols.contains(name));
    assert(std::holds_alternative<Function>(symbols[name]));
    const auto & f = std::get<Function>(symbols[name]);
    return {f.parameters(), f.returnType()};
  }

  const FunctionType*
  enterFunctionScope(const char* function_name) noexcept {
    assume_assert(current_scope == nullptr); assert(containsFunction(function_name));
    current_scope = &std::get<Function>(symbols[function_name]);
    return current_scope->type;
  }

  void leaveFunctionScope() noexcept {
    assume_assert(current_scope);
    current_scope = nullptr;
  }
};

}