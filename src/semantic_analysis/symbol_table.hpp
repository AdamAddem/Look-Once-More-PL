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
    bool is_public;

    friend class Module;
    explicit Function(std::span<Variable> parameters, const FunctionType* type, bool is_public)
   : type(type), is_public(is_public) {
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

    void
    addVariable(eden::releasing_string::released_ptr name, InstantiatedType variable_type) noexcept {
      assert(not getVariable(name.get()));
      locals.emplace_back(variable_type, std::move(name));
    }

    [[nodiscard]] std::optional<std::pair<InstantiatedType, u32_t>>
    getVariable(const char* name) const noexcept {
      sz_t n = locals.size();
      while (n-- not_eq 0)
        if (std::strcmp(locals[n].name.get(), name) == 0)
          return std::optional(std::pair(locals[n].type, n));
      return std::nullopt;
    }
  };

  using Symbol = std::variant<InstantiatedType, Function>;
  std::unordered_map<std::string_view, Symbol> symbols;
  Function* current_scope{nullptr};
  std::string_view name;
  TypeContext types;

public:

  explicit Module(std::string_view module_name) noexcept : name(module_name) {}
  Module(Module&&) noexcept = default;

  [[nodiscard]] std::string_view
  getName() const noexcept {return name;}

  //once called, this table may not be used to create any new types.
  //the addFunction, addRawPointer, addUniquePointer, and addVariant methods must not be called.
  [[nodiscard]] TypeContext
  takeTypeContext() noexcept
  {return std::move(types);}

  const FunctionType* addFunction(
    const eden::owned_stringview& name,
    std::span<Variable> parameters,
    const Type* return_type, bool is_public, bool is_variadic = false) noexcept;

  [[nodiscard]] const Type*
  addRawPointer(InstantiatedType subtype) noexcept
  {return types.addRawPointer(subtype);}

  [[nodiscard]] const Type*
  addUniquePointer(InstantiatedType subtype) noexcept
  {return types.addUniquePointer(subtype);}

  [[nodiscard]] const Type*
  addVariant(std::vector<const Type*> subtypes, bool nullable) noexcept
  {return types.addVariant(std::move(subtypes), nullable);}

  void addGlobalVariable(std::string_view name, InstantiatedType type) noexcept {
    assert(not symbols.contains(name));
    symbols.emplace(std::piecewise_construct,
    std::forward_as_tuple(name),
    std::forward_as_tuple(std::in_place_type<InstantiatedType>, type));
  }

  void addLocalVariable(eden::releasing_string::released_ptr name, InstantiatedType type) const noexcept {
    assume_assert(current_scope not_eq nullptr); assume_assert(type.details.is_public == false);
    current_scope->addVariable(std::move(name), type);
  }

  [[nodiscard]] bool
  containsLocal(const char* name) noexcept {
    assume_assert(current_scope);
    return current_scope->getVariable(name).has_value();
  }

  [[nodiscard]] bool
  containsGlobal(const char* name) noexcept {
    if (not symbols.contains(name))
      return false;
    return std::holds_alternative<InstantiatedType>(symbols[name]);
  }

  [[nodiscard]] auto
  getLocal(const char* name) noexcept {
    assume_assert(current_scope);
    return current_scope->getVariable(name);
  }

  [[nodiscard]] std::optional<InstantiatedType>
  getGlobal(const char* name) noexcept {
    if (not symbols.contains(name))
      return std::nullopt;
    const auto& symbol = symbols[name];
    if (not std::holds_alternative<InstantiatedType>(symbol))
      return std::nullopt;
    return std::get<InstantiatedType>(symbol);
  }

  [[nodiscard]] std::optional<InstantiatedType>
  getPublicGlobal(const char* name) noexcept {
    auto const& res = getGlobal(name);
    if (not res)
      return std::nullopt;
    return res.value().details.is_public ? res : std::nullopt;
  }

  [[nodiscard]] std::optional<const FunctionType*>
  getPublicFunction(const char* name) noexcept {
    if (not symbols.contains(name))
      return std::nullopt;

    const auto& symbol = symbols[name];
    if (not std::holds_alternative<Function>(symbol))
      return std::nullopt;

    auto const& func = std::get<Function>(symbol);
    return func.is_public ? std::optional(func.type) : std::nullopt;
  }

  [[nodiscard]] std::optional<const FunctionType*>
  typeOfFunction(const char* name) noexcept {
    if (not symbols.contains(name))
      return std::nullopt;
    const auto& symbol = symbols[name];
    return std::holds_alternative<Function>(symbol) ? std::optional(std::get<Function>(symbol).type) : std::nullopt;
  }

  [[nodiscard]] std::pair<std::span<const Variable>, const Type*>
  getFunction(const char* name) noexcept {
    assert(symbols.contains(name));
    assert(std::holds_alternative<Function>(symbols[name]));
    auto const & f = std::get<Function>(symbols[name]);
    return {f.parameters(), f.returnType()};
  }

  const FunctionType*
  enterFunctionScope(const char* function_name) noexcept {
    assume_assert(current_scope == nullptr); assert(symbols.contains(function_name)); assert(std::holds_alternative<Function>(symbols[function_name]));
    current_scope = &std::get<Function>(symbols[function_name]);
    return current_scope->type;
  }

  void leaveFunctionScope() noexcept {
    assume_assert(current_scope);
    current_scope = nullptr;
  }
};

}