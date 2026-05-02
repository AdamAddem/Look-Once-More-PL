#pragma once
#include "edenlib/macros.hpp"
#include "edenlib/releasing_vector.hpp"
#include "edenlib/typedefs.hpp"
#include "types.hpp"

#include <unordered_map>
#include <variant>
#include <vector>

namespace LOM {

class Module final {
public:
  struct Variable {
    InstantiatedType type;
    std::string_view name;

    Variable() = default;
    Variable(InstantiatedType type, std::string_view name) noexcept
    : type(type), name(name) {}
    Variable(Variable&& other) noexcept = default;
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
    addVariable(std::string_view name, InstantiatedType variable_type) noexcept {
      assert(not getVariable(name));
      locals.emplace_back(variable_type, name);
    }

    [[nodiscard]] std::optional<std::pair<InstantiatedType, u32_t>>
    getVariable(std::string_view name) const noexcept {
      sz_t n = locals.size();
      while (n-- not_eq 0)
        if (locals[n].name == name)
          return std::optional(std::pair(locals[n].type, n));
      return std::nullopt;
    }
  };

  using Symbol = std::variant<InstantiatedType, Function>;
  std::unordered_map<std::string_view, Symbol> symbols;
  Function* current_scope{nullptr};
  std::string_view module_name;
  TypeContext types;

public:

  explicit Module(std::string_view module_name) noexcept : module_name(module_name) {}
  Module(Module&&) noexcept = default;

  [[nodiscard]] std::string_view
  getName() const noexcept {return module_name;}

  //once called, this table may not be used to create any new types.
  //the addFunction, addRawPointer, addUniquePointer, and addVariant methods must not be called.
  [[nodiscard]] TypeContext
  takeTypeContext() noexcept
  {return std::move(types);}

  eden_return_nonnull eden_nonull_args
  const FunctionType* addFunction(
    std::string_view name,
    std::span<Variable> parameters,
    const Type* return_type, bool is_public, bool is_variadic = false);

  eden_return_nonnull
  [[nodiscard]] const Type*
  addRawPointer(InstantiatedType subtype) noexcept
  {return types.addRawPointer(subtype);}

  eden_return_nonnull
  [[nodiscard]] const Type*
  addUniquePointer(InstantiatedType subtype) noexcept
  {return types.addUniquePointer(subtype);}

  eden_return_nonnull
  [[nodiscard]] const Type*
  addVariant(std::vector<const Type* eden_notnullptr> subtypes, bool nullable) noexcept
  {return types.addVariant(std::move(subtypes), nullable);}

  void addGlobalVariable(std::string_view name, InstantiatedType type) noexcept {
    assert(not symbols.contains(name));
    symbols.emplace(std::piecewise_construct,
    std::forward_as_tuple(name),
    std::forward_as_tuple(std::in_place_type<InstantiatedType>, type));
  }

  void addLocalVariable(std::string_view name, InstantiatedType type) const noexcept {
    assume_assert(current_scope not_eq nullptr); assume_assert(type.details.is_public == false);
    current_scope->addVariable(name, type);
  }

  [[nodiscard]] bool
  containsLocal(std::string_view name) noexcept {
    assume_assert(current_scope);
    return current_scope->getVariable(name).has_value();
  }

  [[nodiscard]] bool
  containsGlobal(std::string_view name) noexcept {
    if (not symbols.contains(name))
      return false;
    return std::holds_alternative<InstantiatedType>(symbols[name]);
  }

  [[nodiscard]] auto
  getLocal(std::string_view name) noexcept {
    assume_assert(current_scope);
    return current_scope->getVariable(name);
  }

  [[nodiscard]] std::optional<InstantiatedType>
  getGlobal(std::string_view name) noexcept {
    if (not symbols.contains(name))
      return std::nullopt;
    const auto& symbol = symbols[name];
    if (not std::holds_alternative<InstantiatedType>(symbol))
      return std::nullopt;
    return std::get<InstantiatedType>(symbol);
  }

  [[nodiscard]] std::optional<InstantiatedType>
  getPublicGlobal(std::string_view name) noexcept {
    auto const& res = getGlobal(name);
    if (not res)
      return std::nullopt;
    return res.value().details.is_public ? res : std::nullopt;
  }

  [[nodiscard]] std::optional<const FunctionType*>
  getPublicFunction(std::string_view name) noexcept {
    if (not symbols.contains(name))
      return std::nullopt;

    const auto& symbol = symbols[name];
    if (not std::holds_alternative<Function>(symbol))
      return std::nullopt;

    auto const& func = std::get<Function>(symbol);
    return func.is_public ? std::optional(func.type) : std::nullopt;
  }

  [[nodiscard]] std::optional<const FunctionType*>
  typeOfFunction(std::string_view name) noexcept {
    if (not symbols.contains(name))
      return std::nullopt;
    const auto& symbol = symbols[name];
    return std::holds_alternative<Function>(symbol) ? std::optional(std::get<Function>(symbol).type) : std::nullopt;
  }

  [[nodiscard]] std::pair<std::span<const Variable>, const Type* eden_notnullptr>
  getFunction(std::string_view name) noexcept {
    assert(symbols.contains(name));
    assert(std::holds_alternative<Function>(symbols[name]));
    auto const & f = std::get<Function>(symbols[name]);
    return {f.parameters(), f.returnType()};
  }

  eden_return_nonnull
  const FunctionType*
  enterFunctionScope(std::string_view function_name) noexcept {
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