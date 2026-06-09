#pragma once
#include "edenlib/macros.hpp"
#include "edenlib/metaprogramming.hpp"
#include "edenlib/typedefs.hpp"
#include "edenlib/vector16.hpp"
#include "table_and_module_sync.hpp"
#include "types.hpp"

#include <algorithm>
#include <optional>
#include <vector>

namespace LOM {


class SymbolTable {
public:
  struct Variable {
    InstantiatedType type;
    const char* name; u32_t name_len;
    bool is_public;

    Variable() noexcept {}
    Variable(const Type* type, std::string_view name, bool is_public) noexcept
    : type(type), name(name.data()), name_len(name.length()), is_public(is_public), id(u16_max) {}

    Variable(InstantiatedType type, std::string_view name, bool is_public) noexcept
    : type(type), name(name.data()), name_len(name.length()), is_public(is_public), id(u16_max) {}

    [[nodiscard]] std::string_view
    nameof() const noexcept
    { return std::string_view(name, name_len); }

    [[nodiscard]] u16_t
    get_id() const noexcept
    { return id; }


  private: friend class SymbolTable;
    u16_t id;
    Variable(InstantiatedType type, std::string_view name, bool is_public, sz_t v_id) noexcept
    : Variable(type, name, is_public) { id = static_cast<u16_t>(v_id); }
  };

  struct Function {
    eden::vector16<Variable> locals;
    const FunctionType* type;
    const char* name; u32_t name_len;
    bool is_public;

    explicit Function(std::string_view name, std::span<Variable> parameters, const FunctionType* type, bool is_public)
    : type(type), name(name.data()), name_len(name.length()), is_public(is_public), id(u16_max) {
      assert(parameters.size() == type->numParameters());
      for (auto& var : parameters) locals.emplace_back(var);
    }

    [[nodiscard]] u16_t
    get_id() const noexcept
    { return id; }

    [[nodiscard]] std::span<const Variable>
    parameters() const noexcept
    { return std::span(locals).subspan(0, type->numParameters()); }

    [[nodiscard]] const Type*
    returnType() const noexcept
    { return type->returnType(); }

    [[nodiscard]] std::string_view
    nameof() const noexcept
    { return std::string_view(name, name_len); }

    void addVariable(std::string_view variable_name, InstantiatedType variable_instance) noexcept {
      assert(not getVariable(variable_name));
      locals.emplace_back(variable_instance, variable_name, false);
    }

    [[nodiscard]] std::optional<std::pair<InstantiatedType, u32_t>>
    getVariable(std::string_view variable_name) const noexcept {
      sz_t n = locals.size();
      while (n-- not_eq 0)
        if (locals[n].name == variable_name)
          return std::optional(std::pair(locals[n].type, n));
      return std::nullopt;
    }

  private: friend class SymbolTable; friend class Module;
    u16_t id;
    Function(std::string_view name, std::span<Variable> parameters, const FunctionType* type, bool is_public, u16_t f_id)
    : Function(name, parameters, type, is_public) { id = f_id; }
  };

protected:

  // marked mutable because searches need to be possible from a const*, but searches will modify
  mutable std::vector<Variable> variables;
  mutable std::vector<Function> functions;

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another T is added or searched.
  template<class T>
  [[nodiscard]] static T*
  get_if_exists(std::vector<T>& targets, std::string_view target_name) noexcept {
    auto curr = targets.rbegin();
    const auto end = targets.rend();

    while (curr not_eq end) {
      if (curr->nameof() == target_name) {
        std::swap((*curr), targets.back());
        return &targets.back();
      }
      ++curr;
    }
    return nullptr;
  }

  // returns nullptr if non-existent.
  template<class T>
  [[nodiscard]] static T*
  get_if_exists(std::vector<T>& targets, u16_t target_id) noexcept {
    if (targets.empty())
      return nullptr;

    if (targets[target_id].id == target_id)
      return &targets[target_id];

    // sorts array for each miss. should only happen at most once (ideally) after table has been Stabilized
    for (auto i{0uz}; i<targets.size(); ++i) {
      auto& target = targets[i];
      std::swap(target, targets[target.id]);
    }

    if (targets[target_id].id == target_id)
      return &targets[target_id];
    return nullptr;
  }

public:

  // ensure a variable with the same name does not already exist
  void addVariable(InstantiatedType type, std::string_view variable_name, bool is_public) noexcept {
    assert(not get_if_exists(variables, variable_name));
    variables.emplace_back( Variable{type, variable_name, is_public, variables.size()} );
  }

  // ensure a variable with the same name does not already exist
  void addVariable(Variable addition) noexcept {
    assert(not get_if_exists(variables, addition.nameof()));
    addition.id = variables.size();
    variables.emplace_back(addition);
  }

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another global is added or searched.
  [[nodiscard]] const Variable*
  getVariable(std::string_view variable_name) const noexcept
  { return get_if_exists(variables, variable_name); }

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another global is added or searched.
  [[nodiscard]] const Variable*
  getVariable(u16_t variable_id) const noexcept
  { return get_if_exists(variables, variable_id); }

  // returns nullptr if non-existent / not public. pointer is not stable and may be invalidated if another global is added or searched.
  [[nodiscard]] const Variable*
  getPublicVariable(std::string_view variable_name) const noexcept {
    auto const variable = get_if_exists(variables, variable_name);
    if (variable == nullptr or not variable->is_public) return nullptr;
    return variable;
  }

  [[nodiscard]] std::vector<Variable> const&
  getVariableList() const noexcept
  { return variables; }

  [[nodiscard]] std::vector<Function> const&
  getFunctionList() const noexcept
  { return functions; }

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another function is added or searched.
  [[nodiscard]] const Function*
  getFunction(std::string_view function_name) const noexcept
  { return get_if_exists(functions, function_name); }

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another function is added or searched.
  [[nodiscard]] const Function*
  getFunction(u16_t function_id) const noexcept
  { return get_if_exists(functions, function_id); }

  // returns nullptr if non-existent / not public. pointer is not stable and may be invalidated if another function is added or searched.
  [[nodiscard]] const Function*
  getPublicFunction(std::string_view function_name) const noexcept {
    auto const function = get_if_exists(functions, function_name);
    if (function == nullptr or not function->is_public)
      return nullptr;
    return function;
  }

  // ensure that a function with the same name does not already exist
  eden_nonull_args void
  addFunction(std::string_view function_name, std::span<Variable> parameters, const FunctionType* function_type, bool is_public) noexcept {
    assert(not get_if_exists(functions, function_name));
    functions.emplace_back( Function{ function_name, parameters, function_type, is_public, static_cast<u16_t>(functions.size()) } );
  }

};

class Module final : public SymbolTable {
  const char* name; u32_t name_len;

  u32_t current_function_idx{u32_max};
  [[nodiscard]] constexpr bool inFunctionScope() const noexcept {return current_function_idx not_eq u32_max;}

  TypeContext* types;
public:

  explicit Module(std::string_view module_name, TypeContext* type_context) noexcept
  : name(module_name.data()), name_len(module_name.length()), types(type_context) {}

  Module(Module&&) noexcept = default;

  [[nodiscard]] std::string_view
  nameof() const noexcept
  { return std::string_view(name, name_len); }

  eden_return_nonnull
  [[nodiscard]] const Type*
  getRawPointerType(InstantiatedType subtype) const noexcept
  { return types->addRawPointer(subtype); }

  eden_return_nonnull
  [[nodiscard]] const Type*
  getUniquePointerType(InstantiatedType subtype) const noexcept
  { return types->addUniquePointer(subtype); }

  eden_return_nonnull
  [[nodiscard]] const Type*
  getVariantType(std::span<const Type*> subtypes, bool nullable) const noexcept
  { return types->addVariant(subtypes, nullable); }

  eden_return_nonnull eden_nonull_args
  [[nodiscard]] const FunctionType*
  getFunctionType(std::span<Variable> parameters, const Type* return_type, bool is_variadic = false) const noexcept {
    const Type* parameter_types[Settings::MAX_FUNCTION_PARAMETERS];
    auto i{0uz};
    for (; i<parameters.size(); ++i)
      parameter_types[i] = parameters[i].type.type;

    return types->addFunction(std::span(parameter_types, i), return_type, is_variadic);
  }

  eden_return_nonnull const CustomType*
  addCustomType(std::string_view type_name, std::span<Variable> members) const noexcept {
    auto const custom = types->addCustomType(type_name);
    auto const custom_table = custom->member_table();
    for (auto const& member : members)
      custom_table->addVariable(member);
    return custom;
  }

  // returns nullptr if non-existent
  [[nodiscard]] const CustomType*
  getCustomType(std::string_view type_name) const noexcept
  { return types->getCustomType(type_name); }

  [[nodiscard]] bool
  containsLocal(std::string_view local_name) const noexcept {
    assert(inFunctionScope());
    return functions[current_function_idx].getVariable(local_name).has_value();
  }

  // assumes the local does not already exist.
  void addLocal(std::string_view local_name, InstantiatedType local_instance) noexcept {
    assert(inFunctionScope()); assert(not functions[current_function_idx].getVariable(local_name));
    functions[current_function_idx].addVariable(local_name, local_instance);
  }

  [[nodiscard]] auto
  getLocal(std::string_view local_name) const noexcept {
    assert(inFunctionScope());
    return functions[current_function_idx].getVariable(local_name);
  }

  // returns false if function already exists
  eden_nonull_args bool
  addFunction(std::string_view function_name, std::span<Variable> parameters, const Type* return_type, bool is_public, bool is_variadic = false) {
    auto const exists = get_if_exists(functions, function_name);
    if (exists) return false;

    auto const function_type = getFunctionType(parameters, return_type, is_variadic);
    functions.emplace_back( Function {function_name, parameters, function_type, is_public, static_cast<u16_t>(functions.size()) } );
    return true;
  }

  eden_return_nonnull const FunctionType*
  enterFunctionScope(std::string_view function_name) noexcept {
    assert(not inFunctionScope());
    auto const fn = get_if_exists(functions, function_name);
    current_function_idx = functions.size() - 1;
    assert(fn);
    return fn->type;
  }

  void leaveFunctionScope() noexcept
  { assert(inFunctionScope()); current_function_idx = u32_max; }

};


// Stabilized table and module exist so Variable* and Function* can be used stabily
// They only allow for id-based search which is O(1), except for the first search which will sort the array
class StabilizedTable {
  const SymbolTable* table;
public:
  StabilizedTable(const SymbolTable* table) noexcept : table(table) {}

  [[nodiscard]] const SymbolTable::Variable*
  getVariable(u16_t variable_id) const noexcept
  { return table->getVariable(variable_id); }

  [[nodiscard]] const SymbolTable::Function*
  getFunction(u16_t function_id) const noexcept
  { return table->getFunction(function_id); }
};

class StabilizedModule {
  const Module* module;
public:
  StabilizedModule(const Module* module) noexcept : module(module) {}

  [[nodiscard]] std::string_view
  nameof() const noexcept
  { return module->nameof(); }

  [[nodiscard]] const SymbolTable::Variable*
  getVariable(u16_t variable_id) const noexcept
  { return module->getVariable(variable_id); }

  [[nodiscard]] const SymbolTable::Function*
  getFunction(u16_t function_id) const noexcept
  { return module->getFunction(function_id); }
};


#include "table_and_module_sync.hpp"
static_assert(sizeof(SymbolTable) == SYMBOL_TABLE_SIZE);
static_assert(alignof(SymbolTable) == SYMBOL_TABLE_ALIGNMENT);
static_assert(sizeof(Module) == MODULE_SIZE);
static_assert(alignof(Module) == MODULE_ALIGNMENT);

}