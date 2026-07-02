#pragma once
#include "edenlib/macros.hpp"
#include "edenlib/metaprogramming/type_class.hpp"
#include "edenlib/typedefs.hpp"
#include "edenlib/vectors/vector.hpp"
#include "table_and_module_sync.hpp"
#include "types.hpp"

#include <optional>

namespace LOM {


class SymbolTable {
public:
  static constexpr u16_t INVALID_ID = u16_max;
  struct Variable {
    // members are organized in this stupid way to shave 8 bytes
    [[no_unique_address]] QualifiedType type;
    bool is_public;
  private:
    u16_t id;
    u32_t name_len;
    char const* name;
  public:

    Variable() noexcept = default;
    Variable(Type const* type, std::string_view name, bool is_public) noexcept
    : type(type), is_public(is_public), id(INVALID_ID), name_len(name.length()), name(name.data()) {}

    Variable(QualifiedType qualified, std::string_view name, bool is_public) noexcept
    : type(qualified), is_public(is_public), id(INVALID_ID), name_len(name.length()), name(name.data()) {}

    [[nodiscard]] std::string_view
    nameof() const noexcept
    { return std::string_view(name, name_len); }

    [[nodiscard]] u16_t
    get_id() const noexcept
    { return id; }


  private: friend class SymbolTable;
    Variable(QualifiedType qualified, std::string_view name, bool is_public, sz_t variable_insert_order) noexcept
    : Variable(qualified, name, is_public) {
      assume_assert(variable_insert_order <= u16_max);
      id = static_cast<u16_t>(variable_insert_order);
    }
  };

  struct Function {
    eden::vector16<Variable> locals;
    FunctionType const* type;
    const char* name; u32_t name_len;
    bool is_public;

    explicit Function(std::string_view name, std::span<Variable> parameters, FunctionType const* type, bool is_public)
    : type(type), name(name.data()), name_len(name.length()), is_public(is_public), id(INVALID_ID) {
      assert(parameters.size() == type->numParameters());
      for (auto& var : parameters) locals.emplace_back(var);
    }

    [[nodiscard]] u16_t
    get_id() const noexcept
    { return id; }

    [[nodiscard]] std::span<const Variable>
    parameters() const noexcept
    { return locals.to_span().subspan(0, type->numParameters()); }

    [[nodiscard]] const Type*
    returnType() const noexcept
    { return type->returnType(); }

    [[nodiscard]] std::string_view
    nameof() const noexcept
    { return std::string_view(name, name_len); }

    void addVariable(std::string_view variable_name, QualifiedType variable_instance) noexcept {
      assert(not getVariable(variable_name));
      locals.emplace_back(variable_instance, variable_name, false);
    }

    [[nodiscard]] std::optional<std::pair<QualifiedType, u32_t>>
    getVariable(std::string_view variable_name) const noexcept {
      sz_t n = locals.size();
      while (n-- not_eq 0)
        if (variable_name == locals[n].nameof())
          return std::optional(std::pair(locals[n].type, n));
      return std::nullopt;
    }

  private: friend class SymbolTable; friend class Module;
    u16_t id;
    Function(std::string_view name, std::span<Variable> parameters, FunctionType const* type, bool is_public, sz_t functon_insert_order)
    : Function(name, parameters, type, is_public) {
      assume_assert(functon_insert_order <= INVALID_ID);
      id = static_cast<u16_t>(functon_insert_order);
    }
  };

protected:

  static constexpr auto name_search = [] (auto const& e, std::string_view key) { return e.nameof() == key; };
  static constexpr auto id_search = [] (auto const& e, u16_t id) { return e.id == id; };
  static constexpr auto swapvec_settings = eden::swap_vector_settings<4, true>{};

  // marked mutable because searches need to be possible from a const*, but searches will modify
  mutable eden::swap_vector<Variable, swapvec_settings> variables;
  mutable eden::swap_vector<Function, swapvec_settings> functions;

  // returns nullptr if non-existent.
  template<class T>
  [[nodiscard]] static T*
  search_by_id(auto& targets, u16_t target_id) noexcept {
    if (targets.empty())
      return nullptr;

    if (targets[target_id].id == target_id)
      return &targets[target_id];

    // sorts array for each miss. should only happen at most once (ideally) after table has been sorted
    targets.sort_by_unique_idx([](T const& e) { return e.id; });

    if (targets[target_id].id == target_id)
      return &targets[target_id];
    return nullptr;
  }

public:

  // ensure a variable with the same name does not already exist
  void addVariable(QualifiedType qualified, std::string_view variable_name, bool is_public) noexcept {
    assert(not variables.search_noswap(name_search, variable_name));
    variables.emplace_back( Variable{qualified, variable_name, is_public, variables.size()} );
  }

  // ensure a variable with the same name does not already exist
  void addVariable(Variable const& addition) noexcept {
    assert(not variables.search_noswap(name_search, addition.nameof()));
    auto const id = variables.size();
    variables.emplace_back(addition);
    variables.back().id = id;
  }

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another global is added or searched via name.
  [[nodiscard]] Variable const*
  getVariable(std::string_view variable_name) const noexcept
  { return variables.search(name_search, variable_name); }

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another global is added or searched via name.
  [[nodiscard]] Variable const*
  getVariable(u16_t variable_id) const noexcept
  { return search_by_id<Variable>(variables, variable_id); }

  // returns nullptr if non-existent / not public. pointer is not stable and may be invalidated if another global is added or searched via name.
  [[nodiscard]] Variable const*
  getPublicVariable(std::string_view variable_name) const noexcept {
    auto const variable = variables.search(name_search, variable_name);
    if (variable == nullptr or not variable->is_public) return nullptr;
    return variable;
  }

  [[nodiscard]] auto const&
  getVariableList() const noexcept
  { return variables; }

  void orderVariableList() const noexcept
  { variables.sort_by_unique_idx([](Variable const& v) { return v.get_id(); }); }

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another function is added or searched via name.
  [[nodiscard]] Function const*
  getFunction(std::string_view function_name) const noexcept
  { return functions.search(name_search, function_name); }

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another function is added or searched via name.
  [[nodiscard]] Function const*
  getFunction(u16_t function_id) const noexcept
  { return search_by_id<Function>(functions, function_id); }

  // returns nullptr if non-existent / not public. pointer is not stable and may be invalidated if another function is added or searched via name.
  [[nodiscard]] Function const*
  getPublicFunction(std::string_view function_name) const noexcept {
    auto const function = functions.search(name_search, function_name);
    if (function == nullptr or not function->is_public)
      return nullptr;
    return function;
  }

  // ensure that a function with the same name does not already exist
  void
  addFunction(std::string_view function_name, std::span<Variable> parameters, FunctionType const* function_type, bool is_public) noexcept {
    assert(not functions.search_noswap(name_search, function_name));
    functions.emplace_back( Function{ function_name, parameters, function_type, is_public, static_cast<u16_t>(functions.size()) } );
  }

  [[nodiscard]] auto const&
  getFunctionList() const noexcept
  { return functions; }

};

class Module final : public SymbolTable {
  const char* name; u32_t name_len;

  TypeContext* types;

  [[nodiscard]] Function&
  current_scope() const noexcept
  { assert(not functions.empty()); return functions.back(); }

public:

  explicit Module(std::string_view module_name, TypeContext* type_context) noexcept
  : name(module_name.data()), name_len(module_name.length()), types(type_context) {}

  Module(Module&&) noexcept = default;

  [[nodiscard]] TypeContext const*
  getTypeContext() const noexcept
  { return types; }

  [[nodiscard]] std::string_view
  nameof() const noexcept
  { return std::string_view(name, name_len); }

  [[nodiscard]] Type const*
  getRawPointerType(QualifiedType subtype) const noexcept
  { return types->addRawPointer(subtype); }

  [[nodiscard]] Type const*
  getUniquePointerType(QualifiedType subtype) const noexcept
  { return types->addUniquePointer(subtype); }

  [[nodiscard]] Type const*
  getVariantType(std::span<const Type*> subtypes, bool nullable) const noexcept
  { return types->addVariant(subtypes, nullable); }

  [[nodiscard]] FunctionType const*
  getFunctionType(std::span<Variable> parameters, const Type* return_type, bool is_variadic = false) const noexcept {
    const Type* parameter_types[Settings::MAX_FUNCTION_PARAMETERS];
    auto i{0uz};
    for (; i<parameters.size(); ++i)
      parameter_types[i] = parameters[i].type.type;

    return types->addFunction(std::span(parameter_types, i), return_type, is_variadic);
  }

  CustomType const*
  addCustomType(std::string_view type_name, std::span<const Variable> members) const noexcept {
    auto const custom = types->addCustomType(type_name);
    auto const custom_table = custom->member_table();
    for (auto const& member : members)
      custom_table->addVariable(member);
    return custom;
  }

  // returns nullptr if non-existent
  [[nodiscard]] CustomType const*
  getCustomType(std::string_view type_name) const noexcept
  { return types->getCustomType(type_name); }

  [[nodiscard]] bool
  containsLocal(std::string_view local_name) const noexcept
  { return functions.back().getVariable(local_name).has_value(); }

  // assumes the local does not already exist.
  void addLocal(std::string_view local_name, QualifiedType local_instance) noexcept {
    assert(not current_scope().getVariable(local_name));
    current_scope().addVariable(local_name, local_instance);
  }

  [[nodiscard]] auto
  getLocal(std::string_view local_name) const noexcept
  { return current_scope().getVariable(local_name); }

  // returns false if function already exists
  bool
  addFunction(std::string_view function_name, std::span<Variable> parameters, Type const* return_type, bool is_public, bool is_variadic = false) {
    auto const exists = functions.search(name_search, function_name);
    if (exists) return false;

    auto const function_type = getFunctionType(parameters, return_type, is_variadic);
    functions.emplace_back( Function {function_name, parameters, function_type, is_public, static_cast<u16_t>(functions.size()) } );
    return true;
  }

  FunctionType const*
  enterFunctionScope(std::string_view function_name) noexcept {
    auto const fn = functions.search_swapback(name_search, function_name); assert(fn);
    return fn->type;
  }

};


// Stabilized table and module exist so Variable* and Function* can be used without worry of the data being relocated
// They only allow for id-based search which is ~O(1)
class StabilizedTable {
  SymbolTable const* table;
public:
  StabilizedTable(SymbolTable const* table) noexcept : table(table) {}

  [[nodiscard]] SymbolTable::Variable const*
  getVariable(u16_t variable_id) const noexcept
  { return table->getVariable(variable_id); }

  [[nodiscard]] SymbolTable::Function const*
  getFunction(u16_t function_id) const noexcept
  { return table->getFunction(function_id); }
};

class StabilizedModule {
  Module const* module;
public:
  StabilizedModule(Module const* module) noexcept : module(module) {}

  [[nodiscard]] std::string_view
  nameof() const noexcept
  { return module->nameof(); }

  [[nodiscard]] SymbolTable::Variable const*
  getVariable(u16_t variable_id) const noexcept
  { return module->getVariable(variable_id); }

  [[nodiscard]] SymbolTable::Function const*
  getFunction(u16_t function_id) const noexcept
  { return module->getFunction(function_id); }
};


#include "table_and_module_sync.hpp"
static_assert(sizeof(SymbolTable) == SYMBOL_TABLE_SIZE);
static_assert(alignof(SymbolTable) == SYMBOL_TABLE_ALIGNMENT);
static_assert(sizeof(Module) == MODULE_SIZE);
static_assert(alignof(Module) == MODULE_ALIGNMENT);

}