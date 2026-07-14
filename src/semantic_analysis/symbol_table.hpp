#pragma once
#include "edenlib/macros.hpp"
#include "edenlib/typedefs.hpp"
#include "edenlib/vectors/vector.hpp"
#include "table_and_module_sync.hpp"
#include "types.hpp"

#include <optional>

namespace LOM {


class SymbolTable {
  friend class Module;
  static constexpr auto name_search = [] (auto const& e, std::string_view key) static { return e.nameof() == key; };
  static constexpr auto id_search = [] (auto const& e, u16_t id) static { return e.id == id; };
  static constexpr auto get_id_of = [] (auto const& e) static { return e.id; };

public:
  static constexpr u16_t INVALID_ID = u16_max;
  struct Variable {
    // members are organized in this stupid way to shave 8 bytes
    [[no_unique_address]] QualifiedType type;
    bool is_public;
    u16_t id;
    u32_t name_len;
    char const* name;

    Variable(QualifiedType qualified, std::string_view name, bool is_public, sz_t variable_insert_order) noexcept
    : type(qualified), is_public(is_public), id(variable_insert_order), name_len(name.length()), name(name.data())
    { assume_assert(variable_insert_order <= u16_max); }

    eden_always_inline [[nodiscard]] std::string_view nameof() const noexcept { return std::string_view(name, name_len); }
  };

  struct Function {
    mutable eden::swap_vector16<Variable> locals;
    FunctionType const* type;
    char const* name; u32_t name_len;
    bool is_public;
    mutable bool locals_sorted{true};
    u16_t id;

    explicit constexpr Function(std::string_view name, eden::swap_vector16<Variable>&& parameters, FunctionType const* type, bool is_public)
    : locals(std::move(parameters)), type(type), name(name.data()), name_len(name.length()), is_public(is_public), id(INVALID_ID) {
      assert(locals.size() == type->numParameters());
      assert(locals.is_ordered(get_id_of));
      for (auto const& param : locals)
        assume_assert(param.id not_eq INVALID_ID);
    }

    eden_always_inline [[nodiscard]] Type const* returnType() const noexcept { return type->returnType(); }
    eden_always_inline [[nodiscard]] std::string_view nameof() const noexcept { return {name, name_len}; }

    [[nodiscard]] std::span<Variable const> parameters() const noexcept {
      if (not locals_sorted) {
        locals.sort_by_unique_idx(get_id_of);
        locals_sorted = true;
      }
      return locals.to_span().subspan(0, type->numParameters());
    }

    void addLocal(std::string_view variable_name, QualifiedType variable_instance) noexcept {
      assert(locals.search_noswap(name_search, variable_name) == nullptr);
      locals.emplace_back(variable_instance, variable_name, false, locals.size());
    }

    // returns nullptr if non-existent. pointer is not stable and may be invalidated if another local is added or searched.
    [[nodiscard]] Variable const*
    getLocal(std::string_view variable_name) const noexcept {
      locals_sorted = false;
      return locals.search(name_search, variable_name);
    }

  private: friend class SymbolTable; friend class Module;
    Function(std::string_view name, eden::swap_vector16<Variable>&& parameters, FunctionType const* type, bool is_public, sz_t functon_insert_order)
    : Function(name, std::move(parameters), type, is_public) {
      assume_assert(functon_insert_order < INVALID_ID);
      assert(locals.size() == type->numParameters());
      assert(locals.is_ordered(get_id_of));
      for (auto const& param : locals)
        assume_assert(param.id not_eq INVALID_ID);
      id = static_cast<u16_t>(functon_insert_order);
    }
  };

private:
  mutable eden::swap_vector<Variable> variables;
  void overrideVariables(eden::swap_vector<Variable>&& new_variables) noexcept {
    assert(variables.empty());
    variables = std::move(new_variables);
  }

  // function vector preserves backmost element, which is the function currently being parsed
  mutable eden::swap_vector<Function, eden::swap_vector_settings<4, true>{}> functions;

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

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another global is added or searched.
  eden_always_inline [[nodiscard]] Variable const* getVariable(std::string_view variable_name) const noexcept { return variables.search(name_search, variable_name); }

  // returns pointer to variable. Pointer is stable unless another variable is added or searched not using ID.
  // Does NOT return nullptr. Variable with matching ID MUST exist.
  [[nodiscard]] Variable const*
  getVariable(u16_t variable_id) const noexcept {
    assert(variables.search_noswap(id_search, variable_id));
    return variables.gradual_sort_search(get_id_of, variable_id);
  }

  // returns nullptr if non-existent / not public. pointer is not stable and may be invalidated if another global is added or searched.
  [[nodiscard]] Variable const*
  getPublicVariable(std::string_view variable_name) const noexcept {
    auto const variable = variables.search(name_search, variable_name);
    if (variable == nullptr or not variable->is_public) return nullptr;
    return variable;
  }

  eden_always_inline [[nodiscard]] auto const& getVariableList() const noexcept { return variables; }
  eden_always_inline void orderVariableList() const noexcept { variables.sort_by_unique_idx(get_id_of); }

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another function is added or searched.
  eden_always_inline [[nodiscard]] Function const* getFunction(std::string_view function_name) const noexcept { return functions.search(name_search, function_name); }

  // returns pointer to function. Pointer is stable unless another function is added or searched not using ID.
  // Does NOT return nullptr. Function with matching ID MUST exist.
  [[nodiscard]] Function const*
  getFunction(u16_t function_id) const noexcept {
    assert(functions.search_noswap(id_search, function_id));
    return functions.gradual_sort_search(get_id_of, function_id);
  }

  // returns nullptr if non-existent / not public. pointer is not stable and may be invalidated if another function is added or searched.
  [[nodiscard]] Function const*
  getPublicFunction(std::string_view function_name) const noexcept {
    auto const function = functions.search(name_search, function_name);
    if (function == nullptr or not function->is_public)
      return nullptr;
    return function;
  }

  // ensure that a function with the same name does not already exist
  void addFunction(std::string_view function_name, eden::swap_vector16<Variable>&& parameters, FunctionType const* function_type, bool is_public) noexcept {
    assert(not functions.search_noswap(name_search, function_name));
    functions.emplace_back( Function{ function_name, std::move(parameters), function_type, is_public, static_cast<u16_t>(functions.size()) } );
  }

};

class Module final : public SymbolTable {
  TypeContext* types;
  eden_always_inline [[nodiscard]] Function& current_scope() const noexcept { assert(not functions.empty()); return functions.back(); }

public:

  eden_always_inline explicit Module(TypeContext* type_context) noexcept : types(type_context) {}
  Module(Module&&) noexcept = default;

  eden_always_inline [[nodiscard]] TypeContext const* getTypeContext() const noexcept { return types; }
  eden_always_inline [[nodiscard]] PointerType const* getRawPointerType(Type const* subtype) const noexcept { return types->addRawPointer(subtype); }
  eden_always_inline [[nodiscard]] PointerType const* getRefPointerType(Type const* subtype) const noexcept { return types->addRefPointer(subtype); }
  eden_always_inline [[nodiscard]] ArrayType   const* getArrayType(u64_t array_size, Type const* subtype) const noexcept { return types->addArray(array_size, subtype); }
  eden_always_inline [[nodiscard]] VariantType const* getVariantType(std::span<Type const*> subtypes, bool nullable) const noexcept { return types->addVariant(subtypes, nullable); }

  [[nodiscard]] FunctionType const*
  getFunctionType(std::span<Variable> parameters, Type const* return_type, bool is_variadic = false) const noexcept {
    const Type* parameter_types[Settings::MAX_FUNCTION_PARAMETERS];
    auto i{0uz};
    for (; i<parameters.size(); ++i)
      parameter_types[i] = parameters[i].type.type;

    return types->addFunction(std::span(parameter_types, i), return_type, is_variadic);
  }

  CustomType const*
  addCustomType(std::string_view type_name, eden::swap_vector<Variable>&& members) const noexcept {
    auto const custom = types->addCustomType(type_name);
    custom->member_table()->overrideVariables(std::move(members));
    return custom;
  }

  // returns nullptr if non-existent
  eden_always_inline [[nodiscard]] CustomType const* getCustomType(std::string_view type_name) const noexcept { return types->getCustomType(type_name); }

  eden_always_inline [[nodiscard]] bool containsLocal(std::string_view local_name) const noexcept { return current_scope().getLocal(local_name) not_eq nullptr; }

  // assumes the local does not already exist.
  void addLocal(std::string_view local_name, QualifiedType local_instance) noexcept {
    assert(not current_scope().getLocal(local_name));
    current_scope().addLocal(local_name, local_instance);
  }

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another local is added or searched.
  eden_always_inline [[nodiscard]] Variable const* getLocal(std::string_view local_name) const noexcept { return current_scope().getLocal(local_name); }

  // returns false if function already exists
  bool addFunction(std::string_view function_name, eden::swap_vector16<Variable>&& parameters, Type const* return_type, bool is_public, bool is_variadic = false) {
    auto const exists = functions.search(name_search, function_name);
    if (exists) return false;

    auto const function_type = getFunctionType(parameters, return_type, is_variadic);
    functions.emplace_back( Function {function_name, std::move(parameters), function_type, is_public, static_cast<u16_t>(functions.size()) } );
    return true;
  }

  // assumes function with name already exists
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