#pragma once
#include "edenlib/macros.hpp"
#include "edenlib/typedefs.hpp"
#include "edenlib/vectors/swap_vector.hpp"

#include "types.hpp"

namespace LOM {

class SymbolTable {
  friend class Module;
  static constexpr auto name_search = [] (auto const& e, std::string_view key) static { return e.nameof() == key; };
  static constexpr auto id_search = [] (auto const& e, u16_t id) static { return e.id == id; };
  static constexpr auto get_id_of = [] (auto const& e) static { return e.id; };


public:
  static constexpr u16_t INVALID_ID = u16_max;
  struct Variable {
    QualifiedTypeID type;
    bool is_public;
    u16_t id;
    u32_t name_len;
    char const* name;

    Variable(QualifiedTypeID qualified, std::string_view name, bool is_public, sz_t variable_insert_order) noexcept
    : type(qualified), is_public(is_public), id(variable_insert_order), name_len(name.length()), name(name.data())
    { assert(variable_insert_order <= u16_max); }

    edenAlwaysInline [[nodiscard]] std::string_view nameof() const noexcept { return std::string_view(name, name_len); }
  };

  struct Function {
    friend class SymbolTable; friend class Module;

    mutable eden::swap_vector16<Variable> locals;
    char const* name; u32_t name_len;
    u32_t function_type_id;
    bool is_public;
    // byte_t _pad[5];
    u16_t id;

    constexpr explicit Function(std::string_view name, eden::swap_vector16<Variable>&& parameters, u32_t function_type_id, bool is_public) noexcept
    : locals(std::move(parameters)), name(name.data()), name_len(name.length()), function_type_id(function_type_id), is_public(is_public), id(INVALID_ID) {
      assert(locals.is_ordered(get_id_of));
#ifndef NDEBUG
      for (auto const& param : locals)
        assert(param.id not_eq INVALID_ID);
#endif
    }

    edenInlineNodiscardCXPR TypeID returnType(u16_t module_id) const noexcept { return { .derived = Type::FUNCTION, .module_id = module_id, .id = function_type_id }; }
    edenInlineNodiscardCXPR std::string_view nameof() const noexcept { return {name, name_len}; }
    edenInlineNodiscardCXPR sz_t num_parameters() const noexcept { return locals.size(); }

#define pre assert(locals.search_noswap(name_search, variable_name) == nullptr);
    constexpr void addLocal(std::string_view variable_name, QualifiedTypeID variable_instance) noexcept { pre locals.emplace_back(variable_instance, variable_name, false, locals.size()); }
#undef pre

    // returns nullptr if non-existent. pointer is not stable and may be invalidated if another local is added or searched.
    edenNodiscardCXPR Variable const* getLocal(std::string_view variable_name) const noexcept { return locals.search(name_search, variable_name); }

    // reference is not stable and may be invalidated if another local is added or searched.
#define pre assert(locals.search_noswap(id_search, local_id));
    edenNodiscardCXPR Variable const& getLocal(u16_t local_id) const noexcept { pre return locals.gradual_sort_search(get_id_of, local_id); }
#undef pre

  private:
    constexpr Function(std::string_view name, eden::swap_vector16<Variable>&& parameters, u32_t function_type_id, bool is_public, sz_t functon_insert_order)
    : Function(name, std::move(parameters), function_type_id, is_public) {
      assert(functon_insert_order < INVALID_ID);
      assert(locals.is_ordered(get_id_of));

#ifndef NDEBUG
      for (auto const& param : locals) assert(param.id not_eq INVALID_ID);
#endif
      id = (u16_t) functon_insert_order;
    }
  };

private:
  mutable eden::swap_vector<Variable> variables;

#define pre assert(variables.empty());
  constexpr void overrideVariables(eden::swap_vector<Variable>&& new_variables) noexcept { variables = std::move(new_variables); }
#undef pre

  // function vector preserves backmost element, which is the function currently being parsed
  mutable eden::swap_vector<Function, eden::swap_vector_settings<4, true>{}> functions;
  edenInlineNodiscardCXPR Function& current_scope() const noexcept { assert(not functions.empty()); return functions.back(); }
public:

#define pre assert(not variables.search_noswap(name_search, variable_name));
  constexpr void addVariable(QualifiedTypeID qualified, std::string_view variable_name, bool is_public) noexcept { pre variables.emplace_back( Variable{qualified, variable_name, is_public, variables.size()} ); }
#undef pre

#define pre assert(not variables.search_noswap(name_search, addition.nameof()));
  constexpr void addVariable(Variable const& addition) noexcept { pre variables.emplace_back(addition); variables.back().id = variables.size() - 1; }
#undef pre

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another global is added or searched.
  edenInlineNodiscardCXPR Variable const* getVariable(std::string_view variable_name) const noexcept { return variables.search(name_search, variable_name); }

  // Reference is stable unless another variable is added or searched not using ID.
#define pre assert(variables.search_noswap(id_search, variable_id));
  edenNodiscardCXPR Variable const& getVariable(u16_t variable_id) const noexcept { pre  return variables.gradual_sort_search(get_id_of, variable_id); }
#undef pre

  // returns nullptr if non-existent / not public. pointer is not stable and may be invalidated if another global is added or searched.
  edenNodiscardCXPR Variable const*
  getPublicVariable(std::string_view variable_name) const noexcept {
    auto const variable = variables.search(name_search, variable_name);
    if (variable == nullptr or not variable->is_public) return nullptr;
    return variable;
  }

  edenInlineNodiscardCXPR sz_t num_variables() const noexcept { return variables.size(); }

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another function is added or searched.
  edenInlineNodiscardCXPR Function const* getFunction(std::string_view function_name) const noexcept { return functions.search(name_search, function_name); }

  // Reference is stable unless another function is added or searched not using ID.
#define pre assert(functions.search_noswap(id_search, function_id));
  edenNodiscardCXPR Function const& getFunction(u16_t function_id) const noexcept { pre return functions.gradual_sort_search(get_id_of, function_id); }
#undef pre

  // returns nullptr if non-existent / not public. pointer is not stable and may be invalidated if another function is added or searched.
  edenNodiscardCXPR Function const*
  getPublicFunction(std::string_view function_name) const noexcept {
    auto const function = functions.search(name_search, function_name);
    if (function == nullptr or not function->is_public) return nullptr;
    return function;
  }

#define pre assert(not functions.search_noswap(name_search, function_name)); assert(function_typeID.derived == Type::FUNCTION);
  edenInlineCXPR void
  addFunction(std::string_view function_name, eden::swap_vector16<Variable>&& parameters, TypeID function_typeID, bool is_public) noexcept { pre
    functions.emplace_back( Function{ function_name, std::move(parameters), function_typeID.id, is_public, static_cast<u16_t>(functions.size()) } );
  }
#undef pre

#define pre assert(functions.search_noswap(name_search, function_name));
  edenInlineCXPR void enterFunctionScope(std::string_view function_name) noexcept { pre [[maybe_unused]] auto _ = functions.search_swapback(name_search, function_name); }
#undef pre

#define pre assert(not current_scope().getLocal(local_name));
  edenInlineCXPR void addLocal(std::string_view local_name, QualifiedTypeID local_instance) noexcept { pre current_scope().addLocal(local_name, local_instance); }
#undef pre

  edenInlineNodiscardCXPR bool containsLocal(std::string_view local_name) const noexcept { return current_scope().getLocal(local_name) not_eq nullptr; }

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another local is added or searched.
  edenInlineNodiscardCXPR Variable const* getLocal(std::string_view local_name) const noexcept { return current_scope().getLocal(local_name); }

};

class Module final : public SymbolTable {
  static constexpr auto search_pred = [] (auto const& type, auto const& other_type) { return type == other_type; };
  static constexpr auto named_search_pred = [] (auto const& type, std::string_view name) { return type.nameof() == name; };

  eden::swap_vector<PointerType> pointer_types; // using a swap vector here is bad, we should only ever use search_noswap. TODO: Change
  eden::swap_vector<ArrayType> array_types;
  eden::swap_vector<FunctionType> function_types;
  eden::swap_vector<CustomType> custom_types;
  //byte_t _pad[6];
  u16_t id;

  template <std::derived_from<Type> T>
  edenInlineNodiscardCXPR static TypeID
  getExisting(T* type_in_vector, eden::swap_vector<T>& owning_vector, u16_t module_id) noexcept {
    return {.derived = Type::corresponding_derived_type<T>(), .module_id = module_id, .id = (u16_t)owning_vector.index_in(type_in_vector)};
  }

  template <std::derived_from<Type> T>
  edenInlineNodiscardCXPR static TypeID
  makeNew(eden::swap_vector<T>& type_vector, u16_t module_id, T&& type) noexcept {
    type_vector.emplace_back(std::move(type));
    return {.derived = Type::corresponding_derived_type<T>(), .module_id = module_id, .id = (u16_t)(type_vector.size() - 1)};
  }

  template <std::derived_from<Type> T>
  edenInlineNodiscardCXPR static TypeID
  returnExistingOrNew(eden::swap_vector<T>& type_vector, u16_t module_id, auto... args) noexcept {
    T tmp(args...);
    auto const res = type_vector.search_noswap(search_pred, tmp);
    if (res) return getExisting(res, type_vector, module_id);
    return makeNew(type_vector, module_id, std::move(tmp));
  }

public:
  constexpr explicit Module(u16_t module_id) noexcept : id(module_id) {
    pointer_types.reserve(8);
    array_types.reserve(8);
    function_types.reserve(8);
    custom_types.reserve(2);
  }
  constexpr Module(Module&&) noexcept = default;

  edenInlineNodiscardCXPR sz_t
  totalNumberOfTypes() const noexcept {
    return PrimitiveType::num_types +
           pointer_types.size() +
           array_types.size() +
           function_types.size() +
           custom_types.size();
  }

  edenInlineNodiscardCXPR u16_t getID()           const noexcept { return id; }
  edenInlineNodiscardCXPR sz_t numArrayTypes()    const noexcept { return array_types.size(); }
  edenInlineNodiscardCXPR sz_t numFunctionTypes() const noexcept { return function_types.size(); }
  edenInlineNodiscardCXPR sz_t numCustomTypes()   const noexcept { return custom_types.size(); }

#define pre assert(subtypeID.module_id == id);
  edenInlineNodiscardCXPR TypeID getPointerType(TypeID subtypeID, bool is_raw)    noexcept { pre return returnExistingOrNew(pointer_types, id, subtypeID, is_raw); }
  edenInlineNodiscardCXPR TypeID getRawPointerType(TypeID subtypeID)              noexcept { pre return getPointerType(subtypeID, true); }
  edenInlineNodiscardCXPR TypeID getRefPointerType(TypeID subtypeID)              noexcept { pre return getPointerType(subtypeID, false); }
  edenInlineNodiscardCXPR TypeID getArrayType(TypeID subtypeID, u64_t array_size) noexcept { pre return returnExistingOrNew(array_types, id, subtypeID, array_size); }
#undef pre

#define pre assert(returnTypeID.module_id == id);
  edenInlineNodiscardCXPR TypeID getFunctionType(std::span<Type::DerivedType const> parameter_derived_types, std::span<u32_t const> parameter_type_ids, TypeID returnTypeID, bool is_variadic = false) noexcept { pre return returnExistingOrNew(function_types, id, parameter_derived_types, parameter_type_ids, returnTypeID, is_variadic); }

  edenInlineNodiscardCXPR TypeID getFunctionType(std::span<Variable const> parameters, TypeID returnTypeID, bool is_variadic) noexcept { pre
    auto const num_parameters = parameters.size(); assert(num_parameters <= Settings::MAX_FUNCTION_PARAMETERS);

    Type::DerivedType parameter_derived_types[Settings::MAX_FUNCTION_PARAMETERS];
    u32_t parameter_type_ids[Settings::MAX_FUNCTION_PARAMETERS];

    for (auto i{0uz}; i<num_parameters; ++i) {
      auto const typeID = parameters[i].type; assert(typeID.module_id == id);
      parameter_derived_types[i] = typeID.derived;
      parameter_type_ids[i] = typeID.id;
    }

    return getFunctionType({parameter_derived_types, num_parameters}, {parameter_type_ids, num_parameters}, returnTypeID, is_variadic);
  }
#undef pre

#define pre assert(custom_types.search_noswap(search_pred, CustomType{type_name}) == nullptr);
  edenInlineNodiscardCXPR TypeID
  addCustomType(std::string_view type_name, eden::swap_vector<Variable>&& members) noexcept { pre
    auto const customID = makeNew(custom_types, id, CustomType{type_name});
    auto& custom = custom_types[customID.id];
    custom.member_table()->overrideVariables(std::move(members));
    return customID;
  }
#undef pre

  // returns devoid TypeID if not found
  edenInlineNodiscardCXPR TypeID
  getCustomType(std::string_view name) noexcept { 
    auto const res = custom_types.search_noswap(named_search_pred, name); 
    if(res) return getExisting(res, custom_types, id);
    return devoid_literal.toTypeID();
  }

  // if typeID is not primitive, devoid, or error, then it must be from this module
  edenNodiscardCXPR Type const& getTypeFromID(TypeID typeID) const noexcept {
    switch (typeID.derived) {
    case Type::PRIMITIVE: return PrimitiveType::make_arr()[typeID.id];
    case Type::DEVOID:    return Type::devoid();
    case Type::ERROR:     return Type::error();

    case Type::POINTER:   assert(typeID.module_id == id); return pointer_types[typeID.id];
    case Type::ARRAY:     assert(typeID.module_id == id); return array_types[typeID.id];
    case Type::FUNCTION:  assert(typeID.module_id == id); return function_types[typeID.id];
    case Type::CUSTOM:    assert(typeID.module_id == id); return custom_types[typeID.id];
    default: edenUnreachable("Invalid derived type.");
    }
  }
};

// Stabilized table and module exist so Variable* and Function* can be used without worry of the data being relocated
// They only allow for id-based search which is ~O(1)
class StabilizedTable {
  SymbolTable const* table;
public:
  edenInlineCXPR explicit StabilizedTable(SymbolTable const* table) noexcept : table(table) {}
  edenInlineNodiscardCXPR SymbolTable::Variable const& getVariable(u16_t variable_id) const noexcept { return table->getVariable(variable_id); }
  edenInlineNodiscardCXPR SymbolTable::Function const& getFunction(u16_t function_id) const noexcept { return table->getFunction(function_id); }
};

#include "table_and_module_sync.hpp"
static_assert(sizeof(SymbolTable) == SYMBOL_TABLE_SIZE);
static_assert(alignof(SymbolTable) == SYMBOL_TABLE_ALIGNMENT);
static_assert(sizeof(Module) == MODULE_SIZE);
static_assert(alignof(Module) == MODULE_ALIGNMENT);

}