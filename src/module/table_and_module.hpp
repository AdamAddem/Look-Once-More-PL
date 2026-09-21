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
    TypeID typeID;
    Type::Qualifiers qualifiers;
    bool is_public;
    u16_t id;
    u32_t name_len;
    char const* name;

    Variable(TypeID typeID, Type::Qualifiers qualifiers, std::string_view name, bool is_public, sz_t variable_insert_order) noexcept
    : typeID(typeID), qualifiers(qualifiers), is_public(is_public), id(variable_insert_order), name_len(name.length()), name(name.data())
    { assert(variable_insert_order != u16_max); }

    edenInlineNodiscardCXPR std::string_view nameof() const noexcept { return std::string_view(name, name_len); }
  };

  struct Function {
    friend class SymbolTable; friend class Module;

    mutable eden::swap_vector16<Variable> locals;
    char const* name; u32_t name_len;
    u32_t type_id;
    u16_t type_module_id;
    u16_t id{INVALID_ID};
    bool is_public;
    //byte_t _pad[3];

    constexpr explicit Function(std::string_view name, eden::swap_vector16<Variable>&& parameters, TypeID function_typeID, bool is_public) noexcept
    : locals(std::move(parameters)), name(name.data()), name_len(name.length()), type_id(function_typeID.id), type_module_id(function_typeID.module_id), is_public(is_public) {
      assert(locals.is_ordered(get_id_of));
#ifndef NDEBUG
      for (auto const& param : locals)
        assert(param.id not_eq INVALID_ID);
#endif
    }

    edenInlineNodiscardCXPR TypeID getTypeID() const noexcept { return { .derived = Type::FUNCTION, .module_id = type_module_id, .id = type_id }; }
    edenInlineNodiscardCXPR std::string_view nameof() const noexcept { return {name, name_len}; }
    edenInlineNodiscardCXPR sz_t num_parameters() const noexcept { return locals.size(); }

#define pre assert(locals.search_noswap(name_search, var_name) == nullptr);
    edenInlineCXPR void addLocal(TypeID var_typeID, Type::Qualifiers var_qualifiers, std::string_view var_name) noexcept { pre locals.emplace_back(var_typeID, var_qualifiers, var_name, false, locals.size()); }
#undef pre

    // returns nullptr if non-existent. pointer is not stable and may be invalidated if another local is added or searched.
    edenNodiscardCXPR Variable const* getLocal(std::string_view variable_name) const noexcept { return locals.search(name_search, variable_name); }

    // reference is not stable and may be invalidated if another local is added or searched.
#define pre assert(locals.search_noswap(id_search, local_id));
    edenNodiscardCXPR Variable const& getLocal(u16_t local_id) const noexcept { pre return locals.gradual_sort_search(get_id_of, local_id); }
#undef pre

  private:
    constexpr Function(std::string_view name, eden::swap_vector16<Variable>&& parameters, TypeID function_typeID, bool is_public, sz_t functon_insert_order)
    : Function(name, std::move(parameters), function_typeID, is_public) {
      id = (u16_t) functon_insert_order;
      assert(functon_insert_order not_eq INVALID_ID);
      assert(locals.is_ordered(get_id_of));

#ifndef NDEBUG
      for (auto const& param : locals) assert(param.id not_eq INVALID_ID);
#endif
    }
  };

private:

  // function vector preserves backmost element, which is the function currently being parsed
  mutable eden::swap_vector<Function, eden::swap_vector_settings<4, true>{}> functions;
  mutable eden::swap_vector<Variable> variables;

#define pre assert(variables.empty());
  constexpr void overrideVariables(eden::swap_vector<Variable>&& new_variables) noexcept { variables = std::move(new_variables); }
#undef pre

  edenInlineNodiscardCXPR Function& current_scope() const noexcept { assert(not functions.empty()); return functions.back(); }
public:

#define pre assert(not variables.search_noswap(name_search, var_name));
  edenInlineCXPR void addVariable(TypeID var_typeID, Type::Qualifiers var_qualifiers, std::string_view var_name, bool is_public) noexcept { pre variables.emplace_back( Variable{var_typeID, var_qualifiers, var_name, is_public, variables.size()} ); }
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

  // returns the function id
#define pre assert(not functions.search_noswap(name_search, function_name)); assert(function_typeID.derived == Type::FUNCTION);
  edenInlineCXPR u16_t
  addFunction(std::string_view function_name, eden::swap_vector16<Variable>&& parameters, TypeID function_typeID, bool is_public) noexcept { pre
    auto const function_id = (u16_t) functions.size();
    functions.emplace_back( Function{ function_name, std::move(parameters), function_typeID, is_public, function_id } );
    return function_id;
  }
#undef pre

#define pre assert(functions.search_noswap(id_search, function_id));
  edenInlineCXPR void enterFunctionScope(u16_t function_id) noexcept { pre [[maybe_unused]] auto _ = functions.search_swapback(id_search, function_id); }
#undef pre

#define pre assert(not current_scope().getLocal(local_name));
  edenInlineCXPR void addLocal(TypeID local_typeID, Type::Qualifiers local_qualifiers, std::string_view local_name) noexcept { pre current_scope().addLocal(local_typeID, local_qualifiers, local_name); }
#undef pre

  edenInlineNodiscardCXPR bool containsLocal(std::string_view local_name) const noexcept { return current_scope().getLocal(local_name) not_eq nullptr; }

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another local is added or searched.
  edenInlineNodiscardCXPR Variable const* getLocal(std::string_view local_name) const noexcept { return current_scope().getLocal(local_name); }

};

class Module final : public SymbolTable {
  static constexpr auto search_pred = [] (auto const& type, auto const& other_type) { return type.sameAs(other_type); };
  static constexpr auto named_search_pred = [] (auto const& type, std::string_view name) { return type.nameof() == name; };

  eden::swap_vector<ArrayType> array_types; // using a swap vector here is bad, we should only ever use search_noswap. TODO: Change
  eden::swap_vector<FunctionType> function_types;
  eden::swap_vector<CustomType> custom_types;
  char const* name{};
  u32_t name_len{};
  u16_t id;
  //byte_t _pad[2];

  template <std::derived_from<Type> T>
  edenInlineNodiscardCXPR static TypeID
  getExisting(T const* type_in_vector, eden::swap_vector<T>& owning_vector, u16_t module_id) noexcept {
    return {.derived = Type::corresponding_derived_type<T>(), .module_id = module_id, .id = (u16_t)owning_vector.index_in(type_in_vector)};
  }

  template <std::derived_from<Type> T>
  edenInlineNodiscardCXPR static TypeID
  makeNew(eden::swap_vector<T>& type_vector, u16_t module_id, T&& type) noexcept {
    auto const fn_type_id = (u16_t) type_vector.size();
    type_vector.emplace_back(std::move(type));
    return {.derived = Type::corresponding_derived_type<T>(), .module_id = module_id, .id = fn_type_id};
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
    array_types.reserve(8);
    function_types.reserve(8);
    custom_types.reserve(2);
  }
  constexpr Module(Module&&) noexcept = default;

  void set_name(std::string_view module_name) noexcept { name = module_name.data(); name_len = module_name.size(); }

  edenInlineNodiscardCXPR sz_t
  totalNumberOfTypes() const noexcept {
    return PrimitiveType::num_types +
           array_types.size() +
           function_types.size() +
           custom_types.size();
  }

  edenInlineNodiscardCXPR std::string_view nameof() const noexcept { return std::string_view{name, name_len}; }
  edenInlineNodiscardCXPR u16_t getID()             const noexcept { return id; }

  edenInlineNodiscardCXPR sz_t numArrayTypes()    const noexcept { return array_types.size(); }
  edenInlineNodiscardCXPR sz_t numFunctionTypes() const noexcept { return function_types.size(); }
  edenInlineNodiscardCXPR sz_t numCustomTypes()   const noexcept { return custom_types.size(); }

  edenInlineNodiscardCXPR TypeID addArrayType(TypeID subtypeID, u64_t array_size) noexcept { return returnExistingOrNew(array_types, id, subtypeID, array_size); }
  edenInlineNodiscardCXPR TypeID addFunctionType(std::span<TypeID const> parameter_typeIDs, TypeID returnTypeID, bool is_variadic = false) noexcept { return returnExistingOrNew(function_types, id, parameter_typeIDs, returnTypeID, is_variadic); }
  edenInlineNodiscardCXPR TypeID addFunctionType(std::span<Variable const> parameters, TypeID returnTypeID, bool is_variadic) noexcept {
    auto const num_parameters = parameters.size(); assert(num_parameters <= Settings::MAX_FUNCTION_PARAMETERS);

    TypeID parameter_typeIDs[Settings::MAX_FUNCTION_PARAMETERS];
    for (auto i{0uz}; i<num_parameters; ++i)
      parameter_typeIDs[i] = parameters[i].typeID;

    return addFunctionType({parameter_typeIDs, num_parameters}, returnTypeID, is_variadic);
  }

#define pre assert(getCustomTypeID(type_name) == devoidID);
  edenInlineNodiscardCXPR TypeID
  addCustomType(std::string_view type_name, eden::swap_vector<Variable>&& members) noexcept { pre
    auto const customID = makeNew(custom_types, id, CustomType{type_name});
    auto& custom = custom_types[customID.id];
    custom.member_table().overrideVariables(std::move(members));
    return customID;
  }
#undef pre

#define pre assert(arrayTypeID.module_id == id); assert(arrayTypeID.derived == Type::ARRAY); assert(not arrayTypeID.isPointer());
  edenInlineNodiscardCXPR ArrayType const& getArrayType(TypeID arrayTypeID) const noexcept { pre return array_types[arrayTypeID.id]; }
#undef pre

#define pre assert(functionTypeID.module_id == id); assert(functionTypeID.derived == Type::FUNCTION); assert(not functionTypeID.isPointer());
  edenInlineNodiscardCXPR FunctionType const& getFunctionType(TypeID functionTypeID) const noexcept { pre return function_types[functionTypeID.id]; }
#undef pre

#define pre assert(customTypeID.module_id == id); assert(customTypeID.derived == Type::CUSTOM); assert(not customTypeID.isPointer());
  edenInlineNodiscardCXPR CustomType const& getCustomType(TypeID customTypeID) const noexcept { pre return custom_types[customTypeID.id]; }
#undef pre

  // returns devoidID if not found
  edenInlineNodiscardCXPR TypeID
  getCustomTypeID(std::string_view name) noexcept {
    auto const res = custom_types.search_noswap(named_search_pred, name); 
    if(res) return getExisting(res, custom_types, id);
    return TypeID { .derived = Type::DEVOID };
  }

  // if typeID is not primitive, devoid, or error, then it must be from this module
  /*
  edenNodiscardCXPR Type const&
  getTypeFromID(TypeID typeID) const noexcept {
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
  } */
};

// Stabilized table and module exist so Variable* and Function* can be used without worry of the data being relocated
// They only allow for id-based search which is ~O(1)
class StabilizedTable {
  SymbolTable const* table{};
public:
  edenInlineCXPR void set(SymbolTable const* to_stabilize) noexcept { table = to_stabilize; }
  edenInlineNodiscardCXPR SymbolTable::Variable const& getVariable(u16_t variable_id) const noexcept { return table->getVariable(variable_id); }
  edenInlineNodiscardCXPR SymbolTable::Function const& getFunction(u16_t function_id) const noexcept { return table->getFunction(function_id); }
};

class StabilizedModule {
  Module const* module{};
public:
  edenInlineCXPR void set(Module const* to_stabilize) noexcept { module = to_stabilize; }
  edenInlineNodiscardCXPR SymbolTable::Variable const& getVariable(u16_t variable_id) const noexcept { return module->getVariable(variable_id); }
  edenInlineNodiscardCXPR SymbolTable::Function const& getFunction(u16_t function_id) const noexcept { return module->getFunction(function_id); }
  edenInlineNodiscardCXPR u32_t getID() const noexcept { return module->getID(); }
};

#include "table_and_module_sync.hpp"
static_assert(sizeof(SymbolTable) == SYMBOL_TABLE_SIZE);
static_assert(alignof(SymbolTable) == SYMBOL_TABLE_ALIGNMENT);
static_assert(sizeof(Module) == MODULE_SIZE);
static_assert(alignof(Module) == MODULE_ALIGNMENT);

}