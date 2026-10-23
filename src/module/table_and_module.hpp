#pragma once
#include "edenlib/macros.hpp"
#include "edenlib/typedefs.hpp"
#include "edenlib/vectors/swap_vector.hpp"

#include "edenlib/casts.hpp"
#include "types.hpp"

namespace LOM {

namespace Symbol {

static constexpr u16_t INVALID_ID = u16_max;
static constexpr auto name_search = [] (auto const& e, std::string_view key) static { return e.nameof() == key; };
static constexpr auto id_search = [] (auto const& e, u16_t id) static { return e.id == id; };
static constexpr auto get_id_of = [] (auto const& e) static { return e.id; };

struct Variable {
  TypeID typeID;
  char const* name;
  u32_t name_len;
  u16_t id;
  Type::Qualifiers qualifiers;
  bool is_public;

  constexpr Variable(TypeID typeID, Type::Qualifiers qualifiers, std::string_view name, bool is_public, sz_t variable_insert_order) noexcept
  : typeID(typeID), name(name.data()), name_len(name.length()), id(variable_insert_order), qualifiers(qualifiers), is_public(is_public)
  { assert(variable_insert_order <= u16_max); }

  InlineNodiscardCXPR std::string_view nameof() const noexcept { return std::string_view(name, name_len); }
};

struct Function {
  friend class LOM::SymbolTable; friend class LOM::Module;

  TypeID return_typeID;
  mutable eden::swap_vector16<Variable> locals;
  char const* name;
  u32_t name_len;
  u16_t id{INVALID_ID};
  bool is_public : 1;
  bool is_variadic : 1;
  u8_t num_parameters : 6;

  InlineNodiscardCXPR std::string_view nameof() const noexcept { return {name, name_len}; }

#define pre Assume(locals.search_noswap(name_search, var_name) == nullptr);
  InlineCXPR void addLocal(TypeID var_typeID, Type::Qualifiers var_qualifiers, std::string_view var_name) noexcept { pre locals.emplace_back(var_typeID, var_qualifiers, var_name, false, locals.size()); }
#undef pre

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another local is added or searched.
  NodiscardCXPR Variable const* getLocal(std::string_view variable_name) const noexcept { return locals.search(name_search, variable_name); }

  // reference is not stable and may be invalidated if another local is added or searched.
#define pre assert(locals.search_noswap(id_search, local_id));
  NodiscardCXPR Variable const& getLocal(u16_t local_id) const noexcept { pre return locals.gradual_sort_search(get_id_of, local_id); }
#undef pre

  InlineNodiscardCXPR sz_t getNumParameters() const noexcept { return num_parameters; }

private:
  constexpr Function(std::string_view name, eden::swap_vector16<Variable>&& parameters, TypeID return_typeID, bool is_public, bool is_variadic, sz_t functon_insert_order)
  : return_typeID(return_typeID), locals(std::move(parameters)),
    name(name.data()), name_len(name.length()),
    id(functon_insert_order), is_public(is_public), is_variadic(is_variadic) {
    num_parameters = eden::narrow<u8_t>(locals.size());
    Assume(functon_insert_order not_eq INVALID_ID);
    assert(locals.is_ordered(get_id_of));

#ifndef NDEBUG
    (void) eden::narrow<u16_t>(functon_insert_order);
    for (auto const& param : locals) assert(param.id not_eq INVALID_ID);
#endif
  }
};

}

class SymbolTable {
  friend class Module;
  using Function = Symbol::Function;
  using Variable = Symbol::Variable;

  static constexpr auto name_search = Symbol::name_search;
  static constexpr auto id_search = Symbol::id_search;
  static constexpr auto get_id_of = Symbol::get_id_of;

  // function vector preserves backmost element, whi  alignas(SYMBOL_TABLE_ALIGNMENT)
  mutable eden::swap_vector<Function, eden::swap_vector_settings<4, true>{}> functions;
  mutable eden::swap_vector<Variable> variables;

  InlineNodiscardCXPR Function& current_scope() const noexcept { assert(not functions.empty()); return functions.back(); }
public:

#define pre assert(not variables.search_noswap(name_search, var_name));
  InlineCXPR void addVariable(TypeID var_typeID, Type::Qualifiers var_qualifiers, std::string_view var_name, bool is_public) noexcept { pre variables.emplace_back( Variable{var_typeID, var_qualifiers, var_name, is_public, variables.size()} ); }
#undef pre

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another global is added or searched.
  InlineNodiscardCXPR Variable const* getVariable(std::string_view variable_name) const noexcept { return variables.search(name_search, variable_name); }

  // Reference is stable unless another variable is added or searched not using ID.
#define pre assert(variables.search_noswap(id_search, variable_id));
  NodiscardCXPR Variable const& getVariable(u16_t variable_id) const noexcept { pre  return variables.gradual_sort_search(get_id_of, variable_id); }
#undef pre

  // returns nullptr if non-existent / not public. pointer is not stable and may be invalidated if another global is added or searched.
  NodiscardCXPR Variable const*
  getPublicVariable(std::string_view variable_name) const noexcept {
    auto const variable = variables.search(name_search, variable_name);
    if (variable == nullptr or not variable->is_public) return nullptr;
    return variable;
  }

  InlineNodiscardCXPR sz_t num_variables() const noexcept { return variables.size(); }

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another function is added or searched.
  InlineNodiscardCXPR Function const* getFunction(std::string_view function_name) const noexcept { return functions.search(name_search, function_name); }

  // Reference is stable unless another function is added or searched not using ID.
#define pre assert(functions.search_noswap(id_search, function_id));
  NodiscardCXPR Function const& getFunction(u16_t function_id) const noexcept { pre return functions.gradual_sort_search(get_id_of, function_id); }
#undef pre

  NodiscardCXPR std::span<Function const> getFunctions() const noexcept { return functions.to_span(); }

  // returns nullptr if non-existent / not public. pointer is not stable and may be invalidated if another function is added or searched.
  NodiscardCXPR Function const*
  getPublicFunction(std::string_view function_name) const noexcept {
    auto const function = functions.search(name_search, function_name);
    if (function == nullptr or not function->is_public) return nullptr;
    return function;
  }

  // returns the function id
#define pre assert(not functions.search_noswap(name_search, function_name));
  InlineCXPR u16_t
  addFunction(std::string_view function_name, eden::swap_vector16<Variable>&& parameters, TypeID return_typeID, bool is_public, bool is_variadic = false) noexcept { pre
    auto const function_id = (u16_t) functions.size();
    functions.emplace_back( Function{ function_name, std::move(parameters), return_typeID, is_public, is_variadic, function_id } );
    return function_id;
  }
#undef pre

#define pre assert(functions.search_noswap(id_search, function_id));
  InlineCXPR void enterFunctionScope(u16_t function_id) noexcept { pre [[maybe_unused]] auto _ = functions.search_swapback(id_search, function_id); }
#undef pre

#define pre assert(not current_scope().getLocal(local_name));
  InlineCXPR void addLocal(TypeID local_typeID, Type::Qualifiers local_qualifiers, std::string_view local_name) noexcept { pre current_scope().addLocal(local_typeID, local_qualifiers, local_name); }
#undef pre

  InlineNodiscardCXPR bool containsLocal(std::string_view local_name) const noexcept { return current_scope().getLocal(local_name) not_eq nullptr; }

};

class Module final : public SymbolTable {
  static constexpr auto search_pred = [] (auto const& type, auto const& other_type) { return type.sameAs(other_type); };
  static constexpr auto named_search_pred = [] (auto const& type, std::string_view name) { return type.nameof() == name; };

  eden::swap_vector<ArrayType> array_types; // using a swap vector here is bad, we should only ever use search_noswap. TODO: Change
  eden::swap_vector<CustomType> custom_types;
  u16_t id;
//byte_t _pad[6];

  template <std::derived_from<Type> T>
  InlineNodiscardCXPR static TypeID
  getExisting(T const* type_in_vector, eden::swap_vector<T>& owning_vector, u16_t module_id) noexcept {
    return {.derived = Type::corresponding_derived_type<T>(), .module_id = module_id, .id = (u16_t)owning_vector.index_in(type_in_vector)};
  }

  template <std::derived_from<Type> T>
  InlineNodiscardCXPR static TypeID
  makeNew(eden::swap_vector<T>& type_vector, u16_t module_id, T&& type) noexcept {
    auto const fn_type_id = (u16_t) type_vector.size();
    type_vector.emplace_back(std::move(type));
    return {.derived = Type::corresponding_derived_type<T>(), .module_id = module_id, .id = fn_type_id};
  }

  template <std::derived_from<Type> T>
  InlineNodiscardCXPR static TypeID
  returnExistingOrNew(eden::swap_vector<T>& type_vector, u16_t module_id, auto... args) noexcept {
    T tmp(args...);
    auto const res = type_vector.search_noswap(search_pred, tmp);
    if (res) return getExisting(res, type_vector, module_id);
    return makeNew(type_vector, module_id, std::move(tmp));
  }

public:
  InlineCXPR explicit Module(u16_t module_id) noexcept : id(module_id) {}

  InlineNodiscardCXPR u16_t getID()           const noexcept { return id; }
  InlineNodiscardCXPR sz_t numArrayTypes()    const noexcept { return array_types.size(); }
  InlineNodiscardCXPR sz_t numCustomTypes()   const noexcept { return custom_types.size(); }

  InlineNodiscardCXPR TypeID addArrayType(TypeID subtypeID, u64_t array_size) noexcept { return returnExistingOrNew(array_types, id, subtypeID, array_size); }

#define pre assert(getCustomTypeID(type_name) == devoidID);
  InlineNodiscardCXPR TypeID
  addCustomType(std::string_view type_name, eden::swap_vector<Variable>&& members) noexcept { pre
    auto const customID = makeNew(custom_types, id, CustomType{type_name});
    auto& custom = custom_types[customID.id];
    custom.member_table().variables = std::move(members);
    return customID;
  }
#undef pre

#define pre assert(arrayTypeID.module_id == id); assert(arrayTypeID.derived == Type::ARRAY); assert(not arrayTypeID.isPointer());
  InlineNodiscardCXPR ArrayType const& getArrayType(TypeID arrayTypeID) const noexcept { pre return array_types[arrayTypeID.id]; }
#undef pre

#define pre assert(customTypeID.module_id == id); assert(customTypeID.derived == Type::CUSTOM); assert(not customTypeID.isPointer());
  InlineNodiscardCXPR CustomType const& getCustomType(TypeID customTypeID) const noexcept { pre return custom_types[customTypeID.id]; }
#undef pre

  // returns devoidID if not found
  InlineNodiscardCXPR TypeID
  getCustomTypeID(std::string_view name) noexcept {
    auto const res = custom_types.search_noswap(named_search_pred, name); 
    if(res) return getExisting(res, custom_types, id);
    return TypeID { .derived = Type::DEVOID };
  }

};

#include "table_and_module_sync.hpp"
static_assert(sizeof(SymbolTable) == SYMBOL_TABLE_SIZE);
static_assert(alignof(SymbolTable) == SYMBOL_TABLE_ALIGNMENT);
static_assert(sizeof(Module) == MODULE_SIZE);
static_assert(alignof(Module) == MODULE_ALIGNMENT);

}