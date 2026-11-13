#pragma once
#include "types.hpp"

#include "edenlib/macros.hpp"
#include "edenlib/utils.hpp"
#include "edenlib/type_aliases.hpp"
#include "edenlib/vectors/swap_vector.hpp"
#include "edenlib/flat_map.hpp"

namespace LOM {
using namespace eden::type_aliases;

namespace Symbol {

static constexpr auto name_search = [] (auto const& e, std::string_view key) { return e.nameof() == key; };
static constexpr auto id_search = [] (auto const& e, u16_t id) { return e.id == id; };
static constexpr auto get_id_of = [] (auto const& e) { return e.id; };

struct Variable {
  TypeID typeID;
  Qualifiers qualifiers;
  bool is_public;
  u16_t id;
  u32_t name_len;
  char const* name;

  constexpr Variable(TypeID typeID, Qualifiers qualifiers, std::string_view name, bool is_public, u16_t variable_insert_order) noexcept
  : typeID(typeID), qualifiers(qualifiers), is_public(is_public), id(variable_insert_order), name_len(name.length()), name(name.data())
  { }

  InlineNodiscardCXPR std::string_view nameof() const noexcept { return std::string_view(name, name_len); }
};

struct Function {
  friend class LOM::SymbolTable; friend class LOM::Module;

  TypeID return_typeID;
  mutable eden::swap_vector16<Variable> locals;
  char const* name;
  u32_t name_len;
  u16_t id;
  bool is_public : 1;
  bool is_variadic : 1;
  u8_t num_parameters : 6;

  InlineNodiscardCXPR std::string_view nameof() const noexcept { return {name, name_len}; }

#define pre Assume(locals.search_noswap(name_search, var_name) == nullptr);
  InlineCXPR void addLocal(TypeID var_typeID, Qualifiers var_qualifiers, std::string_view var_name) noexcept { pre locals.emplace_back(var_typeID, var_qualifiers, var_name, false, locals.size()); }
#undef pre

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another local is added or searched.
  NodiscardCXPR Variable const* getLocal(std::string_view variable_name) const noexcept { return locals.search(name_search, variable_name); }

  // reference is not stable and may be invalidated if another local is added or searched.
#define pre assert(locals.search_noswap(id_search, local_id));
  NodiscardCXPR Variable const& getLocal(u16_t local_id) const noexcept { pre return locals.gradual_sort_search(get_id_of, local_id); }
#undef pre

  InlineNodiscardCXPR sz_t getNumParameters() const noexcept { return num_parameters; }

private:
  constexpr Function(std::string_view name, eden::swap_vector16<Variable>&& parameters, TypeID return_typeID, bool is_public, bool is_variadic, u16_t functon_insert_order)
  : return_typeID(return_typeID), locals(std::move(parameters)),
    name(name.data()), name_len(name.length()),
    id(functon_insert_order), is_public(is_public), is_variadic(is_variadic) {
    assert(locals.size() <= 63);
    num_parameters = eden::narrow<u8_t>(locals.size());
    assert(locals.is_ordered(get_id_of));
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

  // function vector preserves backmost element
  mutable eden::swap_vector<Function, eden::swap_vector_settings<4, true>{}> functions;
  mutable eden::swap_vector<Variable> variables;

  InlineNodiscardCXPR Function& current_scope() const noexcept { assert(not functions.empty()); return functions.back(); }
public:

#define pre assert(not variables.search_noswap(name_search, var_name));
  InlineCXPR void addVariable(TypeID var_typeID, Qualifiers var_qualifiers, std::string_view var_name, bool is_public) noexcept { pre variables.emplace_back( Variable{var_typeID, var_qualifiers, var_name, is_public, eden::narrow<u16_t>(variables.size())} ); }
#undef pre

  // returns nullptr if non-existent. pointer is not stable and may be invalidated if another global is added or searched.
  InlineNodiscardCXPR Variable const* getVariable(std::string_view variable_name) const noexcept { return variables.search(name_search, variable_name); }

  // Reference is stable unless another variable is added or searched not using ID.
#define pre assert(variables.search_noswap(id_search, variable_id));
  NodiscardCXPR Variable const& getVariable(u16_t variable_id) const noexcept { pre return variables.gradual_sort_search(get_id_of, variable_id); }
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
    auto const function_id = eden::narrow<u16_t>(functions.size());
    functions.emplace_back( Function{ function_name, std::move(parameters), return_typeID, is_public, is_variadic, function_id } );
    return function_id;
  }
#undef pre

#define pre assert(functions.search_noswap(id_search, function_id));
  InlineCXPR void enterFunctionScope(u16_t function_id) noexcept { pre [[maybe_unused]] auto _ = functions.search_swapback(id_search, function_id); }
#undef pre

#define pre assert(not current_scope().getLocal(local_name));
  InlineCXPR void addLocal(TypeID local_typeID, Qualifiers local_qualifiers, std::string_view local_name) noexcept { pre current_scope().addLocal(local_typeID, local_qualifiers, local_name); }
#undef pre

  InlineNodiscardCXPR bool containsLocal(std::string_view local_name) const noexcept { return current_scope().getLocal(local_name) not_eq nullptr; }

};

class Module final : public SymbolTable {
  static constexpr auto search_pred = [] (auto const& type, auto const& other_type) { return type.sameAs(other_type); };
  static constexpr auto named_search_pred = [] (auto const& type, std::string_view name) { return type.nameof() == name; };

  eden::swap_vector<ArrayType> array_types; // using a swap vector here is bad, we should only ever use search_noswap. TODO: Change
  eden::small_flat_map<std::string_view, CustomType> custom_types;
  u16_t id;
//byte_t _pad[6];

public:
  InlineCXPR explicit Module(u16_t module_id) noexcept : id(module_id) {}

  InlineNodiscardCXPR u16_t getID()         const noexcept { return id; }
  InlineNodiscardCXPR sz_t numArrayTypes()  const noexcept { return array_types.size(); }
  InlineNodiscardCXPR sz_t numCustomTypes() const noexcept { return custom_types.size(); }

  InlineNodiscardCXPR TypeID 
  addArrayType(TypeID subtypeID, u64_t array_size) noexcept {
    ArrayType tmp{subtypeID, array_size};
    auto const arr = array_types.search_noswap(search_pred, tmp);

    TypeID res{.derived = TypeID::ARRAY, .module_id = id, .id = 0};
    if(arr)
      res.id = eden::narrow<u16_t>(array_types.index_of(eden::unsafe, arr));
    else
      array_types.emplace_back(std::move(tmp)),
      res.id = eden::narrow<u16_t>(array_types.size() - 1);
    
    return res;
  }

  // returns errorID if type already exists
  InlineNodiscardCXPR TypeID
  addCustomType(std::string_view type_name, eden::swap_vector<Variable>&& members) noexcept {
    auto const [idx, success] = custom_types.emplace(std::move(type_name), CustomType{});
    if(not success) return errorID;
    
    custom_types.getFromIndex(idx).member_table().variables = std::move(members);
    return TypeID{ .derived = TypeID::CUSTOM, .module_id = id, .id = eden::narrow<u16_t>(idx) };
  }

#define pre assert(arrayTypeID.module_id == id); assert(arrayTypeID.derived == TypeID::ARRAY); assert(not arrayTypeID.isPointer());
  InlineNodiscardCXPR ArrayType const& getArrayType(TypeID arrayTypeID) const noexcept { pre return array_types[arrayTypeID.id]; }
#undef pre

#define pre assert(customTypeID.module_id == id); assert(customTypeID.isCustom());
  InlineNodiscardCXPR CustomType const& getCustomType(TypeID customTypeID) const noexcept { pre return custom_types.getFromIndex(customTypeID.id); }
#undef pre

#define pre assert(customTypeID.module_id == id); assert(customTypeID.isCustom());
  InlineNodiscardCXPR std::string_view 
  getCustomTypeName(TypeID customTypeID) const noexcept { pre return custom_types.getKeyOf(customTypeID.id); }
#undef pre

  // returns errorID if not found
  InlineNodiscardCXPR TypeID
  getCustomTypeID(std::string_view name) noexcept {
    auto const custom = custom_types.find(name);
    if(custom)
      return {.derived = TypeID::CUSTOM, .module_id = id, .id = eden::narrow<u16_t>(custom_types.index_of(eden::unsafe, custom))};
    else
      return errorID;
  }

};

#include "table_and_module_sync.hpp"
static_assert(sizeof(SymbolTable) == SYMBOL_TABLE_SIZE);
static_assert(alignof(SymbolTable) == SYMBOL_TABLE_ALIGNMENT);
static_assert(sizeof(Module) == MODULE_SIZE);
static_assert(alignof(Module) == MODULE_ALIGNMENT);

}