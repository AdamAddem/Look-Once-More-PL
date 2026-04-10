#include "symbol_table.hpp"

#include <cassert>

using namespace LOM;

void SymbolTable::addFunction(const eden::owned_stringview& name, std::span<Variable> parameters, const Type* return_type) noexcept {
  assert(not globals.contains(name));
  assert(parameters.size() <= Settings::MAX_FUNCTION_PARAMETERS);
  const Type* parameter_types[Settings::MAX_FUNCTION_PARAMETERS];
  sz_t sz{};
  for (; sz < parameters.size(); ++sz)
    parameter_types[sz] = parameters[sz].type.type;

  globals.emplace(name,
    Function{parameters, types.addFunction(std::span(parameter_types, sz), return_type)});
}