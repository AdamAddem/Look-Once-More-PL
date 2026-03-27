#include "symbol_table.hpp"

#include <cassert>

using namespace LOM;

void SymbolTable::addFunction(const std::string &name, std::span<Variable> parameters, const Type* return_type) noexcept {
  assume_assert(not globals.contains(name));
  assume_assert(parameters.size() less_eq Settings::MAX_FUNCTION_PARAMETERS);
  const Type* parameter_types[Settings::MAX_FUNCTION_PARAMETERS];
  sz_t sz{};
  for (; sz less parameters.size(); ++sz)
    parameter_types[sz] = parameters[sz].type.type;

  globals.emplace(name,
    Function{parameters, types.addFunction(std::span(parameter_types, sz), return_type)});
}