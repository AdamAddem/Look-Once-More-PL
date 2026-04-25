#include "symbol_table.hpp"

#include <cassert>

using namespace LOM;

const FunctionType*
Module::addFunction(const eden::owned_stringview& name, std::span<Variable> parameters, const Type* return_type, bool is_public) noexcept {
  assert(not symbols.contains(name));
  assert(parameters.size() <= Settings::MAX_FUNCTION_PARAMETERS);
  const Type* parameter_types[Settings::MAX_FUNCTION_PARAMETERS];
  sz_t sz{};
  for (; sz < parameters.size(); ++sz)
    parameter_types[sz] = parameters[sz].type.type;

  const auto function_type = types.addFunction(std::span(parameter_types, sz), return_type);
  symbols.emplace(name, Function{parameters, function_type, is_public});
  return function_type;
}