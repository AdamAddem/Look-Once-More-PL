#include "symbol_table.hpp"

#include "error.hpp"

#include <cassert>

using namespace LOM;

eden_return_nonnull eden_nonull_args
const FunctionType*
Module::addFunction(std::string_view name, std::span<Variable> parameters, const Type* return_type, bool is_public, bool is_variadic) {
  if (symbols.contains(name))
    throw ValidationError("Function redefined.", name.data(), 0);

  assert(parameters.size() <= Settings::MAX_FUNCTION_PARAMETERS);
  const Type* parameter_types[Settings::MAX_FUNCTION_PARAMETERS];
  sz_t sz{};
  for (; sz < parameters.size(); ++sz)
    parameter_types[sz] = parameters[sz].type.type;

  const auto function_type = types.addFunction(std::span(parameter_types, sz), return_type, is_variadic);
  symbols.emplace(name, Function{parameters, function_type, is_public});
  return function_type;
}