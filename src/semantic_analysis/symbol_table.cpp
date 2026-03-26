#include "symbol_table.hpp"

#include <cassert>
#include <stdexcept>

using namespace LOM;


bool SymbolTable::containsVariable(const std::string &name) const noexcept {
  if (current_scope && current_scope->containsVariable(name))
    return true;

  if (globals.contains(name))
    return std::holds_alternative<InstantiatedType>(globals.at(name));

  return false;
}

void SymbolTable::addGlobalVariable(std::string name, InstantiatedType type) {
  if (globals.contains(name))
    throw std::runtime_error("Redefinition of global symbol with name: " + name);

  globals.emplace(std::move(name), type);
}

void SymbolTable::addLocalVariable(std::string name, InstantiatedType type) const noexcept {
  assert(current_scope not_eq nullptr);
  const bool res = current_scope->addVariable(std::move(name), type);
  assert(res);
}

InstantiatedType SymbolTable::closestVariable(const std::string &name) noexcept {
  if (current_scope && current_scope->containsVariable(name))
    return current_scope->getVariable(name);

  assert(globals.contains(name));
  assert(std::holds_alternative<InstantiatedType>(globals[name]));
  return std::get<InstantiatedType>(globals.at(name));
}

void SymbolTable::addFunction(const std::string &name, std::span<Variable> parameters, const Type* return_type) noexcept {
  assert(not globals.contains(name));
  assert(parameters.size() <= Settings::MAX_FUNCTION_PARAMETERS);

  const Type* parameter_types[Settings::MAX_FUNCTION_PARAMETERS];
  sz_t sz{};
  for (; sz< parameters.size(); ++sz)
    parameter_types[sz] = parameters[sz].type.type;

  globals.emplace(name,
    Function{parameters, types.addFunction(std::span(parameter_types, sz), return_type)});
}