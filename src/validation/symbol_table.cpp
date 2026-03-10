#include "symbol_table.hpp"
#include <cassert>
#include <iostream>
#include <stdexcept>

using namespace AST;
// this probably shouldn't be here
// and also should be done way better idk
SymbolTable::SymbolTable() {

  //0 alignment represents undefined alignment
  type_registry.emplace("");
  type_registry.emplace("devoid");

  type_registry.emplace("i8");
  type_registry.emplace("i16");
  type_registry.emplace("i32");
  type_registry.emplace("i64");

  type_registry.emplace("u8");
  type_registry.emplace("u16");
  type_registry.emplace("u32");
  type_registry.emplace("u64");

  type_registry.emplace("f32");
  type_registry.emplace("f64");

  type_registry.emplace("char");
  type_registry.emplace("string");
  type_registry.emplace("bool");

  type_registry.emplace("raw");
  type_registry.emplace("unique");
  type_registry.emplace("vague");

}


constexpr SymbolTable::FunctionSignature::FunctionSignature(Types&& initial_parameter_types, Type&& initial_return_type) {
  addFunction(std::move(initial_parameter_types), std::move(initial_return_type));
}

void SymbolTable::FunctionSignature::addFunction(Types&& parameter_types, Type&& return_type) {
  function_types.emplace_back(Type::function, std::move(parameter_types), std::move(return_type));
}

const Type& SymbolTable::FunctionSignature::returnTypeOfCall(const std::vector<Type> &provided_param) const {
  const auto num_params = provided_param.size();
  int valid_index = -1;
  for (auto i{0uz}; i<function_types.size(); ++i) {
    const auto& f = function_types[i].getTypes();

    if (num_params != (f.size() - 1))
      continue;

    for (auto param{0uz}; param < provided_param.size(); ++param) {
      if (!provided_param[param].convertible_to(f[param]))
        goto escape;
    }

    if (valid_index != -1)
      throw std::runtime_error("Ambiguous function overload");

    valid_index = i;
  escape:
  }

  if (valid_index == -1)
    throw std::runtime_error("No viable function overload found");

  return function_types[valid_index].getTypes().back();
}

bool SymbolTable::containsVariable(const std::string &name) const {
  for (const auto &scope : locals) // order doesn't matter
    if (scope.variables.contains(name))
      return true;

  if (globals.contains(name))
    return std::holds_alternative<TypeInstance>(globals.at(name));
  return false;
}

void SymbolTable::addGlobalVariable(std::string name, Type type) {
  if (!isRegisteredType(type))
    throw std::runtime_error("Undefined type used in variable declaration");

  if (globals.contains(name))
    throw std::runtime_error("Redefinition of global symbol with name: " +
                             name);

  globals.emplace(std::move(name), TypeInstance(std::move(type)));
}

void SymbolTable::addLocalVariable(std::string name, Type type) {
  if (!isRegisteredType(type))
    throw std::runtime_error("Undefined type used in variable declaration");

  if (locals.back().variables.contains(name))
    throw std::runtime_error("Redefinition of local symbol with name: " + name);

  locals.back().variables.emplace(std::move(name),
                                  TypeInstance(std::move(type)));
}

auto SymbolTable::closestVariable(const std::string &name) const -> const TypeInstance & {
  const auto end = locals.rend();
  for (auto curr = locals.rbegin(); curr != end; ++curr)
    if (curr->variables.contains(name))
      return curr->variables.at(name);

  if (globals.contains(name))
    return std::get<TypeInstance>(globals.at(name));

  throw std::runtime_error(
      "No variable found in typeOfMatchingVariable method");
}

const Type& SymbolTable::closestVariableType(const std::string &name) const {
  return closestVariable(name).type;
}

void SymbolTable::enterScope(Type expected_return_type) {
  locals.emplace_back(std::move(expected_return_type));
}

void SymbolTable::leaveScope() { locals.pop_back(); }

const Type& SymbolTable::returnTypeOfCurrentScope() const {
  assert(!locals.empty());
  return locals.back().expected_return_type;
}

void SymbolTable::addFunction(const std::string &name, Type return_type, std::vector<Type> &&parameter_types) {
  if (!isRegisteredType(return_type))
    throw std::runtime_error("Function return type undefined");

  if (globals.contains(name)) {
    if (std::holds_alternative<TypeInstance>(globals.at(name)))
      throw std::runtime_error("Redefinition of global variable with name: " +
                               name);
  } else {
    globals.emplace(name, FunctionSignature(std::move(parameter_types), std::move(return_type)));
    return;
  }

  std::get<FunctionSignature>(globals.at(name)).addFunction(std::move(parameter_types), std::move(return_type));
}

bool SymbolTable::containsFunction(const std::string &name) const {
  if (globals.contains(name))
    return std::holds_alternative<FunctionSignature>(globals.at(name));
  return false;
}

const Type& SymbolTable::returnTypeOfCall(const std::string &name, const std::vector<Type> &provided_params) const {
  if (!containsFunction(name))
    throw std::runtime_error("Identifier not recognized as callable");

  return std::get<FunctionSignature>(globals.at(name)).returnTypeOfCall(provided_params);
}

bool SymbolTable::isSymbolInCurrentScope(const std::string &name) const {
  if (locals.empty())
    return globals.contains(name);

  return locals.back().variables.contains(name);
}

bool SymbolTable::isVarMutable(const std::string &var_name) const {
  if (!containsVariable(var_name))
    return false;

  return closestVariable(var_name).is_mutable;
}


bool SymbolTable::isRegisteredType(const Type &t) const noexcept {
  if (t.isVariant()) {
    for (const auto& type : t.getTypes()) {
      if (!type_registry.contains(type.getTypename()))
        return false;
    }
    return true;
  }

  return type_registry.contains(t.getTypename());
}

void SymbolTable::clearLocalTable() { locals.clear(); }
