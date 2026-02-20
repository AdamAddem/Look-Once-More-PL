#include "symbol_table.hpp"
#include <cassert>
#include <iostream>
#include <stdexcept>

// this probably shouldn't be here
// and also should be done way better idk
SymbolTable::SymbolTable() {
  type_registry["i8"] = {
      .arithmetic = true, .callable = false, .array = false};
  type_registry["i16"] = {
    .arithmetic = true, .callable = false, .array = false};
  type_registry["i32"] = {
    .arithmetic = true, .callable = false, .array = false};
  type_registry["i64"] = {
      .arithmetic = true, .callable = false, .array = false};

  type_registry["u8"] = {
    .arithmetic = true, .callable = false, .array = false};
  type_registry["u16"] = {
    .arithmetic = true, .callable = false, .array = false};
  type_registry["u32"] = {
    .arithmetic = true, .callable = false, .array = false};
  type_registry["u64"] = {
    .arithmetic = true, .callable = false, .array = false};

  type_registry["f32"] = {
    .arithmetic = true, .callable = false, .array = false};
  type_registry["f64"] = {
    .arithmetic = true, .callable = false, .array = false};

  type_registry["char"] = {
      .arithmetic = true, .callable = false, .array = false};
  type_registry["string"] = {
      .arithmetic = false, .callable = false, .array = false};
  type_registry["bool"] = {
      .arithmetic = true, .callable = false, .array = false};
  type_registry["devoid"] = {
      .arithmetic = false, .callable = false, .array = false};
}

void SymbolTable::Function::print() const {
  return_type.print();
  std::cout << " (";

  for (auto &t : parameter_types) {
    t.print();
    std::cout << ", ";
  }

  if (!parameter_types.empty())
    std::cout << "\b\b";
  std::cout << ")";
}

void SymbolTable::FunctionSignature::addFunction(Type &&t,
                                                 std::vector<Type> &&param_types) {
  functions.emplace_back(std::move(t), std::move(param_types));
}

Type SymbolTable::FunctionSignature::returnTypeOfCall( //untested
    const std::vector<Type> &provided_param) const {
  bool is_valid{false};
  Type ret_type{devoid_type};
  for (const auto &f : functions) {
    if (provided_param.size() != f.parameter_types.size())
      continue;

    for (auto i{0uz}; i < provided_param.size(); ++i) {
      if (!provided_param[i].convertible_to(f.parameter_types[i]))
        goto escape;
    }

    if (is_valid)
      throw std::runtime_error("Ambiguous function overload");

    is_valid = true;
    ret_type = f.return_type;

  escape:
  }

  if (!is_valid)
    throw std::runtime_error(
        "No viable function overload found");

  return ret_type;
}

void SymbolTable::FunctionSignature::print() const {
  for (auto &f : functions) {
    f.print();
    std::cout << ", ";
  }
  std::cout << "\b\b";
}

bool SymbolTable::containsVariable(const std::string &name) const {
  for (const auto &scope : locals) // order doesn't matter
    if (scope.variables.contains(name))
      return true;

  if (globals.contains(name))
    return std::holds_alternative<Variable>(globals.at(name));
  return false;
}

void SymbolTable::addGlobalVariable(std::string name, Type type) {
  if (!isRegisteredType(type))
    throw std::runtime_error("Undefined type used in variable declaration");

  if (globals.contains(name))
    throw std::runtime_error("Redefinition of global symbol with name: " +
                             name);

  globals.emplace(std::move(name), Variable(std::move(type)));
}

void SymbolTable::addLocalVariable(std::string name, Type type) {
  if (!isRegisteredType(type))
    throw std::runtime_error("Undefined type used in variable declaration");

  if (locals.back().variables.contains(name))
    throw std::runtime_error("Redefinition of local symbol with name: " + name);

  locals.back().variables.emplace(std::move(name),
                                  Variable(std::move(type)));
}

auto SymbolTable::closestVariable(const std::string &name) const -> const Variable & {
  const auto end = locals.rend();
  for (auto curr = locals.rbegin(); curr != end; ++curr)
    if (curr->variables.contains(name))
      return curr->variables.at(name);

  if (globals.contains(name))
    return std::get<Variable>(globals.at(name));

  throw std::runtime_error(
      "No variable found in typeOfMatchingVariable method");
}

Type SymbolTable::closestVariableType(const std::string &name) const {
  return closestVariable(name).type;
}

void SymbolTable::enterScope(Type expected_return_type) {
  locals.emplace_back(std::move(expected_return_type));
}

void SymbolTable::leaveScope() { locals.pop_back(); }

Type SymbolTable::returnTypeOfCurrentScope() const {
  assert(!locals.empty());
  return locals.back().expected_return_type;
}

void SymbolTable::addFunction(const std::string &name, Type return_type,
                              std::vector<Type> &&parameter_types) {
  if (!isRegisteredType(return_type))
    throw std::runtime_error("Function return type undefined");

  if (globals.contains(name)) {
    if (std::holds_alternative<Variable>(globals.at(name)))
      throw std::runtime_error("Redefinition of global variable with name: " +
                               name);
  } else {
    globals.emplace(name,
                    FunctionSignature(Function(std::move(return_type),
                                               std::move(parameter_types))));
    return;
  }

  std::get<FunctionSignature>(globals.at(name))
      .addFunction(std::move(return_type), std::move(parameter_types));
}

bool SymbolTable::containsFunction(const std::string &name) const {
  if (globals.contains(name))
    return std::holds_alternative<FunctionSignature>(globals.at(name));
  return false;
}

Type
SymbolTable::returnTypeOfCall(const std::string &name,
                              const std::vector<Type> &provided_params) const {
  if (!containsFunction(name))
    throw std::runtime_error("Identifier not recognized as callable");

  return std::get<FunctionSignature>(globals.at(name))
      .returnTypeOfCall(provided_params).asImmutable();
}

bool SymbolTable::isSymbolInCurrentScope(const std::string &name) const {
  if (locals.empty())
    return globals.contains(name);

  return locals.back().variables.contains(name);
}

bool SymbolTable::isVarMutable(const std::string &var_name) const {
  if (!containsVariable(var_name))
    return false;

  return closestVariable(var_name).type.is_mutable;
}

auto SymbolTable::detailsOfType(const std::string &type_name) const
    -> TypeDetails {
  if (!type_registry.contains(type_name))
    throw std::runtime_error("Type name not registered");

  return type_registry.at(type_name);
}

bool SymbolTable::isRegisteredType(const Type &t) const noexcept {
  if (t.isVariant()) {
    for (auto& type : t.getTypes()) {
      if (!type_registry.contains(type.getTypename()))
        return false;
    }
    return true;
  }

  return type_registry.contains(t.getTypename());
}

void SymbolTable::clearLocalTable() { locals.clear(); }


void SymbolTable::printGlobals() {
  std::cout << "\nGlobals:\n{\n" << std::endl;
  for (auto &[name, entry] : globals) {
    if (const auto *var = std::get_if<Variable>(&entry)) {
      std::cout << "Variable: ";
      var->type.print();
      std::cout << " " << name;
    } else {
      std::cout << "Function: ";
      std::get<FunctionSignature>(entry).print();
    }

    std::cout << std::endl;
  }
  std::cout << "\n}" << std::endl;
}

void SymbolTable::printLocals() {
  if (locals.back().variables.empty())
    return;

  std::cout << "\nScope:\n{\n" << std::endl;
  for (auto &[name, var] : locals.back().variables) {
    var.type.print();
    std::cout << " " << name << ";\n";
  }
  std::cout << "\n}" << std::endl;
}