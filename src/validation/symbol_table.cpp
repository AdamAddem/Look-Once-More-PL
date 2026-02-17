#include "symbol_table.hpp"
#include <iostream>
#include <stdexcept>
#include <cassert>

SymbolTable::SymbolTable() {
	type_registry["int"] = {.arithmetic = true, .callable = false, .array = false};
	type_registry["uint"] = {.arithmetic = true, .callable = false, .array = false};
	type_registry["float"] = {.arithmetic = true, .callable = false, .array = false};
	type_registry["double"] = {.arithmetic = true, .callable = false, .array = false};
	type_registry["char"] = {.arithmetic = true, .callable = false, .array = false};
	type_registry["uchar"] = {.arithmetic = true, .callable = false, .array = false};
	type_registry["string"] = {.arithmetic = false, .callable = false, .array = false};
	type_registry["bool"] = {.arithmetic = true, .callable = false, .array = false};
	type_registry["short"] = {.arithmetic = true, .callable = false, .array = false};
	type_registry["long"] = {.arithmetic = true, .callable = false, .array = false};
	type_registry["signed"] = {.arithmetic = true, .callable = false, .array = false};
	type_registry["unsigned"] = {.arithmetic = true, .callable = false, .array = false};
	type_registry["devoid"] = {.arithmetic = false, .callable = false, .array = false};
}


void SymbolTable::Function::print() const {
  std::cout << return_type;
  std::cout << " (";
  
  for (auto &t : parameter_types) {
    printType(t);
    std::cout << ", ";
  }
  
  if (!parameter_types.empty())
    std::cout << "\b\b";
  std::cout << ")";
}

void SymbolTable::FunctionSignature::addFunction(std::string &&t, std::vector<Types> &&params) {
  functions.emplace_back(std::move(t), std::move(params));
}

std::string SymbolTable::FunctionSignature::returnTypeOfCall(const std::vector<Types> &provided_param) const {
  bool is_valid{false};
  std::string ret_type;
  for (const auto &f : functions) {

    if (provided_param.size() != f.parameter_types.size())
      continue;

    for (auto i{0uz}; i < provided_param.size(); ++i) {
      if (!are_types_convertible(provided_param[i], f.parameter_types[i]))
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
        "Parameters not matching any variant of function signature");

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
  for (const auto& scope : locals) // order doesn't matter
    if (scope.variables.contains(name))
      return true;

  if (globals.contains(name))
    return std::holds_alternative<Variable>(globals.at(name));
  return false;
}

void SymbolTable::addGlobalVariable(std::string name, Types type, const bool is_mutable) {

  if (globals.contains(name))
    throw std::runtime_error("Redefinition of global symbol with name: " +
                             name);

  globals.emplace(std::move(name), Variable(std::move(type), is_mutable));
}

void SymbolTable::addLocalVariable(std::string name, Types type, const bool is_mutable) {
  if (locals.back().variables.contains(name))
    throw std::runtime_error("Redefinition of local symbol with name: " + name);

  locals.back().variables.emplace(std::move(name), Variable(std::move(type), is_mutable));
}

const SymbolTable::Variable &SymbolTable::closestVariable(const std::string &name) const {
  auto end = locals.rend();
  for (auto curr = locals.rbegin(); curr != end; ++curr)
    if (curr->variables.contains(name))
      return curr->variables.at(name);

  if (globals.contains(name))
    return std::get<Variable>(globals.at(name));

  throw std::runtime_error(
      "No variable found in typeOfMatchingVariable method");
}

Types SymbolTable::typeOfClosestVariable(const std::string &name) const {
	return closestVariable(name).type;
}

void SymbolTable::enterScope(std::string expected_return_type) {
	locals.emplace_back(std::move(expected_return_type));
}
void SymbolTable::leaveScope() {
  locals.pop_back();
}

std::string SymbolTable::returnTypeOfCurrentScope() const {
	assert(!locals.empty());
	return locals.back().expected_return_type;
}

void SymbolTable::addFunction(const std::string& name, std::string ret_type, std::vector<Types> &&parameter_types) {
  if (globals.contains(name)) {
    if (std::holds_alternative<Variable>(globals.at(name)))
      throw std::runtime_error("Redefinition of global variable with name: " +
                               name);
  } else {
    globals.emplace(name,
                    FunctionSignature(Function(std::move(ret_type),
                                               std::move(parameter_types))));
    return;
  }

  std::get<FunctionSignature>(globals.at(name))
      .addFunction(std::move(ret_type), std::move(parameter_types));
}

bool SymbolTable::containsFunction(const std::string &name) const {
  if (globals.contains(name))
    return std::holds_alternative<FunctionSignature>(globals.at(name));
  return false;
}

std::string SymbolTable::returnTypeOfCall(const std::string &name, const std::vector<Types> &provided_params) const {
  if (!containsFunction(name))
    throw std::runtime_error("Identifier not recognized as callable");

  return std::get<FunctionSignature>(globals.at(name))
      .returnTypeOfCall(provided_params);
}

bool SymbolTable::isSymbolInCurrentScope(const std::string &name) const {
  if (locals.empty())
    return globals.contains(name);

  return locals.back().variables.contains(name);
}

bool SymbolTable::isAssignable(const std::string &var_name) const {
  if (!containsVariable(var_name))
    return false;

  return closestVariable(var_name).is_mutable;
}

auto SymbolTable::detailsOfType(const std::string &type_name) const -> TypeDetails {
  if (!type_registry.contains(type_name))
    throw std::runtime_error("Type name not registered");

  return type_registry.at(type_name);
}



void SymbolTable::clearLocalTable() { locals.clear(); }

void SymbolTable::printGlobals() {

  std::cout << "\nGlobals:\n{\n" << std::endl;
  for (auto &[name, entry] : globals) {
    if (auto *var = std::get_if<Variable>(&entry)) {
      std::cout << "Variable: ";
      printType(var->type);
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
    printType(var.type);
    std::cout << " " << name << ";\n";
  }
  std::cout << "\n}" << std::endl;
}
