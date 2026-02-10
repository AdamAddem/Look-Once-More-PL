#include "symbol_table.hpp"
#include <iostream>
#include <stdexcept>

void SymbolTable::Function::print() {
  printType(return_type);
  std::cout << " (";
  
  for (auto &t : parameter_types) {
    printType(t);
    std::cout << ", ";
  }
  
  if (!parameter_types.empty())
    std::cout << "\b\b";
  std::cout << ")";
}

void SymbolTable::FunctionSignature::addFunction(StrictType &&t, std::vector<Type> &&params) {
  functions.emplace_back(std::move(t), std::move(params));
}

StrictType SymbolTable::FunctionSignature::returnTypeOfCall(const std::vector<Type> &provided_param) {
  bool is_valid{false};
  StrictType ret_type("placeholder");
  for (const auto &f : functions) {

    if (provided_param.size() != f.parameter_types.size())
      continue;

    for (auto i{0uz}; i < provided_param.size(); ++i) {
      if (!convertibleFromTo(provided_param[i], f.parameter_types[i]))
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

void SymbolTable::FunctionSignature::print() {
  for (auto &f : functions) {
    f.print();
    std::cout << ", ";
  }
  std::cout << "\b\b";
}

bool SymbolTable::containsVariable(const std::string &name) {
  for (const auto& m : locals) // order doesn't matter
    if (m.contains(name))
      return true;

  if (globals.contains(name))
    return std::holds_alternative<Variable>(globals.at(name));
  return false;
}

void SymbolTable::addGlobalVariable(std::string name, Type type, bool is_const) {

  if (globals.contains(name))
    throw std::runtime_error("Redefinition of global symbol with name: " +
                             name);

  globals.emplace(std::move(name), Variable(std::move(type), is_const));
}

void SymbolTable::addLocalVariable(std::string name, Type type, bool is_const) {
  if (locals.back().contains(name))
    throw std::runtime_error("Redefinition of local symbol with name: " + name);

  locals.back().emplace(std::move(name), Variable(std::move(type), is_const));
}

const SymbolTable::Variable &SymbolTable::closestVariable(const std::string &name) {
  auto end = locals.rend();
  for (auto curr = locals.rbegin(); curr != end; ++curr)
    if (curr->contains(name))
      return curr->at(name);

  if (globals.contains(name))
    return std::get<Variable>(globals.at(name));

  throw std::runtime_error(
      "No variable found in typeOfMatchingVariable method");
}

void SymbolTable::enterScope() { locals.emplace_back(); }
void SymbolTable::leaveScope() {
  printLocals();
  locals.pop_back();
}

void SymbolTable::addFunction(std::string name, StrictType ret_type, std::vector<Type> &&parameter_types) {
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

bool SymbolTable::containsFunction(const std::string &name) {
  if (globals.contains(name))
    return std::holds_alternative<FunctionSignature>(globals.at(name));
  return false;
}

StrictType SymbolTable::returnTypeOfCall(const std::string &name, const std::vector<Type> &provided_params) {
  if (!containsFunction(name))
    throw std::runtime_error("Identifier not recognized as callable");

  return std::get<FunctionSignature>(globals.at(name))
      .returnTypeOfCall(provided_params);
}

bool SymbolTable::isSymbolInCurrentScope(const std::string &name) {
  if (locals.empty())
    return globals.contains(name);

  return locals.back().contains(name);
}

bool SymbolTable::isAssignable(const std::string &var_name) {
  if (!containsVariable(var_name))
    return false;

  return closestVariable(var_name).is_mutable;
}

auto SymbolTable::detailsOfType(const std::string &type_name) -> TypeDetails {
  if (!type_registry.contains(type_name))
    throw std::runtime_error("Type name not registered");

  return type_registry.at(type_name);
}

auto SymbolTable::detailsOfType(const StrictType &type) -> TypeDetails {
  return detailsOfType(type.type_name);
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
  if (locals.back().empty())
    return;

  std::cout << "\nScope:\n{\n" << std::endl;
  for (auto &[name, var] : locals.back()) {
    printType(var.type);
    std::cout << " " << name << ";\n";
  }
  std::cout << "\n}" << std::endl;
}
