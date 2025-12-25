#include "symbol_table.hpp"
#include <iostream>
#include <stdexcept>

void Function::print() {
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

void FunctionSignature::addFunction(Type &&t, std::vector<Type> &&params) {
  functions.emplace_back(std::move(t), std::move(params));
}

bool FunctionSignature::isValidCall(const Type &capture_type,
                                    const std::vector<Type> &provided_param) {
  bool is_valid{false};
  for (const auto &f : functions) {

    if (!convertibleFromTo(capture_type, f.return_type))
      continue;

    if (provided_param.size() != f.parameter_types.size())
      continue;

    for (auto i{0uz}; i < provided_param.size(); ++i) {
      if (!convertibleFromTo(provided_param[i], f.parameter_types[i]))
        goto escape;
    }

    if (is_valid)
      throw std::runtime_error("Ambiguous function overload");

    is_valid = true;

  escape:
  }

  return is_valid;
}

void FunctionSignature::print() {
  for (auto &f : functions) {
    f.print();
    std::cout << ", ";
  }
  std::cout << "\b\b";
}

bool SymbolTable::containsVariable(const std::string &name) {

  for (auto m : locals) // order doesn't matter
    if (m.contains(name))
      return true;

  if (globals.contains(name))
    return std::holds_alternative<Variable>(globals.at(name));
  return false;
}

void SymbolTable::addGlobalVariable(std::string name, Type type,
                                    bool is_const) {

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

void SymbolTable::enterScope() { locals.emplace_back(); }
void SymbolTable::leaveScope() {
  printLocals();
  locals.pop_back();
}

void SymbolTable::addFunction(std::string name, Type type,
                              std::vector<Type> &&parameter_types) {
  if (globals.contains(name)) {
    if (std::holds_alternative<Variable>(globals.at(name)))
      throw std::runtime_error("Redefinition of global variable with name: " +
                               name);
  } else {
    globals.emplace(name, FunctionSignature(Function(
                              std::move(type), std::move(parameter_types))));
    return;
  }

  std::get<FunctionSignature>(globals.at(name))
      .addFunction(std::move(type), std::move(parameter_types));
}

bool SymbolTable::containsFunction(const std::string &name) {
  if (globals.contains(name))
    return std::holds_alternative<FunctionSignature>(globals.at(name));
  return false;
}

bool SymbolTable::isValidCall(const std::string &name, const Type &capture_type,
                              const std::vector<Type> &provided_params) {
  if (!containsFunction(name))
    return false;

  return std::get<FunctionSignature>(globals.at(name))
      .isValidCall(capture_type, provided_params);
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
  std::cout << "\nScope:\n{\n" << std::endl;
  for (auto &[name, var] : locals.back()) {
    printType(var.type);
    std::cout << " " << name << ";\n";
  }
  std::cout << "\n}" << std::endl;

  // for (auto &scope : locals) {
  //   std::cout << "\nScope:\n{\n" << std::endl;
  //   for (auto &[name, var] : scope) {
  //     printType(var.type);
  //     std::cout << " " << name << ";\n";
  //   }
  //   std::cout << "\n}" << std::endl;
  // }
}

TypeRegistry::TypeRegistry() {
  registry["int"] = {true};
  registry["uint"] = {true};
  registry["float"] = {true};
  registry["double"] = {true};
  registry["char"] = {true};
  registry["uchar"] = {true};
  registry["bool"] = {true};
  registry["string"] = {false};
  registry["short"] = {true};
  registry["long"] = {true};
  registry["signed"] = {true};
  registry["unsigned"] = {true};
}

void TypeRegistry::addType(const std::string &type_name, TypeDetails details) {
  if (registry.contains(type_name))
    throw std::runtime_error("Redefinition of registered type_name");

  registry[type_name] = details;
}

bool TypeRegistry::isValidType(const std::string &type_name) {
  return registry.contains(type_name);
}

bool TypeRegistry::isArithmeticType(const std::string &type_name) {
  if (!isValidType(type_name))
    throw std::runtime_error("Invalid typename in isArithmetic check");

  return registry[type_name].arithmetic;
}
