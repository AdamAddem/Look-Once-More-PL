#include "symbol_table.hpp"

#include <cassert>
#include <iostream>
#include <stdexcept>

using namespace AST;

SymbolTable::~SymbolTable() {
  for (const auto ptr : pointers)
    delete ptr;

  for (const auto variant : variants)
    delete variant;

  for (const auto function : functions)
    delete function;
}



const Type* SymbolTable::addPointer(const Pointer::Pointers ptr_type, const Type* pointed_type, const bool is_pointed_mutable) noexcept {
  assert(ptr_type != Pointer::Pointers::VAGUE);
  for (const auto ptr : pointers) {
    if (ptr->sameAs(ptr_type, pointed_type, is_pointed_mutable))
      return ptr;
  }

  const auto new_pointer = new Pointer(ptr_type, pointed_type, is_pointed_mutable);
  pointers.push_back(new_pointer);
  return new_pointer;
}

const Type* SymbolTable::addVariant(std::vector<const Type*> subtypes, const bool nullable) noexcept {
  for (const auto variant : variants)
    if (variant->sameAs(subtypes, nullable))
      return variant;

  const auto new_variant = new Variant(std::move(subtypes), nullable);
  variants.push_back(new_variant);
  return new_variant;
}

auto SymbolTable::FunctionSignature::returnTypeOfCall(const std::vector<InstantiatedType>& parameters)
const -> std::expected<const Type*, CallError> {
  int valid = -1;
  for (auto i{0uz}; i<function_types.size(); ++i) {
    if (function_types[i]->isValidCall(parameters)) {
      if (valid != -1)
        return std::unexpected(CallError::AMBIGUOUS_OVERLOAD);

      valid = i;
    }
  }

  if (valid == -1)
    return std::unexpected(CallError::NO_SUITABLE_FUNCTION);

  return function_types[valid]->returnType();
}

bool SymbolTable::containsVariable(const std::string &name) const {
  for (const auto &scope : locals) // order doesn't matter
    if (scope.variables.contains(name))
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

void SymbolTable::addLocalVariable(std::string name, InstantiatedType type) {
  if (locals.back().variables.contains(name))
    throw std::runtime_error("Redefinition of local symbol with name: " + name);

  locals.back().variables.emplace(std::move(name), type);
}

InstantiatedType SymbolTable::closestVariable(const std::string &name) const {
  const auto end = locals.rend();
  for (auto curr = locals.rbegin(); curr != end; ++curr)
    if (curr->variables.contains(name))
      return curr->variables.at(name);

  if (globals.contains(name)) {
    assert(std::holds_alternative<InstantiatedType>(globals.at(name)));
    return std::get<InstantiatedType>(globals.at(name));
  }
  throw std::runtime_error("No variable found in typeOfMatchingVariable method");
}

void SymbolTable::addFunction(const std::string &name, const std::vector<const Type*>& parameters, const Type* return_type) {
  if (globals.contains(name)) {
    if (std::holds_alternative<InstantiatedType>(globals.at(name)))
      throw std::runtime_error("Redefinition of global variable with name: " + name);
  } else
    globals.emplace(name, FunctionSignature{});



  auto& signature = std::get<FunctionSignature>(globals.at(name));
  for (const auto f : functions) {
    if (f->sameAs(parameters, return_type))
      return signature.function_types.push_back(f);
  }

  signature.function_types.push_back(new Function(parameters, return_type));
}

bool SymbolTable::containsFunction(const std::string &name) const noexcept {
  if (globals.contains(name))
    return std::holds_alternative<FunctionSignature>(globals.at(name));
  return false;
}

auto SymbolTable::returnTypeOfCall(const std::string &name, const std::vector<InstantiatedType>& provided_params)
const -> std::expected<const Type*, CallError> {
  if (!containsFunction(name))
    throw std::runtime_error("Identifier not recognized as callable");


  return std::get<FunctionSignature>(globals.at(name))
  .returnTypeOfCall(provided_params);
}

bool SymbolTable::isSymbolInCurrentScope(const std::string &name) const noexcept {
  if (locals.empty())
    return globals.contains(name);

  return locals.back().variables.contains(name);
}

bool SymbolTable::isVarMutable(const std::string &var_name) const {
  assert(containsVariable(var_name));

  return closestVariable(var_name).details.is_mutable;
}



