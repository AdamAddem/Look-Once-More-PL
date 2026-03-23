#include "symbol_table.hpp"

#include <cassert>
#include <stdexcept>

using namespace LOM;


const Type* SymbolTable::addPointer(const PointerType::Pointers ptr_type, const Type* pointed_type, const bool is_pointed_mutable) noexcept {
  assert(ptr_type != PointerType::Pointers::VAGUE);
  for (const auto ptr : pointers) {
    if (ptr->sameAs(ptr_type, pointed_type, is_pointed_mutable))
      return ptr;
  }

  const auto new_pointer = std::construct_at(
    type_allocator.allocate<PointerType>(), ptr_type, pointed_type, is_pointed_mutable);
  pointers.push_back(new_pointer);
  return new_pointer;
}

const Type* SymbolTable::addVariant(std::vector<const Type*> subtypes, const bool nullable) noexcept {
  for (const auto variant : variants)
    if (variant->sameAs(subtypes, nullable))
      return variant;

  const auto new_variant = std::construct_at(
    type_allocator.allocate<VariantType>(), std::move(subtypes), nullable);
  variants.push_back(new_variant);
  return new_variant;
}

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

void SymbolTable::addFunction( const std::string &name, std::span<Variable> parameters, const Type* return_type) noexcept {
  assert(not globals.contains(name));
  globals.emplace(name, Function{parameters, return_type});
}