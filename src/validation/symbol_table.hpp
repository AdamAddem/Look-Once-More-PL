#pragma once
#include "ast/types.hpp"
#include <expected>
#include <stdexcept>
#include <unordered_map>
#include <variant>
#include <vector>

class SymbolTable final {
public:
    enum class CallError : bool {NO_SUITABLE_FUNCTION, AMBIGUOUS_OVERLOAD};
private:
  struct FunctionSignature {
    std::vector<const AST::Function*> function_types;
    constexpr explicit FunctionSignature() = default;

    [[nodiscard]] std::expected<const AST::Type*, CallError>
    returnTypeOfCall(const std::vector<AST::InstantiatedType> &parameters) const;
  };

  struct LocalScope {
    constexpr explicit LocalScope(const AST::Type* scope_return_type)
    : expected_return_type(scope_return_type) {}

    std::unordered_map<std::string, AST::InstantiatedType> variables;
    const AST::Type* expected_return_type;
  };

  using Symbol = std::variant<AST::InstantiatedType, FunctionSignature>;
  std::unordered_map<std::string, Symbol> globals;
  std::vector<LocalScope> locals;

  std::vector<AST::Pointer*> pointers;
  std::vector<AST::Variant*> variants;
  std::vector<AST::Function*> functions;
  //std::unordered_map<std::string, AST::Custom> custom_typeregistry;

  const AST::Type* addPointer(AST::Pointer::Pointers ptr_type, const AST::Type* pointed_type, bool is_pointed_mutable) noexcept;
public:

  SymbolTable() = default;
  ~SymbolTable();

  void addFunction(const std::string &name, const std::vector<const AST::Type*> &parameters, const AST::Type* return_type);

  const AST::Type* addRawPointer(const AST::Type* subtype, const bool is_subtype_mutable) noexcept
  {return addPointer(AST::Pointer::Pointers::RAW, subtype, is_subtype_mutable);}
  const AST::Type* addUniquePointer(const AST::Type* subtype, const bool is_subtype_mutable) noexcept
  {return addPointer(AST::Pointer::Pointers::UNIQUE, subtype, is_subtype_mutable);}

  const AST::Type* addVariant(std::vector<const AST::Type*> subtypes, bool nullable) noexcept;

  void addGlobalVariable(std::string name, AST::InstantiatedType type);
  void addLocalVariable(std::string name, AST::InstantiatedType type);
  [[nodiscard]]bool containsVariable(const std::string &name) const;
  [[nodiscard]] AST::InstantiatedType closestVariable(const std::string &name) const;
  [[nodiscard]] bool isVarMutable(const std::string &var_name) const;

  [[nodiscard]] bool containsFunction(const std::string &name) const noexcept;
  [[nodiscard]] std::expected<const AST::Type*, CallError>
  returnTypeOfCall(const std::string &name, const std::vector<AST::InstantiatedType> &provided_params) const;

  [[nodiscard]] bool containsSymbol(const std::string &name) const {return containsVariable(name) || containsFunction(name);}
  [[nodiscard]] bool isClosestSymbolAVariable(const std::string &name) const {
    if (containsVariable(name))
      return true;

    if (containsFunction(name))
      return false;

    throw std::runtime_error("Symbol not found");
  }
  [[nodiscard]] AST::InstantiatedType closestSymbol(const std::string &name) const {
    if (containsVariable(name))
      return closestVariable(name);

    if (containsFunction(name))
      return {nullptr, {}};

    throw std::runtime_error("Symbol not found");
  }

  void enterScope(const AST::Type* expected_return_type) noexcept {locals.emplace_back(expected_return_type);}
  void leaveScope() noexcept {locals.pop_back();}
  [[nodiscard]] const AST::Type* returnTypeOfCurrentScope() const noexcept {
    assert(!locals.empty());
    return locals.back().expected_return_type;
  }
  [[nodiscard]] bool isSymbolInCurrentScope(const std::string &name) const noexcept;
  void clearLocalTable() noexcept {locals.clear();}
};