#pragma once
#include "types.hpp"
#include "utilities/arena.hpp"
#include "utilities/typedefs.hpp"

#include <unordered_map>
#include <variant>
#include <vector>



namespace LOM {


class SymbolTable final {
public:
  struct Variable {
    InstantiatedType type;
    std::string name;
  };
private:
  class Function {
    //each num is the number of variables in each scope
    //first scope is for parameters
    std::vector<u16_t> scopes;
    std::vector<Variable> locals;
    const FunctionType* type;
  public:

    explicit Function(std::span<Variable> parameters, const FunctionType* type) : type(type) {
      scopes.emplace_back(parameters.size());
      scopes.emplace_back(0);
      for (const auto& var : parameters)
        locals.emplace_back(var);
    }

    [[nodiscard]] auto
    parameters() const noexcept
    {return std::span(locals).subspan(0, scopes.front());}
    [[nodiscard]] const Type*
    returnType() const noexcept {return type->returnType();}

    void addScope() noexcept {assert(not scopes.empty()); scopes.emplace_back(0);}
    void leaveScope() noexcept {
      assert(scopes.size() > 1);
      for (auto i{scopes.back() + 1}; i > 0; --i)
        locals.pop_back();
      scopes.pop_back();
    }

    [[nodiscard]] bool
    containsVariable(const std::string& name) const noexcept {
      auto sz = static_cast<i64_t>(locals.size());
      while (sz-- > 0)
        if (locals[sz].name == name)
          return true;
      return false;
    }

    [[nodiscard]] bool
    addVariable(std::string name, InstantiatedType type) noexcept {
      assert(scopes.size() > 1);
      if (containsVariable(name))
          return false;

      ++scopes.back();
      locals.emplace_back(type, std::move(name));
      return true;
    }

    [[nodiscard]] InstantiatedType
    getVariable(const std::string& name) const noexcept {
      assert(containsVariable(name));
      auto sz = static_cast<i64_t>(locals.size());
      while (sz-- > 0)
        if (locals[sz].name == name)
          return locals[sz].type;

      assert(false);
    }
  };

  using Symbol = std::variant<InstantiatedType, Function>;
  std::unordered_map<std::string, Symbol> globals;
  Function* current_scope{nullptr};

  TypeContext types;
public:

  SymbolTable() = default;
  SymbolTable(SymbolTable&& other) noexcept = default;

  void addFunction(
    const std::string &name,
    std::span<Variable> parameters,
    const Type* return_type) noexcept;

  [[nodiscard]] const Type*
  addRawPointer(InstantiatedType subtype) noexcept
  {return types.addRawPointer(subtype);}

  [[nodiscard]] const Type*
  addUniquePointer(InstantiatedType subtype) noexcept
  {return types.addUniquePointer(subtype);}

  [[nodiscard]] const Type*
  addVariant(std::vector<const Type*> subtypes, bool nullable) noexcept
  {return types.addVariant(std::move(subtypes), nullable);}

  void addVariable(std::string name, InstantiatedType type) {
    if (current_scope)
      return addLocalVariable(std::move(name), type);

    return addGlobalVariable(std::move(name), type);
  }
  void addGlobalVariable(std::string name, InstantiatedType type);
  void addLocalVariable(std::string name, InstantiatedType type) const noexcept;

  [[nodiscard]] bool
  containsVariable(const std::string &name) const noexcept;

  [[nodiscard]] InstantiatedType
  closestVariable(const std::string &name) noexcept;

  [[nodiscard]] bool
  containsFunction(const std::string &name) noexcept {
    if (globals.contains(name))
      return std::holds_alternative<Function>(globals[name]);
    return false;
  }

  [[nodiscard]] auto
  parametersOfFunction(const std::string& name) noexcept {
    assert(containsFunction(name));
    return std::get<Function>(globals[name]).parameters();
  }

  [[nodiscard]] const Type*
  returnTypeOfFunction(const std::string& name) noexcept {
    assert(containsFunction(name));
    return std::get<Function>(globals[name]).returnType();
  }

  [[nodiscard]] bool
  containsSymbol(const std::string &name) noexcept {return containsVariable(name) || containsFunction(name);}

  [[nodiscard]] bool
  isClosestSymbolAVariable(const std::string &name) noexcept {
    if (containsVariable(name))
      return true;

    assert(containsFunction(name));
    return false;
  }

  void enterFunctionScope(const std::string& function_name) noexcept {
    assert(current_scope == nullptr);
    assert(containsFunction(function_name));
    current_scope = &std::get<Function>(globals[function_name]);
  }
  void enterLocalScope() const noexcept {
    assert(current_scope);
    current_scope->addScope();
  }
  void leaveFunctionScope() noexcept {
    assert(current_scope);
    current_scope = nullptr;
  }
  void exitLocalScope() const noexcept {
    assert(current_scope);
    current_scope->leaveScope();
  }

  [[nodiscard]] const Type*
  returnTypeOfCurrentFunction() const noexcept {
    assert(current_scope);
    return current_scope->returnType();
  }
};

}