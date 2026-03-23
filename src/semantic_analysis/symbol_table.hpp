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
  struct Function {
    u64_t num_parameters{};
    std::vector<Variable> locals;
    const Type* return_type;

    explicit Function(std::span<Variable> parameters, const Type* return_type) : return_type(return_type) {
      for (const auto& var : parameters) {
        ++num_parameters;
        locals.emplace_back(var);
      }
    }

    [[nodiscard]] auto
    getParameters() const noexcept { return std::span(locals).subspan(0, num_parameters); }

    [[nodiscard]] bool
    containsVariable(const std::string& name) const noexcept {
      for (const auto& var : locals) {
        if (var.name == name)
          return true;
      }
      return false;
    }

    [[nodiscard]] bool
    addVariable(std::string name, InstantiatedType type) noexcept {
      if (containsVariable(name))
          return false;

      locals.emplace_back(type, std::move(name));
      return true;
    }

    [[nodiscard]] InstantiatedType
    getVariable(const std::string& name) const noexcept {
      assert(containsVariable(name));
      for (const auto& var : locals) {
        if (var.name == name)
          return var.type;
      }
      assert(false);
    }
  };

  using Symbol = std::variant<InstantiatedType, Function>;
  std::unordered_map<std::string, Symbol> globals;
  Function* current_scope{nullptr};

  Arena<> type_allocator;
  std::vector<PointerType*> pointers;
  std::vector<VariantType*> variants;
  std::vector<FunctionType*> functions;

  const Type* addPointer(PointerType::Pointers ptr_type, const Type* pointed_type, bool is_pointed_mutable) noexcept;
public:

  SymbolTable() = default;
  SymbolTable(SymbolTable&& other) noexcept = default;
  Arena<> takeTypeAllocator() noexcept { return std::move(type_allocator); }

  void addFunction(
    const std::string &name,
    std::span<Variable> parameters,
    const Type* return_type) noexcept;

  const Type* addRawPointer(const Type* subtype, const bool is_subtype_mutable) noexcept
  {return addPointer(PointerType::Pointers::RAW, subtype, is_subtype_mutable);}
  const Type* addUniquePointer(const Type* subtype, const bool is_subtype_mutable) noexcept
  {return addPointer(PointerType::Pointers::UNIQUE, subtype, is_subtype_mutable);}

  const Type* addVariant(std::vector<const Type*> subtypes, bool nullable) noexcept;

  void addGlobalVariable(std::string name, InstantiatedType type);
  void addLocalVariable(std::string name, InstantiatedType type) const noexcept;
  [[nodiscard]] bool containsVariable(const std::string &name) const noexcept;
  [[nodiscard]] InstantiatedType closestVariable(const std::string &name) noexcept;

  [[nodiscard]] bool containsFunction(const std::string &name) noexcept {
    if (globals.contains(name))
      return std::holds_alternative<Function>(globals[name]);
    return false;
  }
  [[nodiscard]] auto
  parametersOfFunction(const std::string& name) noexcept {
    assert(containsFunction(name));
    return std::get<Function>(globals[name]).getParameters();
  }
  [[nodiscard]] const Type*
  returnTypeOfFunction(const std::string& name) noexcept {
    assert(containsFunction(name));
    return std::get<Function>(globals[name]).return_type;
  }
  [[nodiscard]] bool containsSymbol(const std::string &name) noexcept {return containsVariable(name) || containsFunction(name);}
  [[nodiscard]] bool isClosestSymbolAVariable(const std::string &name) noexcept {
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
  void leaveFunctionScope() noexcept {
    assert(current_scope not_eq nullptr);
    current_scope = nullptr;
  }
  [[nodiscard]] const Type* returnTypeOfCurrentFunction() const noexcept {
    assert(current_scope);
    return current_scope->return_type;
  }
};

}