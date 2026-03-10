#pragma once
#include "../ast/types.hpp"

#include <stdexcept>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

struct SymbolTable {
  struct FunctionSignature {
    std::vector<AST::Type> function_types;

    constexpr explicit FunctionSignature(AST::Types&& initial_parameter_types, AST::Type&& initial_return_type);

    void addFunction(AST::Types &&parameter_types, AST::Type&& return_type);

    [[nodiscard]] const AST::Type& returnTypeOfCall(const AST::Types &provided_param) const;
  };

  struct TypeInstance {
    AST::Type type;
    bool is_mutable{false};
    bool is_stolen{false};
  };

  struct LocalScope {
    explicit LocalScope(AST::Type ret_type)
        : expected_return_type(std::move(ret_type)) {}

    std::unordered_map<std::string, TypeInstance> variables;
    AST::Type expected_return_type;
  };

  using Symbol = std::variant<TypeInstance, FunctionSignature>;
  std::unordered_map<std::string, Symbol> globals;
  std::vector<LocalScope> locals;
  std::unordered_set<std::string> type_registry;

  SymbolTable();
  SymbolTable(SymbolTable&& other) noexcept :
  globals(std::move(other.globals)), locals(std::move(other.locals)), type_registry(std::move(other.type_registry)) {}

  bool containsVariable(const std::string &name) const;
  void addGlobalVariable(std::string name, AST::Type type);
  void addLocalVariable(std::string name, AST::Type type);
  const TypeInstance& closestVariable(const std::string &name) const;
  const AST::Type& closestVariableType(const std::string &name) const;
  bool isVarMutable(const std::string &var_name) const;

  bool containsFunction(const std::string &name) const;
  void addFunction(const std::string &name, AST::Type return_type, std::vector<AST::Type> &&parameter_types);
  const AST::Type& returnTypeOfCall(const std::string &name, const std::vector<AST::Type> &provided_params) const;

  bool containsSymbol(const std::string &name) const {return containsVariable(name) || containsFunction(name);}
  AST::Type closestSymbolType(const std::string &name) const {
    if (containsVariable(name))
      return closestVariableType(name);

    if (containsFunction(name))
      return AST::Type(AST::Type::normal, name, {});

    throw std::runtime_error("Symbol not found");
  }

  void enterScope(AST::Type expected_return_type);
  void leaveScope();
  const AST::Type& returnTypeOfCurrentScope() const;
  bool isSymbolInCurrentScope(const std::string &name) const;
  void clearLocalTable();



  [[nodiscard]] bool isRegisteredType(const std::string& type_name) const noexcept {return type_registry.contains(type_name);}
  [[nodiscard]] bool isRegisteredType(const AST::Type& t) const noexcept;


};