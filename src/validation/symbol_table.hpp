#pragma once
#include "../ast/types.hpp"

#include <cstdint>
#include <stdexcept>
#include <unordered_map>
#include <variant>
#include <vector>

struct SymbolTable {
  struct Function {
    AST::Type return_type;
    std::vector<AST::Type> parameter_types;

    Function(AST::Type &&_return_type, std::vector<AST::Type> &&_param_types)
        : return_type(std::move(_return_type)),
          parameter_types(std::move(_param_types)) {}

    Function(Function &&other) noexcept
        : return_type(std::move(other.return_type)),
          parameter_types(std::move(other.parameter_types)) {}

    // return_types aren't involved in overloading
    bool operator==(const Function &other) const {
      return (parameter_types == other.parameter_types);
    }

    void print() const;
  };

  struct FunctionSignature {
    std::vector<Function> functions;

    explicit FunctionSignature(Function &&f) noexcept { functions.emplace_back(std::move(f)); }
    FunctionSignature(FunctionSignature &&other) noexcept
        : functions(std::move(other.functions)) {}

    void addFunction(AST::Type &&return_type,
                     std::vector<AST::Type> &&param_types);

    [[nodiscard]] AST::Type
    returnTypeOfCall(const std::vector<AST::Type> &provided_param) const;

    void print() const;
  };

  struct Variable {
    AST::Type type;
    bool is_stolen{false};

    Variable(const Variable &) = default;

    explicit Variable(AST::Type &&_t) : type(std::move(_t)) {}

    Variable(Variable &&other) noexcept
        : type(std::move(other.type)), is_stolen(other.is_stolen) {}
  };

  struct TypeDetails {
    bool arithmetic;
    bool callable;
    bool array;
    uint8_t alignment; //maybe too small? who knows lol
  };

  struct LocalScope {
    explicit LocalScope(AST::Type ret_type)
        : expected_return_type(std::move(ret_type)) {}

    std::unordered_map<std::string, Variable> variables;
    AST::Type expected_return_type;
  };

  using Symbol = std::variant<Variable, FunctionSignature>;
  std::unordered_map<std::string, Symbol> globals;
  std::vector<LocalScope> locals;
  std::unordered_map<std::string, TypeDetails> type_registry;
  std::vector<AST::Type> expected_return_types;

  SymbolTable();
  SymbolTable(SymbolTable&& other) noexcept :
  globals(std::move(other.globals)), locals(std::move(other.locals)),
  type_registry(std::move(other.type_registry)), expected_return_types(std::move(other.expected_return_types)) {}

  bool containsVariable(const std::string &name) const;
  void addGlobalVariable(std::string name, AST::Type type);
  void addLocalVariable(std::string name, AST::Type type);
  const Variable &closestVariable(const std::string &name) const;
  AST::Type closestVariableType(const std::string &name) const;
  bool isVarMutable(const std::string &var_name) const;

  bool containsFunction(const std::string &name) const;
  void addFunction(const std::string &name, AST::Type return_type, std::vector<AST::Type> &&parameter_types);
  AST::Type returnTypeOfCall(const std::string &name, const std::vector<AST::Type> &provided_params) const;

  bool containsSymbol(const std::string &name) const {return containsVariable(name) || containsFunction(name);}
  AST::Type closestSymbolType(const std::string &name) const { //revisit this sack of shit eventually
    if (containsVariable(name))
      return closestVariableType(name);

    if (containsFunction(name))
      return AST::Type(AST::Type::function, name);

    throw std::runtime_error("Symbol not found");
  }

  void enterScope(AST::Type expected_return_type = AST::devoid_type);
  void leaveScope();
  AST::Type returnTypeOfCurrentScope() const;
  bool isSymbolInCurrentScope(const std::string &name) const;
  void clearLocalTable();



  TypeDetails detailsOfType(const std::string &type_name) const;
  [[nodiscard]] bool isRegisteredType(const std::string& type_name) const noexcept {return type_registry.contains(type_name);}
  [[nodiscard]] bool isRegisteredType(const AST::Type& t) const noexcept;


  void printGlobals();
  void printLocals();
};