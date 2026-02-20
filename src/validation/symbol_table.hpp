#pragma once
#include "../grammar/types.hpp"
#include <unordered_map>
#include <variant>
#include <vector>

struct SymbolTable {
  struct Function {
    Type return_type;
    std::vector<Type> parameter_types;

    Function(Type &&_return_type, std::vector<Type> &&_param_types)
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

    void addFunction(Type &&return_type,
                     std::vector<Type> &&param_types);

    [[nodiscard]] Type
    returnTypeOfCall(const std::vector<Type> &provided_param) const;

    void print() const;
  };

  struct Variable {
    Type type;
    bool is_stolen{false};

    Variable(const Variable &) = default;

    Variable(Type &&_t) : type(std::move(_t)) {}

    Variable(Variable &&other) noexcept
        : type(std::move(other.type)), is_stolen(other.is_stolen) {}
  };

  struct TypeDetails {
    bool arithmetic;
    bool callable;
    bool array;
  };

  struct LocalScope {
    explicit LocalScope(Type ret_type)
        : expected_return_type(std::move(ret_type)) {}

    std::unordered_map<std::string, Variable> variables;
    Type expected_return_type;
  };

  using Symbol = std::variant<Variable, FunctionSignature>;
  std::unordered_map<std::string, Symbol> globals;
  std::vector<LocalScope> locals;
  std::unordered_map<std::string, TypeDetails> type_registry;
  std::vector<Type> expected_return_types;

  SymbolTable();

  bool containsVariable(const std::string &name) const;
  void addGlobalVariable(std::string name, Type type);
  void addLocalVariable(std::string name, Type type);
  const Variable &closestVariable(const std::string &name) const;
  Type closestVariableType(const std::string &name) const;
  bool isVarMutable(const std::string &var_name) const;

  bool containsFunction(const std::string &name) const;
  void addFunction(const std::string &name, Type return_type, std::vector<Type> &&parameter_types);
  Type returnTypeOfCall(const std::string &name, const std::vector<Type> &provided_params) const;

  bool containsSymbol(const std::string &name) const {return containsVariable(name) || containsFunction(name);}
  Type closestSymbolType(const std::string &name) const { //revisit this sack of shit eventually
    if (containsVariable(name))
      return closestVariableType(name);

    if (containsFunction(name))
      return Type(Type::function, name);

    throw std::runtime_error("Symbol not found");
  }

  void enterScope(Type expected_return_type = devoid_type);
  void leaveScope();
  Type returnTypeOfCurrentScope() const;
  bool isSymbolInCurrentScope(const std::string &name) const;
  void clearLocalTable();



  TypeDetails detailsOfType(const std::string &type_name) const;
  [[nodiscard]] bool isRegisteredType(const std::string& type_name) const noexcept {return type_registry.contains(type_name);}
  [[nodiscard]] bool isRegisteredType(const Type& t) const noexcept;


  void printGlobals();
  void printLocals();
};