#pragma once
#include "../grammar/types.hpp"
#include <unordered_map>
#include <variant>
#include <vector>

struct SymbolTable {
  struct Function {
    std::string return_type;
    std::vector<Types> parameter_types;

    Function(std::string &&_return_type, std::vector<Types> &&_param_types)
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

    explicit FunctionSignature(Function &&f) noexcept {
      functions.emplace_back(std::move(f));
    }
    FunctionSignature(FunctionSignature &&other) noexcept
        : functions(std::move(other.functions)) {}

    void addFunction(std::string &&return_type,
                     std::vector<Types> &&param_types);

    [[nodiscard]] std::string
    returnTypeOfCall(const std::vector<Types> &provided_param) const;

    void print() const;
  };

  struct Variable {
    Types type;
    bool is_mutable;
    bool is_stolen{false};

    Variable(const Variable &) = default;

    Variable(Types &&_t, const bool _mut)
        : type(std::move(_t)), is_mutable(_mut) {}

    Variable(Variable &&other) noexcept
        : type(std::move(other.type)), is_mutable(other.is_mutable),
          is_stolen(other.is_stolen) {}
  };

  struct TypeDetails {
    bool arithmetic;
    bool callable;
    bool array;
  };

  struct LocalScope {
    explicit LocalScope(std::string ret_type)
        : expected_return_type(std::move(ret_type)) {}
    std::unordered_map<std::string, Variable> variables;
    std::string expected_return_type;
  };

  using GlobalEntry = std::variant<Variable, FunctionSignature>;
  std::unordered_map<std::string, GlobalEntry> globals;
  std::vector<LocalScope> locals;
  std::unordered_map<std::string, TypeDetails> type_registry;
  std::vector<std::string> expected_return_types;

  SymbolTable();

  bool containsVariable(const std::string &name) const;

  void addGlobalVariable(std::string name, Types type, bool is_mutable = false);

  void addLocalVariable(std::string name, Types type, bool is_mutable = false);

  const Variable &closestVariable(const std::string &name) const;

  Types typeOfClosestVariable(const std::string &name) const;

  void enterScope(std::string expected_return_type = "devoid");

  void leaveScope();

  std::string returnTypeOfCurrentScope() const;

  void addFunction(const std::string &name, std::string type,
                   std::vector<Types> &&parameter_types);

  bool containsFunction(const std::string &name) const;

  std::string returnTypeOfCall(const std::string &name,
                               const std::vector<Types> &provided_params) const;

  bool isSymbolInCurrentScope(const std::string &name) const;

  bool isAssignable(const std::string &var_name) const;

  TypeDetails detailsOfType(const std::string &type_name) const;

  void clearLocalTable();

  void printGlobals();

  void printLocals();
};