#pragma once
#include "../grammar/types.hpp"
#include <unordered_map>
#include <vector>
#include <optional>


struct SymbolTable {
  struct Function {
    StrictType return_type;
    std::vector<Type> parameter_types;
    Function(StrictType &&_t, std::vector<Type> &&_params)
        : return_type(std::move(_t)), parameter_types(std::move(_params)) {}
    Function(Function &&other) noexcept
        : return_type(std::move(other.return_type)),
          parameter_types(std::move(other.parameter_types)) {}
    // return_types aren't involved in overloading
    bool operator==(const Function &other) const {
      return (parameter_types == other.parameter_types);
    }

    void print();
  };

  struct FunctionSignature {
    std::vector<Function> functions;

    explicit FunctionSignature(Function &&f) noexcept { functions.emplace_back(std::move(f)); }
    FunctionSignature(FunctionSignature &&other) noexcept : functions(std::move(other.functions)) {}

    void addFunction(StrictType &&t, std::vector<Type> &&params);

    StrictType returnTypeOfCall(const std::vector<Type> &provided_param) const;

    void print();
  };

  struct Variable {
    Type type;
    bool is_mutable;
    bool is_stolen{false};

    Variable(const Variable &) = default;
    Variable(Type &&_t, const bool _mut) : type(std::move(_t)), is_mutable(_mut) {}
    Variable(Variable &&other) noexcept
        : type(std::move(other.type)), is_mutable(other.is_mutable),
          is_stolen(other.is_stolen) {}
  };
  
  struct TypeDetails {
    bool arithmetic;
    bool callable;
  	bool array;
  };

  using GlobalEntry = std::variant<Variable, FunctionSignature>;
  using LocalScope = std::unordered_map<std::string, Variable>;
  std::unordered_map<std::string, GlobalEntry> globals;
  std::vector<LocalScope> locals;
  std::unordered_map<std::string, TypeDetails> type_registry;

  SymbolTable() = default;
  bool            containsVariable(const std::string &name) const;
  void            addGlobalVariable(std::string name, Type type, bool is_const = false);
  void            addLocalVariable(std::string name, Type type, bool is_const = false);
  const Variable &closestVariable(const std::string &name);
  void            enterScope();
  void            leaveScope();
  void            addFunction(std::string name, StrictType type, std::vector<Type> &&parameter_types);
  bool            containsFunction(const std::string &name) const;
  StrictType      returnTypeOfCall(const std::string &name, const std::vector<Type> &provided_params) const;
  bool            isSymbolInCurrentScope(const std::string &name) const;
  bool            isAssignable(const std::string &var_name);
  TypeDetails     detailsOfType(const std::string &type_name) const;
  TypeDetails     detailsOfType(const StrictType &type) const;



  void clearLocalTable();
  void printGlobals();
  void printLocals();
};
