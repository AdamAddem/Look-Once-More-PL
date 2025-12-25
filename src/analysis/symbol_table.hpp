#pragma once
#include "../grammar/types.hpp"
#include <unordered_map>
#include <vector>

struct Function {
  Type return_type;
  std::vector<Type> parameter_types;
  Function(Type &&_t, std::vector<Type> &&_params)
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

  FunctionSignature(Function &&f) { functions.emplace_back(std::move(f)); }
  FunctionSignature(FunctionSignature &&other)
      : functions(std::move(other.functions)) {}

  void addFunction(Type &&t, std::vector<Type> &&params);

  bool isValidCall(const Type &capture_type,
                   const std::vector<Type> &provided_param);

  void print();
};

struct Variable {
  Type type;
  bool is_const;
  bool is_stolen{false};

  Variable(const Variable &) = default;
  Variable(Type &&_t, bool _const) : type(std::move(_t)), is_const(_const) {}
  Variable(Variable &&other) noexcept
      : type(std::move(other.type)), is_const(other.is_const),
        is_stolen(other.is_stolen) {}
};

struct SymbolTable {
  using Entry = std::variant<Variable, FunctionSignature>;

  std::unordered_map<std::string, Entry> globals;
  std::vector<std::unordered_map<std::string, Variable>> locals;

  bool containsVariable(const std::string &name);
  void addGlobalVariable(std::string name, Type type, bool is_const = false);
  void addLocalVariable(std::string name, Type type, bool is_const = false);
  void enterScope();
  void leaveScope();

  void addFunction(std::string name, Type type,
                   std::vector<Type> &&parameter_types);
  bool containsFunction(const std::string &name);
  bool isValidCall(const std::string &name, const Type &capture_type,
                   const std::vector<Type> &provided_params);

  void clearLocalTable();

  void printGlobals();
  void printLocals();
};

struct TypeDetails {
  bool arithmetic;
};

struct TypeRegistry {
  std::unordered_map<std::string, TypeDetails> registry;

  TypeRegistry();

  void addType(const std::string &type_name, TypeDetails details);
  bool isValidType(const std::string &type_name);
  bool isArithmeticType(const std::string &type_name);
};
