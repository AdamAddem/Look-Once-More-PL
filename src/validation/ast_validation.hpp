#pragma once
#include "ast/statements.hpp"
#include "validation/symbol_table.hpp"

namespace Parser {
struct ParsedTU;
}

namespace Validation {

struct ValidatedFunction {
  AST::Type return_type;
  std::string name;
  std::vector<AST::VarDeclaration> parameter_list; // VarDeclarations should have expr = nullptr
  std::vector<AST::Statement *> function_body; // owned

  //takes ownership of pointers
  ValidatedFunction(AST::Type &&_return_type, std::string &&_name,
                 std::vector<AST::VarDeclaration> &&_parameter_list,
                 std::vector<AST::Statement *> &&_function_body) noexcept
  : return_type(std::move(_return_type)), name(std::move(_name)), parameter_list(std::move(_parameter_list)), function_body(std::move(_function_body)) {}

  ValidatedFunction(ValidatedFunction &&other) noexcept
      : return_type(std::move(other.return_type)), name(std::move(other.name)),
        parameter_list(std::move(other.parameter_list)),
        function_body(std::move(other.function_body)) {}

  ~ValidatedFunction();
};

struct ValidatedTU {
  std::vector<AST::VarDeclaration> globals;
  std::vector<ValidatedFunction> functions;
  SymbolTable table;

  ValidatedTU(std::vector<AST::VarDeclaration> &&_globals,
              std::vector<ValidatedFunction> &&_functions,
              SymbolTable&& _table) noexcept
  : globals(std::move(_globals)), functions(std::move(_functions)), table(std::move(_table)) {}

  ValidatedTU(ValidatedTU&& other) noexcept
  : globals(std::move(other.globals)), functions(std::move(other.functions)), table(std::move(other.table)) {}

};


ValidatedTU validateTU(Parser::ParsedTU &&);

};