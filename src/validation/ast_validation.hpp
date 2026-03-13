#pragma once
#include "ast/statements.hpp"
#include "validation/symbol_table.hpp"

namespace Parser {
struct ParsedTU;
}

namespace Validation {

struct ValidatedFunction {
  const AST::Type* return_type;
  std::string name;
  std::vector<AST::VarDeclaration> parameter_list; // VarDeclarations should have expr = nullptr
  std::vector<AST::Statement*> function_body;

  ValidatedFunction(const AST::Type* return_type, std::string&& name,
                 std::vector<AST::VarDeclaration>&& parameter_list,
                 std::vector<AST::Statement*>&& function_body) noexcept
  : return_type(return_type), name(std::move(name)), parameter_list(std::move(parameter_list)), function_body(std::move(function_body)) {}

  ValidatedFunction(ValidatedFunction &&other) noexcept
      : return_type(other.return_type), name(std::move(other.name)),
        parameter_list(std::move(other.parameter_list)),
        function_body(std::move(other.function_body)) {}

  ~ValidatedFunction();
};

struct ValidatedTU {
  SymbolTable table;
  std::vector<AST::VarDeclaration> globals;
  std::vector<ValidatedFunction> functions;

  ValidatedTU(SymbolTable&& table,
              std::vector<AST::VarDeclaration>&& globals,
              std::vector<ValidatedFunction>&& functions) noexcept
  : table(std::move(table)), globals(std::move(globals)), functions(std::move(functions)) {}

};


ValidatedTU validateTU(Parser::ParsedTU &&);

};