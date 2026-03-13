#pragma once
#include "../ast/statements.hpp"

namespace Lexer {
  struct Token;
}


namespace Parser {

struct ParsedFunction {
  const AST::Type* return_type;
  std::string name;
  std::vector<AST::VarDeclaration> parameter_list; // VarDeclarations will have expr = nullptr
  std::vector<AST::Statement*> function_body;

  ParsedFunction(const AST::Type* _return_type, std::string &&_name,
                 std::vector<AST::VarDeclaration> &&_parameter_list,
                 std::vector<AST::Statement*> &&_function_body) noexcept
      : return_type(std::move(_return_type)), name(std::move(_name)),
        parameter_list(std::move(_parameter_list)),
        function_body(std::move(_function_body)) {}

  ParsedFunction(ParsedFunction &&other) noexcept
      : return_type(other.return_type), name(std::move(other.name)),
        parameter_list(std::move(other.parameter_list)),
        function_body(std::move(other.function_body)) {}

};

struct ParsedTU {
  SymbolTable table;
  std::vector<AST::VarDeclaration> globals;
  std::vector<ParsedFunction> functions;

  ParsedTU(SymbolTable&& table, std::vector<AST::VarDeclaration>&& global, std::vector<ParsedFunction>&& functions)
  : table(std::move(table)), globals(std::move(global)), functions(std::move(functions)) {}

};


[[nodiscard]] ParsedTU parseTokens(std::vector<Lexer::Token> &&tokens);
} // namespace Parser