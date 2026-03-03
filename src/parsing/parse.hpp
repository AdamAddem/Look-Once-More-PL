#pragma once
#include "../ast/statements.hpp"

namespace Lexer {
  struct TokenHandler;
}

namespace Parser {

struct ParsedFunction {
  AST::Type return_type;
  std::string name;
  std::vector<AST::VarDeclaration>
      parameter_list; // VarDeclarations should have expr = nullptr
  std::vector<AST::Statement *> function_body;

  ParsedFunction(AST::Type &&_return_type, std::string &&_name,
                 std::vector<AST::VarDeclaration> &&_parameter_list,
                 std::vector<AST::Statement *> &&_function_body) noexcept
      : return_type(std::move(_return_type)), name(std::move(_name)),
        parameter_list(std::move(_parameter_list)),
        function_body(std::move(_function_body)) {}

  ParsedFunction(ParsedFunction &&other) noexcept
      : return_type(std::move(other.return_type)), name(std::move(other.name)),
        parameter_list(std::move(other.parameter_list)),
        function_body(std::move(other.function_body)) {}

};

struct ParsedTU {
  std::vector<AST::VarDeclaration> globals;
  std::vector<ParsedFunction> functions;

  ParsedTU(std::vector<AST::VarDeclaration> &&_global,
                        std::vector<ParsedFunction> &&_functions)
      : globals(std::move(_global)), functions(std::move(_functions)) {}

};


[[nodiscard]] ParsedTU parseTokens(Lexer::TokenHandler &&tokens);
} // namespace Parser