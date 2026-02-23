#pragma once
#include "../grammar/statements.hpp"

namespace Lexer {
  struct TokenHandler;
}

namespace Parser {

struct ParsedFunction {
  Type return_type;
  std::string name;
  std::vector<VarDeclaration>
      parameter_list; // VarDeclarations should have expr = nullptr
  std::vector<Statement *> function_body;

  ParsedFunction(Type &&_return_type, std::string &&_name,
                 std::vector<VarDeclaration> &&_parameter_list,
                 std::vector<Statement *> &&_function_body)
      : return_type(std::move(_return_type)), name(std::move(_name)),
        parameter_list(std::move(_parameter_list)),
        function_body(std::move(_function_body)) {}

  ParsedFunction(ParsedFunction &&other) noexcept
      : return_type(std::move(other.return_type)), name(std::move(other.name)),
        parameter_list(std::move(other.parameter_list)),
        function_body(std::move(other.function_body)) {}

};

struct ParsedTranslationUnit {
  std::vector<VarDeclaration> globals;
  std::vector<ParsedFunction> functions;

  ParsedTranslationUnit(std::vector<VarDeclaration> &&_global,
                        std::vector<ParsedFunction> &&_functions)
      : globals(std::move(_global)), functions(std::move(_functions)) {}

};


[[nodiscard]] ParsedTranslationUnit parseTokens(Lexer::TokenHandler &&tokens);
} // namespace Parser