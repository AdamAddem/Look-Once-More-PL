#pragma once
#include "lex.hpp"
#include "statements.hpp"
#include <cassert>
namespace Parser {

struct ParsedFunction {
  StrictType return_value;
  std::string name;
  std::vector<VarDeclaration>
      parameter_list; // VarDeclarations should have expr = nullptr
  std::vector<Statement *> function_body;

  ParsedFunction(StrictType _return_value, std::string &&_name,
                 std::vector<VarDeclaration> &&_parameter_list,
                 std::vector<Statement *> &&_function_body)
      : return_value(_return_value), name(std::move(_name)),
        parameter_list(std::move(_parameter_list)),
        function_body(std::move(_function_body)) {}
};

struct ParsedTranslationUnit {
  std::vector<VarDeclaration> globals;
  std::vector<ParsedFunction> functions;

  ParsedTranslationUnit(std::vector<VarDeclaration> &&_globals,
                        std::vector<ParsedFunction> &&_functions)
      : globals(std::move(_globals)), functions(std::move(_functions)) {}
};

void beginParsing(std::vector<Lexer::Token> &&token_list);
} // namespace Parser
