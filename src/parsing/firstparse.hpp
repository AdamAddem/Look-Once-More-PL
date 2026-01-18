#pragma once
#include "../grammar/statements.hpp"
#include "../lexing/lex.hpp"
#include <cassert>

namespace Parser {

struct UnparsedGlobals {
  std::vector<VarDeclaration> declarations;
  Lexer::TokenHandler global_init_body;

  UnparsedGlobals() = default;

  void print();
};

struct UnparsedFunction {
  StrictType return_value;
  std::string name;
  std::vector<VarDeclaration> parameter_list;
  Lexer::TokenHandler body_tokens;

  UnparsedFunction(StrictType &&_return_value, std::string &&_name,
                   std::vector<VarDeclaration> &&_parameter_list,
                   Lexer::TokenHandler &&_body_tokens)
      : return_value(std::move(_return_value)), name(std::move(_name)),
        parameter_list(std::move(_parameter_list)),
        body_tokens(std::move(_body_tokens)) {}

  void print();
};

struct UnparsedTU {
  UnparsedGlobals globals;
  bool globalsDeclared = false;
  bool parsedGlobalBody = false;
  std::vector<UnparsedFunction> functions;

  void registerGlobal(Type &&_type, std::string &&_name);
  void registerGlobalsFuncBody(Lexer::TokenHandler &&_body);
  void registerFunction(StrictType _type, std::string _name,
                        std::vector<VarDeclaration> _decl,
                        Lexer::TokenHandler _body);
  void print();

  UnparsedTU() = default;
};

[[nodiscard]] std::string parseIdentifier(Lexer::TokenHandler &tokens);
[[nodiscard]] Type parseType(Lexer::TokenHandler &tokens);
[[nodiscard]] UnparsedTU firstPassParsing(Lexer::TokenHandler &&tokens);
} // namespace Parser
