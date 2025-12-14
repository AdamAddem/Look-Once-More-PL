#include "parse.hpp"
#include "lex.hpp"
#include <iostream>
#include <stdexcept>
using namespace Parser;
using TokenList = std::vector<Lexer::Token>;

struct UnparsedFunction {
  StrictType return_value;
  std::string name;
  std::vector<VarDeclaration> parameter_list;
  TokenList body_tokens;

  UnparsedFunction(StrictType &&_return_value, std::string &&_name,
                   std::vector<VarDeclaration> &&_parameter_list,
                   TokenList &&_body_tokens)
      : return_value(std::move(_return_value)), name(std::move(_name)),
        parameter_list(std::move(_parameter_list)),
        body_tokens(std::move(_body_tokens)) {}

  void print() {
    std::cout << return_value.type_name << " " << name << "(";
    for (auto &decl : parameter_list) {
      std::cout << std::get<StrictType>(decl.type).type_name << " "
                << decl.ident << ", ";
    }

    std::cout << ") {" << std::endl;

    for (auto &t : body_tokens) {
      std::cout << t.toString() << std::endl;
    }

    std::cout << "}" << std::endl;
  }
};

struct UnparsedTU {
  std::vector<VarDeclaration> globals;
  std::vector<UnparsedFunction> functions;

  UnparsedTU() = default;
};

std::string parseType(TokenList &token_list) {
  Lexer::Token t = std::move(token_list.back());
  token_list.pop_back();

  if (t.isPrimitive())
    return t.toString();

  if (t.type == Lexer::IDENTIFIER)
    return std::get<std::string>(t.value);

  throw std::runtime_error("Expected typename");
}

std::string parseIdentifier(TokenList &token_list) {
  Lexer::Token t = std::move(token_list.back());
  token_list.pop_back();

  if (t.type == Lexer::IDENTIFIER)
    return std::get<std::string>(t.value);

  throw std::runtime_error("Expected identifier");
}

std::vector<VarDeclaration> parseParameterDecl(TokenList &token_list) {
  std::vector<VarDeclaration> parameter_list;
  if (token_list.back().type == Lexer::RPAREN) {
    token_list.pop_back();
    return parameter_list;
  }

  while (true) {
    std::string type_name = parseType(token_list);
    std::string ident = parseIdentifier(token_list);

    parameter_list.emplace_back(
        VarDeclaration(StrictType(std::move(type_name)), std::move(ident)));

    if (token_list.back().type == Lexer::RPAREN) {
      token_list.pop_back();
      return parameter_list;
    }

    if (token_list.back().type == Lexer::COMMA)
      token_list.pop_back();
    else
      throw std::runtime_error(
          "Expected ending parenthesis or comma in parameter list");
  }
}

void parsePassOne(UnparsedTU &tu, TokenList &token_list) {

  std::string type_name = parseType(token_list);
  std::string ident = parseIdentifier(token_list);

  Lexer::Token third = std::move(token_list.back());
  token_list.pop_back();

  if (third.type == Lexer::SEMI_COLON) // global var declaration
  {
    // this is fucking stupid
    tu.globals.emplace_back(
        VarDeclaration(StrictType(std::move(type_name)), std::move(ident)));

    return;
  }

  if (third.type != Lexer::LPAREN)
    throw std::runtime_error("Expected lparen in function declaration");

  std::vector<VarDeclaration> parameter_list = parseParameterDecl(token_list);

  if (token_list.back().type != Lexer::LBRACE) {
    std::string error_msg = "Expected lbrace in function declaration: ";
    error_msg += token_list.back().toString();
    throw std::runtime_error(error_msg);
  }

  token_list.pop_back();

  TokenList funcbody;
  int openbrace = 1;
  while (openbrace) {
    if (token_list.empty())
      throw std::runtime_error("Expected rbrace in function declaration");

    if (token_list.back().type == Lexer::LBRACE)
      ++openbrace;
    else if (token_list.back().type == Lexer::RBRACE)
      --openbrace;

    funcbody.emplace_back(std::move(token_list.back()));
    token_list.pop_back();
  }
  funcbody.pop_back();

  tu.functions.emplace_back(
      UnparsedFunction(StrictType(std::move(type_name)), std::move(ident),
                       std::move(parameter_list), std::move(funcbody)));
}

void Parser::beginParsing(TokenList &&t) {

  TokenList token_list(std::move(t));
  UnparsedTU pass_one_tu;

  while (!token_list.empty())
    parsePassOne(pass_one_tu, token_list);

  for (auto &f : pass_one_tu.functions) {
    f.print();

    std::cout << std::endl;
  }
}
