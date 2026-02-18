#include "firstparse.hpp"

#include "../debug_flags.hpp"
#include <iostream>
#include <stdexcept>

using namespace Parser;
using namespace Lexer;

void UnparsedGlobals::print() {
  if (declarations.empty())
    return;

  for (auto &decl : declarations) {
    PrintStatementVisitor{}(decl);
    std::cout << '\n';
  }

  std::cout << "\nglobals{\n ";
  global_init_body.print(1);
  std::cout << "\b\b}\n" << std::endl;
}

void UnparsedFunction::print() {
  std::cout << return_type;
  std::cout << " " << name << "(";
  for (auto &decl : parameter_list) {
    printType(decl.type);
    std::cout << " " << decl.ident << ", ";
  }

  std::cout << ") {" << std::endl << " ";

  body_tokens.print(1);

  std::cout << "\b}" << std::endl;
}

void UnparsedTU::registerGlobal(Types &&_type, std::string &&_name) {
  globalsDeclared = true;
  globals.declarations.emplace_back(std::move(_type), std::move(_name));
}

void UnparsedTU::registerGlobalsFuncBody(TokenHandler &&_body) {
  if (parsedGlobalBody == true)
    throw std::runtime_error("Redefinition of global initialization body");
  globals.global_init_body = std::move(_body);
  parsedGlobalBody = true;
}

void UnparsedTU::registerFunction(std::string _return_type, std::string _name,
                                  std::vector<VarDeclaration> _decl,
                                  TokenHandler _body) {
  std::vector<Types> param_types;
  for (auto &decl : _decl)
    param_types.emplace_back(decl.type); // this is stupid

  functions.emplace_back(std::move(_return_type), std::move(_name),
                         std::move(_decl), std::move(_body));
}

void UnparsedTU::print() {
  globals.print();
  for (auto &f : functions)
    f.print();

  std::cout << std::endl;
}

Types Parser::parseType(TokenHandler &tokens) {
  Token token = tokens.eat();

  if (token.isPrimitive())
    return {token.toString()};

  if (token.is(TokenType::IDENTIFIER)) // custom type
    return {token.takeString()};

  if (token.is(TokenType::LESS)) {
    // allows for the same type multiple times, change this
    Types types;
    do {
      if (tokens.empty())
        throw std::runtime_error("Expected ending > in variant declaration");

      token = tokens.eat();
      if (token.isPrimitive()) {
        types.emplace_back(token.toString());
        continue;
      }

      if (token.is(TokenType::IDENTIFIER)) {
        types.emplace_back(token.takeString());
        continue;
      }

      if (token.is(TokenType::KEYWORD_DEVOID)) {
        types.emplace_back("");
        continue;
      }

      if (!token.is(TokenType::COMMA) && !token.is(TokenType::GTR))
        throw std::runtime_error("Expected comma in variant type list");
    } while (!token.is(TokenType::GTR));

    return types;
  }

  std::string error_msg = "Expected typename: ";
  error_msg += token.toString();
  throw std::runtime_error(error_msg);
}

std::string Parser::parseIdentifier(TokenHandler &tokens) {
  Token token = tokens.eat();

  if (token.is(TokenType::IDENTIFIER))
    return token.takeString();

  std::cout << token.toString() << std::endl;
  throw std::runtime_error("Expected identifier");
}

std::vector<VarDeclaration> parseParameterDecl(TokenHandler &tokens) {
  std::vector<VarDeclaration> parameter_list;
  if (tokens.peek_is(TokenType::RPAREN)) {
    tokens.pop();
    return parameter_list;
  }

  while (true) {
    Types type_name = parseType(tokens);
    std::string ident = parseIdentifier(tokens);

    parameter_list.emplace_back(std::move(type_name), std::move(ident));

    if (tokens.pop_if(TokenType::RPAREN))
      return parameter_list;

    tokens.expect_then_pop(
        TokenType::COMMA,
        "Expected ending parenthesis or comma in parameter list");
  }
}

void parseGlobalFunctions(UnparsedTU &tu, TokenHandler &tokens) {
  std::string return_type;
  if (tokens.pop_if(TokenType::KEYWORD_DEVOID)) {
  } else {
    Types t = parseType(tokens);
    if (t.size() > 1)
      throw std::runtime_error("Variant used as return type of function.");

    return_type = std::move(t.front());
  }

  std::string ident = parseIdentifier(tokens);
  const Token third = tokens.eat();

  third.throw_if(TokenType::ASSIGN,
                 "Global declaration not allowed after globals "
                 "initialization specified"); // global var declaration

  third.throw_if_not(TokenType::LPAREN,
                     "Expected lparen in function declaration");

  std::vector<VarDeclaration> parameter_list = parseParameterDecl(tokens);

  tokens.expect_then_pop(TokenType::LBRACE,
                         "Expected lbrace in function declaration");

  tu.registerFunction(std::move(return_type), std::move(ident),
                      std::move(parameter_list),
                      tokens.getTokensBetweenBraces());
}

// returns false when we're done parsing globals
bool parseGlobals(UnparsedTU &tu, TokenHandler &tokens) {
  if (tokens.pop_if(TokenType::KEYWORD_GLOBALS)) {
    // global func definition

    tokens.expect_then_pop(TokenType::LBRACE,
                           "Expected lbrace in global initialization body");

    if (!tu.globalsDeclared)
      throw std::runtime_error(
          "Global initialzation body declared despite no global variables");

    tu.registerGlobalsFuncBody(tokens.getTokensBetweenBraces());
    return false;
  }

  if (!tokens.peek_ahead(2).is(TokenType::ASSIGN)) {
    if (tu.globalsDeclared)
      throw std::runtime_error("Expected global initialization body");

    return false; // we're done parsing globals, we've detected something else,
                  // likely a function
  }

  Types type = parseType(tokens);
  std::string ident = parseIdentifier(tokens);

  tokens.eat();

  if (tu.parsedGlobalBody)
    throw std::runtime_error("Global declaration not allowed after globals "
                             "initialization specified");

  tokens.expect_then_pop(TokenType::KEYWORD_GLOBAL, "Expected keyword global");
  tokens.expect_then_pop(TokenType::SEMI_COLON,
                         "Expected semicolon after global declaration");

  tu.registerGlobal(std::move(type), std::move(ident));
  return true;
}

UnparsedTU Parser::firstPassParsing(TokenHandler &&tokens) {
  TokenHandler token_list(std::move(tokens));
  UnparsedTU pass_one_tu;

  while (parseGlobals(pass_one_tu, token_list)) {
  }

  while (!token_list.empty())
    parseGlobalFunctions(pass_one_tu, token_list);

  if (lom_debug::output_firstparse) {
    pass_one_tu.print();
    std::cout << "FirstParse stage passed!" << std::endl;
    std::exit(0);
  }

  return pass_one_tu;
}