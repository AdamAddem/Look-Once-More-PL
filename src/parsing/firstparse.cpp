#include "firstparse.hpp"
#include <iostream>
#include <stdexcept>
#include <variant>

using namespace Parser;
using namespace Lexer;

void UnparsedGlobals::print() {
  if (declarations.empty())
    return;

  for (auto &decl : declarations)
    PrintStatementVisitor{}(decl);

  std::cout << "globals{\n";
  global_init_body.print();
  std::cout << "}" << std::endl;
}

void UnparsedFunction::print() {
  printType(return_value);
  for (auto &decl : parameter_list) {
    printType(decl.type);
    std::cout << " " << decl.ident << ", ";
  }

  std::cout << ") {" << std::endl;

  body_tokens.print();

  std::cout << "}" << std::endl;
}

void UnparsedTU::registerGlobal(Type &&_type, std::string &&_name) {
  globalsDeclared = true;
  globals.declarations.emplace_back(std::move(_type), std::move(_name));
}

void UnparsedTU::registerGlobalsFuncBody(TokenHandler &&_body) {
  if (parsedGlobalBody == true)
    throw std::runtime_error("Redefinition of global initialization body");
  globals.global_init_body = std::move(_body);
  parsedGlobalBody = true;
}

void UnparsedTU::registerFunction(StrictType _type, std::string _name,
                                  std::vector<VarDeclaration> _decl,
                                  Lexer::TokenHandler _body) {
  std::vector<Type> param_types;
  for (auto &decl : _decl)
    param_types.emplace_back(decl.type); // this is stupid

  functions.emplace_back(std::move(_type), std::move(_name), std::move(_decl),
                         std::move(_body));
}

void UnparsedTU::print() {
  globals.print();
  for (auto &f : functions)
    f.print();

  std::cout << std::endl;
}

Type Parser::parseType(TokenHandler &tokens) {
  Token token = tokens.eat();

  if (token.isPrimitive())
    return StrictType(token.toString());

  if (token.is(TokenType::IDENTIFIER))
    return StrictType(std::move(std::get<std::string>(token.value)));

  if (token.is(TokenType::LESS)) { // allows for the same type multiple times
    std::vector<StrictType> types;
    bool devoid = false;
    do {
      if (tokens.empty())
        throw std::runtime_error("Expected ending > in variant declaration");

      token = tokens.eat();
      if (token.isPrimitive()) {
        types.emplace_back(StrictType(token.toString()));
        continue;
      }

      if (token.is(TokenType::IDENTIFIER)) {
        types.emplace_back(StrictType(token.takeString()));
        continue;
      }

      if (token.is(TokenType::KEYWORD_DEVOID)) {
        devoid = true;
        continue;
      }

      if (!token.is(TokenType::COMMA) && !token.is(TokenType::GTR)) {
        throw std::runtime_error("Expected comma in variant type list");
      }

    } while (!token.is(TokenType::GTR));

    return VariantType(std::move(types), devoid);
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
    Type type_name = parseType(tokens);
    std::string ident = parseIdentifier(tokens);

    parameter_list.emplace_back(std::move(type_name), std::move(ident));

    if (tokens.pop_if(TokenType::RPAREN))
      return parameter_list;

    if (tokens.pop_if(TokenType::COMMA))
      continue;
    else
      throw std::runtime_error(
          "Expected ending parenthesis or comma in parameter list");
  }
}

void parseGlobalFunctions(UnparsedTU &tu, TokenHandler &tokens) {

  StrictType type = [&]() -> StrictType {
    if (tokens.peek_is(TokenType::KEYWORD_DEVOID))
      return StrictType("devoid");

    Type t = parseType(tokens);
    if (std::holds_alternative<VariantType>(t))
      throw std::runtime_error("VariantType used as return type of function.");

    return std::get<StrictType>(t);
  }();

  std::string ident = parseIdentifier(tokens);

  Lexer::Token third = tokens.eat();

  if (third.is(TokenType::ASSIGN)) // global var declaration
    throw std::runtime_error("Global declaration not allowed after globals "
                             "initialization specified");

  if (!third.is(TokenType::LPAREN))
    throw std::runtime_error("Expected lparen in function declaration");

  std::vector<VarDeclaration> parameter_list = parseParameterDecl(tokens);

  if (!tokens.pop_if(TokenType::LBRACE)) {
    std::string error_msg = "Expected lbrace in function declaration: ";
    throw std::runtime_error(error_msg);
  }

  tu.registerFunction(std::move(type), std::move(ident),
                      std::move(parameter_list),
                      tokens.getTokensBetweenBraces());
}

bool parseGlobals(UnparsedTU &tu, TokenHandler &tokens) {
  if (tokens.pop_if(TokenType::KEYWORD_GLOBALS)) { // global func definition

    if (!tokens.pop_if(TokenType::LBRACE))
      throw std::runtime_error("Expected lbrace in global initialization body");

    if (!tu.globalsDeclared)
      throw std::runtime_error(
          "Global initialzation body declared despite no global variables");

    tu.registerGlobalsFuncBody(tokens.getTokensBetweenBraces());
    return false;
  }

  if (!tokens.peek_ahead(2).is(TokenType::ASSIGN)) {
    if (tu.globalsDeclared)
      throw std::runtime_error("Expected global initialization body");

    return false;
  }

  Type type = parseType(tokens);
  std::string ident = parseIdentifier(tokens);

  Lexer::Token third = tokens.eat();

  if (tu.parsedGlobalBody)
    throw std::runtime_error("Global declaration not allowed after globals "
                             "initialization specified");

  if (!tokens.pop_if(TokenType::KEYWORD_GLOBAL))
    throw std::runtime_error("Expected keyword global");

  if (!tokens.pop_if(TokenType::SEMI_COLON))
    throw std::runtime_error("Expected semicolon after global declaration");

  tu.registerGlobal(std::move(type), std::move(ident));
  return true;
}

UnparsedTU Parser::firstPassParsing(TokenHandler &&tokens) {

  TokenHandler token_list(std::move(tokens));
  UnparsedTU pass_one_tu;

  while (parseGlobals(pass_one_tu, token_list))
    ;

  while (!token_list.empty())
    parseGlobalFunctions(pass_one_tu, token_list);

  return pass_one_tu;
}
