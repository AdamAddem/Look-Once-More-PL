#include "firstparse.hpp"
#include <iostream>
#include <stdexcept>
#include <variant>
using namespace Parser;
using namespace Lexer;

void UnparsedGlobals::print() {
  if (declarations.empty())
    return;

  for (auto &decl : declarations) {
    decl.print();
  }

  std::cout << "globals{\n";
  global_init_body.print();
  std::cout << "}" << std::endl;
}

void UnparsedFunction::print() {
  std::string return_type_name;
  if (std::holds_alternative<StrictType>(return_value))
    return_type_name = std::get<StrictType>(return_value).type_name;
  else
    return_type_name = std::get<VariantType>(return_value).type_name;
  std::cout << return_type_name << " " << name << "(";
  for (auto &decl : parameter_list) {
    std::string param_type;
    if (std::holds_alternative<StrictType>(decl.type))
      param_type = std::get<StrictType>(decl.type).type_name;
    else
      param_type = std::get<VariantType>(decl.type).type_name;
    std::cout << param_type << " " << decl.ident << ", ";
  }

  std::cout << ") {" << std::endl;

  body_tokens.print();

  std::cout << "}" << std::endl;
}

void UnparsedTU::registerGlobal(Type &&_type, std::string &&_name) {
  globalsDeclared = true;
  globals.declarations.emplace_back(std::move(_type), std::move(_name));
}

void UnparsedTU::registerGlobalFuncBody(TokenHandler &&_body) {
  if (parsedGlobalBody == true)
    throw std::runtime_error("Redefinition of global initialization body");
  globals.global_init_body = std::move(_body);
  parsedGlobalBody = true;
}

void UnparsedTU::print() {
  globals.print();
  for (auto &f : functions)
    f.print();

  std::cout << std::endl;
}

Type parseType(TokenHandler &tokens) {
  Token token = tokens.eat();

  if (token.isPrimitive())
    return StrictType(token.toString());

  if (token.is(IDENTIFIER))
    return StrictType(std::move(std::get<std::string>(token.value)));

  if (token.is(LESS)) { // allows for the same type multiple times
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

      if (token.is(IDENTIFIER)) {
        types.emplace_back(StrictType(token.takeString()));
        continue;
      }

      if (token.is(KEYWORD_DEVOID))
        devoid = true;

    } while (!token.is(GTR));

    return VariantType(std::move(types), devoid);
  }

  std::string error_msg = "Expected typename: ";
  error_msg += token.toString();
  throw std::runtime_error(error_msg);
}

std::string parseIdentifier(TokenHandler &tokens) {
  Token token = tokens.eat();

  if (token.is(IDENTIFIER))
    return token.takeString();

  throw std::runtime_error("Expected identifier");
}

std::vector<VarDeclaration> parseParameterDecl(TokenHandler &tokens) {
  std::vector<VarDeclaration> parameter_list;
  if (tokens.peek().is(RPAREN)) {
    tokens.pop();
    return parameter_list;
  }

  while (true) {
    Type type_name = parseType(tokens);
    std::string ident = parseIdentifier(tokens);

    parameter_list.emplace_back(std::move(type_name), std::move(ident));

    if (tokens.pop_if(RPAREN))
      return parameter_list;

    if (tokens.pop_if(COMMA))
      continue;
    else
      throw std::runtime_error(
          "Expected ending parenthesis or comma in parameter list");
  }
}

void parseGlobalFunctions(UnparsedTU &tu, TokenHandler &tokens) {

  Type type = parseType(tokens);
  std::string ident = parseIdentifier(tokens);

  Lexer::Token third = tokens.eat();

  if (third.is(ASSIGN)) // global var declaration
    throw std::runtime_error("Global declaration not allowed after globals "
                             "initialization specified");

  if (!third.is(LPAREN))
    throw std::runtime_error("Expected lparen in function declaration");

  std::vector<VarDeclaration> parameter_list = parseParameterDecl(tokens);

  if (!tokens.pop_if(LBRACE)) {
    std::string error_msg = "Expected lbrace in function declaration: ";
    throw std::runtime_error(error_msg);
  }

  tu.functions.emplace_back(std::move(type), std::move(ident),
                            std::move(parameter_list),
                            tokens.getTokensBetweenBraces());
}

bool parseGlobals(UnparsedTU &tu, TokenHandler &tokens) {
  if (tokens.pop_if(KEYWORD_GLOBALS)) { // global func definition

    if (!tokens.pop_if(LBRACE))
      throw std::runtime_error("Expected lbrace in global initialization body");

    if (!tu.globalsDeclared)
      throw std::runtime_error(
          "Global initialzation body declared despite no global variables");

    tu.registerGlobalFuncBody(tokens.getTokensBetweenBraces());
    return false;
  }

  if (!tokens.peek_ahead(2).is(ASSIGN)) {
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

  if (!tokens.pop_if(KEYWORD_GLOBAL))
    throw std::runtime_error("Expected keyword global");

  if (!tokens.pop_if(SEMI_COLON))
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

  pass_one_tu.print();
  return pass_one_tu;
}
