#include "firstparse.hpp"

#include "../debug_flags.hpp"

#include <cassert>
#include <iostream>
#include <stdexcept>
#include <unordered_set>

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
  std::cout << "fn " << name << "( ";
  for (auto &decl : parameter_list) {
    decl.type.print();
    std::cout << " " << decl.ident << ", ";
  }
  std::cout << "\b\b ";

  std::cout << ") -> ";
  return_type.print();
  std::cout << " {\n ";

  body_tokens.print(1);

  std::cout << "\b}\n";
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

void UnparsedTU::registerFunction(Type&& _return_type, std::string _name,
                                  std::vector<VarDeclaration> _decl,
                                  TokenHandler _body) {
  std::vector<Type> param_types;
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

Type Parser::parseType(TokenHandler &tokens) {
  Token token = tokens.eat();
  bool is_mutable{false};
  if (token.is(TokenType::KEYWORD_MUT)) {
    token = tokens.eat();
    is_mutable = true;
  }

  if (token.isPrimitive() || token.is(TokenType::IDENTIFIER)) {
    if (token.is(TokenType::KEYWORD_DEVOID))
      return devoid_type;

    if (token.isPointer()) { // add reference support eventually
      tokens.expect_then_pop(TokenType::ARROW, "Expected arrow in pointer declaration");
      return {token.toString(), is_mutable, new Type(parseType(tokens))};
    }

    std::string s = token.toString();
    return {std::move(s), is_mutable};
  }


  if (token.is(TokenType::LESS)) {
    TokenHandler variant_types_tokens = tokens.getTokensBetweenAngleBrackets();

    Type variant_types(Type::variant, parseType(variant_types_tokens));
    variant_types.is_mutable = is_mutable;

    std::unordered_set<std::string> typenames; //prevent duplicate types

    while (variant_types_tokens.pop_if(TokenType::COMMA)) {
      Type type = parseType(variant_types_tokens);
      if (type.isVariant())
        throw std::runtime_error("Nested variant types not allowed");

      if (type.isDevoid()) {
        typenames.emplace("");
        variant_types.addTypeToVariantList(std::move(type));
        continue;
      }

      if (type.is_mutable)
        throw std::runtime_error("Mutability cannot be specified within variant type list, must be specified prior to type list");

      if (typenames.contains(type.getTypename()))
        throw std::runtime_error("Duplicate types specified in variant declaration");

      typenames.emplace(type.getTypename());

      variant_types.addTypeToVariantList(std::move(type));
    }

    if (variant_types.numVariantTypes() < 2)
      throw std::runtime_error("Two or more types must be specified in variant type list");
    return variant_types;
  }

  std::string error_msg = "Expected typename, got: ";
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
  if (tokens.empty())
    return {};

  std::vector<VarDeclaration> parameter_list;

  while (true) {
    Type type = parseType(tokens);
    std::string ident = parseIdentifier(tokens);

    parameter_list.emplace_back(std::move(type), std::move(ident));

    if (tokens.empty())
      return parameter_list;

    tokens.expect_then_pop(
        TokenType::COMMA,
        "Expected ending parenthesis or comma in parameter list");
  }
}

void parseGlobalFunctions(UnparsedTU &tu, TokenHandler &tokens) {
  tokens.expect_then_pop(TokenType::KEYWORD_FN, "Expected function");

  std::string ident = parseIdentifier(tokens);
  tokens.expect_then_pop(TokenType::LPAREN, "Expected lparen in function declaration");

  TokenHandler parameter_tokens = tokens.getTokensBetweenParenthesis();
  std::vector<VarDeclaration> parameter_list = parseParameterDecl(parameter_tokens);

  Type return_type{devoid_type};
  if (tokens.pop_if(TokenType::ARROW))
    return_type = parseType(tokens);

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

  if (tokens.peek_is(TokenType::KEYWORD_FN))
    return false;

  auto decl_tokens = tokens.getAllTokensUntilFirstOf(TokenType::ASSIGN);
  tokens.pop();

  Type type = parseType(decl_tokens);
  std::string ident = parseIdentifier(decl_tokens);


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