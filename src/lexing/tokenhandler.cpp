#include "lex.hpp"
#include <iostream>
#include <stdexcept>
#include <unordered_map>
#include <functional>

using namespace Lexer;

#define KEYWORDS_TO_STRING_MAPPING                                             \
  {TokenType::KEYWORD_AND, "and"}, {TokenType::KEYWORD_OR, "or"},              \
      {TokenType::KEYWORD_XOR, "xor"}, {TokenType::KEYWORD_NOT, "not"},        \
      {TokenType::KEYWORD_EQUALS, "equals"},                                   \
      {TokenType::KEYWORD_BITAND, "bitand"},                                   \
      {TokenType::KEYWORD_BITOR, "bitor"},                                     \
      {TokenType::KEYWORD_BITXOR, "bitxor"},                                   \
      {TokenType::KEYWORD_BITNOT, "bitnot"}, {TokenType::KEYWORD_INT, "int"},  \
      {TokenType::KEYWORD_UINT, "uint"}, {TokenType::KEYWORD_FLOAT, "float"},  \
      {TokenType::KEYWORD_DOUBLE, "double"},                                   \
      {TokenType::KEYWORD_CHAR, "char"}, {TokenType::KEYWORD_UCHAR, "uchar"},  \
      {TokenType::KEYWORD_BOOL, "bool"},                                       \
      {TokenType::KEYWORD_STRING, "string"},                                   \
      {TokenType::KEYWORD_SHORT, "short"}, {TokenType::KEYWORD_LONG, "long"},  \
      {TokenType::KEYWORD_SIGNED, "signed"},                                   \
      {TokenType::KEYWORD_UNSIGNED, "unsigned"},                               \
      {TokenType::KEYWORD_NULL, "null"},                                       \
      {TokenType::KEYWORD_DEVOID, "devoid"},                                   \
      {TokenType::KEYWORD_JUNK, "junk"},                                       \
      {TokenType::KEYWORD_SELFISH, "selfish"},                                 \
      {TokenType::KEYWORD_SHARING, "sharing"},                                 \
      {TokenType::KEYWORD_WATCHING, "watching"},                               \
      {TokenType::KEYWORD_RAW, "raw"}, {TokenType::KEYWORD_VAGUE, "vague"},    \
      {TokenType::KEYWORD_IF, "if"}, {TokenType::KEYWORD_ELSE, "else"},        \
      {TokenType::KEYWORD_ELIF, "elif"}, {TokenType::KEYWORD_FOR, "for"},      \
      {TokenType::KEYWORD_WHILE, "while"}, {TokenType::KEYWORD_DO, "do"},      \
      {TokenType::KEYWORD_RETURN, "return"},                                   \
      {TokenType::KEYWORD_SWITCH, "switch"},                                   \
      {TokenType::KEYWORD_CASE, "case"},                                       \
      {TokenType::KEYWORD_DEFAULT, "default"},                                 \
      {TokenType::KEYWORD_GOTO, "goto"}, {TokenType::KEYWORD_BREAK, "break"},  \
      {TokenType::KEYWORD_CONTINUE, "continue"},                               \
      {TokenType::KEYWORD_CAST, "cast"},                                       \
      {TokenType::KEYWORD_CAST_IF, "cast_if"},                                 \
      {TokenType::KEYWORD_UNSAFE_CAST, "unsafe_cast"},                         \
      {TokenType::KEYWORD_VERY_UNSAFE_CAST, "very_unsafe_cast"},               \
      {TokenType::KEYWORD_STEAL, "steal"},                                     \
      {TokenType::KEYWORD_BUILD_NEW, "build_new"},                             \
      {TokenType::KEYWORD_ALLOCATE, "allocate"},                               \
      {TokenType::KEYWORD_CONSTRUCT, "construct"},                             \
      {TokenType::KEYWORD_AUTO, "auto"}, {TokenType::KEYWORD_CONST, "const"},  \
      {TokenType::KEYWORD_EXCEPT, "except"},                                   \
      {TokenType::KEYWORD_STATIC, "static"},                                   \
      {TokenType::KEYWORD_EXTERN, "extern"},                                   \
      {TokenType::KEYWORD_TRUE, "true"}, {TokenType::KEYWORD_FALSE, "false"},  \
      {TokenType::KEYWORD_FROM, "from"}, {TokenType::KEYWORD_AS, "as"},        \
      {TokenType::KEYWORD_GLOBAL, "global"},                                   \
      {TokenType::KEYWORD_GLOBALS, "globals"},

#define SYMBOLS_TO_STRING_MAPPING                                              \
  {TokenType::PLUS, "+"}, {TokenType::PLUSPLUS, "++"},                         \
      {TokenType::MINUS, "-"}, {TokenType::MINUSMINUS, "--"},                  \
      {TokenType::SLASH, "/"}, {TokenType::STAR, "*"}, {TokenType::POW, "^"},  \
      {TokenType::MOD, "%"}, {TokenType::ASSIGN, "="},												\
      {TokenType::LPAREN, "("}, {TokenType::RPAREN, ")"},                      \
      {TokenType::LBRACE, "{"}, {TokenType::RBRACE, "}"},                      \
      {TokenType::LBRACKET, "["}, {TokenType::RBRACKET, "]"},                  \
      {TokenType::LESS, "<"}, {TokenType::GTR, ">"},                           \
      {TokenType::LESSEQ, "<="}, {TokenType::GTREQ, ">="},                     \
      {TokenType::SEMI_COLON, ";"}, {TokenType::ADDR, "@"},                    \
      {TokenType::COMMA, ","},


void Token::throw_if(TokenType unwanted_type, const char* err_message)const {
	if (type == unwanted_type)
		throw std::runtime_error(err_message);
}

void Token::throw_if_not(TokenType expected_type, const char* err_message) const {
	if (type != expected_type)
		throw std::runtime_error(err_message);
}

bool Token::isPrimitive() const {
  switch (type) {
  case TokenType::KEYWORD_INT:
  case TokenType::KEYWORD_UINT:
  case TokenType::KEYWORD_FLOAT:
  case TokenType::KEYWORD_DOUBLE:
  case TokenType::KEYWORD_CHAR:
  case TokenType::KEYWORD_UCHAR:
  case TokenType::KEYWORD_STRING:
  case TokenType::KEYWORD_BOOL:
  case TokenType::KEYWORD_SHORT:
  case TokenType::KEYWORD_LONG:
  case TokenType::KEYWORD_SIGNED:
  case TokenType::KEYWORD_UNSIGNED:
    return true;

  default:
    return false;
  }
}

bool Token::isLiteral() const {
  switch (type) {
  case TokenType::INT_LITERAL:
  case TokenType::FLOAT_LITERAL:
  case TokenType::DOUBLE_LITERAL:
  case TokenType::CHAR_LITERAL:
  case TokenType::STRING_LITERAL:
  case TokenType::BOOL_LITERAL:
    return true;

  default:
    return false;
  }
}

std::string Token::toString() {
  static std::unordered_map<TokenType, std::string> tokenToString{
      KEYWORDS_TO_STRING_MAPPING SYMBOLS_TO_STRING_MAPPING};
  if (type == TokenType::IDENTIFIER)
    return std::get<std::string>(value);

  if (isLiteral()) {
    switch (type) {
    case TokenType::INT_LITERAL:
      return std::to_string(std::get<int>(value));
    case TokenType::FLOAT_LITERAL:
      return std::to_string(std::get<float>(value));
    case TokenType::DOUBLE_LITERAL:
      return std::to_string(std::get<double>(value));
    case TokenType::CHAR_LITERAL:
      return std::to_string(static_cast<char>(std::get<int>(value)));
    case TokenType::STRING_LITERAL:
      return std::get<std::string>(value);
    case TokenType::BOOL_LITERAL:
      return std::get<int>(value) ? "true" : "false";

    default:
      throw std::runtime_error("Error in isLiteral method");
    }
  }

  return tokenToString[type];
}

std::string Token::toDebugString() const {
  static std::unordered_map<TokenType, std::string> tokenToString{
    KEYWORDS_TO_STRING_MAPPING SYMBOLS_TO_STRING_MAPPING};
  if (type == TokenType::IDENTIFIER)
    return std::string("id_") + std::get<std::string>(value);

  if (isLiteral()) {
    switch (type) {
      case TokenType::INT_LITERAL:
        return std::string("il_") + std::to_string(std::get<int>(value));
      case TokenType::FLOAT_LITERAL:
        return std::string("fl_") + std::to_string(std::get<float>(value));
      case TokenType::DOUBLE_LITERAL:
        return std::string("dl_") + std::to_string(std::get<double>(value));
      case TokenType::CHAR_LITERAL:
        return std::string("cl_") + std::to_string(static_cast<char>(std::get<int>(value)));
      case TokenType::STRING_LITERAL:
        return std::string("sl_") + std::get<std::string>(value);
      case TokenType::BOOL_LITERAL:
        return std::get<int>(value) ? "bl_true" : "bl_false";

      default:
        throw std::runtime_error("Error in isLiteral method");
    }
  }

  return tokenToString[type];
}

int Token::getInt() const { return std::get<int>(value); }
float Token::getFloat() const { return std::get<float>(value); }
double Token::getDouble() const { return std::get<double>(value); }
bool Token::getBool() const { return std::get<int>(value); }
std::string Token::takeString() {
  return std::get<std::string>(std::move(value));
}

/* Token Methods */

/* TokenHandler Methods */

void TokenHandler::print() {
  auto curr = token_list.rbegin();
  const auto end = token_list.rend();

  auto indent{0uz};
  auto withinParenthesis{0uz};
  while (curr != end) {
    const auto type = curr->type;

    //all this extra shit is just to make the output look somewhat pretty,
    //otherwise its just a straight line of tokens
    //probably
    if (type == TokenType::LBRACE) {
      std::cout << '{' << std::endl;
      indent++;
      std::cout << std::string(indent, ' ');
    }
    else if (type == TokenType::LPAREN) {
      std::cout << "( ";
      withinParenthesis++;
    }
    else if (type == TokenType::RPAREN) {
      std::cout << ") ";
      withinParenthesis--;
    }

    else if (type == TokenType::RBRACE) {
      std::cout << "\b}\n" << std::endl;

      indent--;
      std::cout << std::string(indent, ' ');
    }

    else if (type == TokenType::SEMI_COLON) {
      std::cout << "; ";
      if (withinParenthesis == 0) {
        std::cout << std::endl;
        std::cout << std::string(indent, ' ');
      }
    }
    else
      std::cout << curr->toDebugString() << " ";

    ++curr;
  }
}

bool TokenHandler::pop_if(const TokenType _type) {
  if (token_list.empty())
    return false;

  if (token_list.back().type == _type) {
    token_list.pop_back();
    return true;
  }

  return false;
}

void TokenHandler::reject_then_pop(TokenType unwanted_type, const char* throw_message) {
	token_list.back().throw_if(unwanted_type, throw_message);
	token_list.pop_back();
}

void TokenHandler::expect_then_pop(TokenType expected_type, const char* throw_message) {
	token_list.back().throw_if_not(expected_type, throw_message);
	token_list.pop_back();
}

TokenHandler TokenHandler::getTokensBetweenBraces() {
  std::vector<Lexer::Token> body;
  int openbrace = 1;
  while (openbrace) {
    if (token_list.empty())
      throw std::runtime_error("Expected closing rbrace");

    if (token_list.back().type == TokenType::LBRACE)
      ++openbrace;
    else if (token_list.back().type == TokenType::RBRACE)
      --openbrace;

    body.emplace_back(std::move(token_list.back()));
    token_list.pop_back();
  }
  body.pop_back();
  std::reverse(body.begin(), body.end()); // stackify

  return TokenHandler(std::move(body));
}

TokenHandler TokenHandler::getTokensBetweenParenthesis() {
  std::vector<Lexer::Token> body;
  int openbrace = 1;
  while (openbrace) {
    if (token_list.empty())
      throw std::runtime_error("Expected closing rparen");

    if (token_list.back().type == TokenType::LPAREN)
      ++openbrace;
    else if (token_list.back().type == TokenType::RPAREN)
      --openbrace;

    body.emplace_back(std::move(token_list.back()));
    token_list.pop_back();
  }
  body.pop_back();

  std::reverse(body.begin(), body.end()); // stackify

  return TokenHandler(std::move(body));
}

TokenHandler TokenHandler::getTokensBetweenBrackets() {
  std::vector<Lexer::Token> body;
  int openbracket = 1;
  while (openbracket) {
    if (token_list.empty())
      throw std::runtime_error("Expected closing rparen");

    if (token_list.back().type == TokenType::LBRACKET)
      ++openbracket;
    else if (token_list.back().type == TokenType::RBRACKET)
      --openbracket;

    body.emplace_back(std::move(token_list.back()));
    token_list.pop_back();
  }
  body.pop_back();
  std::reverse(body.begin(), body.end()); // stackify

  return TokenHandler(std::move(body));
}

TokenHandler TokenHandler::getAllTokensUntilFirstOf(TokenType _type) {
  std::vector<Token> tokens;

  while (!token_list.back().is(_type)) {
    tokens.emplace_back(std::move(token_list.back()));
    token_list.pop_back();

    if (token_list.empty())
      throw std::runtime_error(
          "Did not find token in getAllTokensUntilFirstOf");
  }
  std::reverse(tokens.begin(), tokens.end());

  return TokenHandler(std::move(tokens));
}

TokenHandler TokenHandler::getAllTokensUntilLastOf(TokenType _type) {
  const auto last_of = std::find_if(token_list.begin(), token_list.end(),
                              [_type](const Token &t) { return t.is(_type); });

  if (last_of == token_list.end())
    throw std::runtime_error("Did not find token in getAllTokensUntilLastOf");

  const size_t num = std::distance(last_of, token_list.end()) - 1;
  std::vector<Token> tokens;
  tokens.reserve(num);

  for (size_t i = 0; i < num; ++i) {
    tokens.emplace_back(std::move(token_list.back()));
    token_list.pop_back();
  }

  std::reverse(tokens.begin(), tokens.end());
  return TokenHandler(std::move(tokens));
}

//not really used anymore
void debugPrintList(const std::vector<Token> &token_list) {
  static std::unordered_map<TokenType, std::string> keywordToString = {
      KEYWORDS_TO_STRING_MAPPING};
  static std::unordered_map<TokenType, std::string> symbolToString = {
      SYMBOLS_TO_STRING_MAPPING};

  for (auto &t : token_list) {

    switch (t.type) {
    case TokenType::IDENTIFIER:
      std::cout << "IDENTIFIER: " << std::get<std::string>(t.value);
      break;

    case TokenType::INT_LITERAL:
      std::cout << "INT_LITERAL: " << std::get<int>(t.value);
      break;

    case TokenType::FLOAT_LITERAL:
      std::cout << "FLOAT_LITERAL: " << std::get<float>(t.value);
      break;

    case TokenType::DOUBLE_LITERAL:
      std::cout << "DOUBLE_LITERAL: " << std::get<double>(t.value);
      break;

    case TokenType::CHAR_LITERAL:
      std::cout << "CHAR_LITERAL: " << std::get<std::string>(t.value);
      break;

    case TokenType::STRING_LITERAL:
      std::cout << "STRING_LITERAL: " << std::get<std::string>(t.value);
      break;

    case TokenType::BOOL_LITERAL:
      std::cout << "BOOL_LITERAL: "
                << (std::get<int>(t.value) ? "true" : "false");
      break;

    default:
      if (keywordToString.contains(t.type))
        std::cout << "KEYWORD: " << keywordToString[t.type];
      else if (symbolToString.contains(t.type))
        std::cout << "sym: " << symbolToString[t.type];
      else
        std::cout << "ERROR: ";
    }

    std::cout << std::endl;
  }
}
