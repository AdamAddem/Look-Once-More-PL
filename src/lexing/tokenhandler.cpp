#include "lex.hpp"
#include <functional>
#include <iostream>
#include <stdexcept>
#include <utility>

using namespace Lexer;

//this is so fucking dumb
static constexpr const char *tokenToString[] = {"INVALID_TOKEN",
                                                "IDENTIFIER",
                                                "BEGIN_LITERALS",
                                                "INT_LITERAL",
                                                "FLOAT_LITERAL",
                                                "DOUBLE_LITERAL",
                                                "STRING_LITERAL",
                                                "CHAR_LITERAL",
                                                "BOOL_LITERAL",
                                                "END_LITERALS",

                                                "BEGIN_SYMBOLS",
                                                "+",
                                                "++",
                                                "-",
                                                "--",
                                                "/",
                                                "*",
                                                "^",
                                                "%",
                                                "=",
                                                "(",
                                                ")",
                                                "{",
                                                "}",
                                                "[",
                                                "]",
                                                "<",
                                                ">",
                                                "<=",
                                                ">=",
                                                ";",
                                                "@",
                                                ",",
                                                "END_SYMBOLS",

                                                "BEGIN_COMP_BITWISE",
                                                "and",
                                                "or",
                                                "xor",
                                                "not",
                                                "eq",
                                                "not_eq",
                                                "bitand",
                                                "bitor",
                                                "bitxor",
                                                "bitnot",
                                                "END_COMP_BITWISE",

                                                "BEGIN_PRIMITIVES",
                                                "i8",
                                                "i16",
                                                "i32",
                                                "i64",
                                                "u8",
                                                "u16",
                                                "u32",
                                                "u64",
                                                "f32",
                                                "f64",
                                                "char",
                                                "string",
                                                "bool",
                                                "devoid",
                                                "END_PRIMITIVES",

                                                "BEGIN_POINTERS",
                                                "selfish",
                                                "sharing",
                                                "watching",
                                                "raw",
                                                "vague",
                                                "END_POINTERS",

                                                "BEGIN_CONTROL_FLOW",
                                                "if",
                                                "else",
                                                "for",
                                                "while",
                                                "do",
                                                "return",
                                                "switch",
                                                "case",
                                                "default",
                                                "goto",
                                                "break",
                                                "continue",
                                                "END_CONTROL_FLOW",

                                                "BEGIN_CAST",
                                                "cast",
                                                "cast_if",
                                                "unsafe_cast",
                                                "end_cast",

                                                "BEGIN_ALLOC_LIFETIMES",
                                                "steal",
                                                "build_new",
                                                "allocate",
                                                "construct",
                                                "END_ALLOC_LIFETIMES",

                                                "from",
                                                "as",
                                                "global",
                                                "globals",
                                                "mut",
                                                "null",
                                                "junk"};

void Token::throw_if(TokenType unwanted_type, const char *err_message) const {
  if (type == unwanted_type)
    throw std::runtime_error(err_message);
}

void Token::throw_if_not(TokenType expected_type,
                         const char *err_message) const {
  if (type != expected_type)
    throw std::runtime_error(err_message);
}

bool Token::isPrimitive() const {
  const auto underlying_value = std::to_underlying(type);
  static constexpr auto primitives_min =
      std::to_underlying(TokenType::BEGIN_PRIMITIVES);
  static constexpr auto primitives_max =
      std::to_underlying(TokenType::END_PRIMITIVES);

  return underlying_value > primitives_min && underlying_value < primitives_max;
}

bool Token::isLiteral() const {
  const auto underlying_value = std::to_underlying(type);
  static constexpr auto literals_min =
      std::to_underlying(TokenType::BEGIN_LITERALS);
  static constexpr auto literals_max =
      std::to_underlying(TokenType::END_LITERALS);

  return underlying_value > literals_min && underlying_value < literals_max;
}

std::string Token::toString() {
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

  return tokenToString[std::to_underlying(type)];
}

std::string Token::toDebugString() const {
  if (type == TokenType::IDENTIFIER)
    return std::string("id_") + std::get<std::string>(value);

  if (isLiteral()) {
    switch (type) {
    case TokenType::INT_LITERAL:
      return std::to_string(std::get<int>(value)) + std::string("i");
    case TokenType::FLOAT_LITERAL:
      return std::to_string(std::get<float>(value)) + std::string("f");
    case TokenType::DOUBLE_LITERAL:
      return std::to_string(std::get<double>(value)) + std::string("d");
    case TokenType::CHAR_LITERAL:
      return std::to_string(static_cast<char>(std::get<int>(value))) +
             std::string("c");
    case TokenType::STRING_LITERAL:
      return std::get<std::string>(value) + std::string("s");
    case TokenType::BOOL_LITERAL:
      return std::get<int>(value) ? "true_b" : "false_b";

    default:
      throw std::runtime_error("Error in isLiteral method");
    }
  }

  return tokenToString[std::to_underlying(type)];
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

void TokenHandler::print(const unsigned initial_indent) {
  auto curr = token_list.rbegin();
  const auto end = token_list.rend();

  auto indent{initial_indent};
  auto withinParenthesis{0uz};
  while (curr != end) {
    const auto type = curr->type;

    // all this extra shit is just to make the output look somewhat pretty,
    // otherwise its just a straight line of tokens
    // probably
    if (type == TokenType::LBRACE) {
      std::cout << '{' << std::endl;
      indent++;
      std::cout << std::string(indent, ' ');
    } else if (type == TokenType::LPAREN) {
      std::cout << "( ";
      withinParenthesis++;
    } else if (type == TokenType::RPAREN) {
      std::cout << ") ";
      withinParenthesis--;
    } else if (type == TokenType::RBRACE) {
      std::cout << "\b}\n" << std::endl;
      indent--;
      std::cout << std::string(indent, ' ');
    } else if (type == TokenType::SEMI_COLON) {
      std::cout << "; ";
      if (withinParenthesis == 0) {
        std::cout << std::endl;
        std::cout << std::string(indent, ' ');
      }
    } else
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

void TokenHandler::reject_then_pop(TokenType unwanted_type,
                                   const char *throw_message) {
  token_list.back().throw_if(unwanted_type, throw_message);
  token_list.pop_back();
}

void TokenHandler::expect_then_pop(TokenType expected_type,
                                   const char *throw_message) {
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
  const auto last_of =
      std::find_if(token_list.begin(), token_list.end(),
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