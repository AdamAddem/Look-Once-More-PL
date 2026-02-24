#pragma once
#include "tokentype.hpp"
#include "../error.hpp"
#include <string>
#include <variant>
#include <vector>

namespace Lexer {


struct Token {
  using TokenValue = std::variant<int, float, double, std::string>;
  TokenType type;
  TokenValue value;
  unsigned line_number{};

  Token(const TokenType _type, const unsigned line_num) : type(_type), line_number(line_num) {};

  Token(const TokenType _type, TokenValue &&_value, const unsigned line_num)
      : type(_type), value(std::move(_value)), line_number(line_num) {};

  Token(const Token &) = default;

  Token(Token &&other) noexcept
      : type(other.type), value(std::move(other.value)), line_number(other.line_number) {}

  Token &operator=(const Token &) = default;

  Token &operator=(Token &&other) noexcept {
    type = other.type;
    value = std::move(other.value);
    line_number = other.line_number;
    return *this;
  }


  void throw_if(TokenType unwanted_type, const char* err_msg, LOMError::Stage error_stage) const;
  void throw_if_not(TokenType expected_type, const char* err_msg, LOMError::Stage error_stage) const;

  [[nodiscard]] constexpr bool is(const TokenType _type) const { return type == _type; }
  [[nodiscard]] constexpr bool isPrimitive() const {return isCategoryPRIMITIVES(type);}
  [[nodiscard]] constexpr bool isLiteral() const {return isCategoryLITERALS(type);}
  [[nodiscard]] constexpr bool isPointer() const {return isCategoryPOINTERS(type);}
  [[nodiscard]] constexpr bool isTypeModifier() const {return isCategoryTYPE_MODIFIERS(type);}

  [[nodiscard]] std::string toString() const;
  [[nodiscard]] std::string toDebugString() const;

  [[nodiscard]] int getInt() const;
  [[nodiscard]] float getFloat() const;
  [[nodiscard]] double getDouble() const;
  [[nodiscard]] bool getBool() const;
  [[nodiscard]] std::string takeString();
};

class TokenHandler {
  std::vector<Token> token_list;

public:
  explicit TokenHandler(std::vector<Token> &&_tokens)
      : token_list(std::move(_tokens)) {};

  TokenHandler() = default;

  TokenHandler(TokenHandler &&other) noexcept
      : token_list(std::move(other.token_list)) {}

  TokenHandler &operator=(TokenHandler &&other) noexcept {
    token_list = std::move(other.token_list);
    return *this;
  }

  [[nodiscard]] const Token &peek() const { return token_list.back(); };
  [[nodiscard]] const Token &peek_back() const { return token_list.front(); }

  [[nodiscard]] bool peek_is(const TokenType type) const {
    return token_list.back().type == type;
  }

  [[nodiscard]] const Token &peek_ahead(const std::size_t distance) {
    return token_list.at(token_list.size() - distance - 1);
  }

  Token eat() {
    Token t = std::move(token_list.back());
    token_list.pop_back();
    return t;
  }

  [[nodiscard]] bool check(const TokenType _type) const {
    return token_list.back().type == _type;
  }
  [[nodiscard]] bool empty() const { return token_list.empty(); }
  [[nodiscard]] unsigned size() const { return token_list.size(); }

  void pop() { token_list.pop_back(); }

  bool pop_if(TokenType _type);

  void reject_then_pop(TokenType unwanted_type, const char* err_msg, LOMError::Stage error_stage);
  void expect_then_pop(TokenType expected_type, const char* err_msg, LOMError::Stage error_stage);


  void print(unsigned initial_indent = 0);


  //expects that opening_token has already been popped
  //does NOT include opening and closing token in returned Handler
  //will pop closing_token, do not use for opening / closing tokens with a value that matters to you, it WILL be lost
  [[nodiscard]] TokenHandler getTokensBetween(TokenType opening_token, TokenType closing_token);

  [[nodiscard]] TokenHandler getTokensBetweenBraces() {
    return getTokensBetween(TokenType::LBRACE, TokenType::RBRACE);
  }

  [[nodiscard]] TokenHandler getTokensBetweenParenthesis() {
    return getTokensBetween(TokenType::LPAREN, TokenType::RPAREN);
  }

  [[nodiscard]] TokenHandler getTokensBetweenBrackets() {
    return getTokensBetween(TokenType::LBRACKET, TokenType::RBRACKET);
  }

  [[nodiscard]] TokenHandler getTokensBetweenAngleBrackets() {
    return getTokensBetween(TokenType::LESS, TokenType::GTR);
  }


  //Does not return the type specified, but will keep it
  [[nodiscard]] TokenHandler getAllTokensUntilFirstOf(TokenType _type);
  [[nodiscard]] TokenHandler getAllTokensUntilLastOf(TokenType _type);
};

[[nodiscard]] TokenHandler tokenizeFile(const std::string &file_path);
} // namespace Lexer
