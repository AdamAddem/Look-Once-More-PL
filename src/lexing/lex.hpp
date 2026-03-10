#pragma once
#include "tokentype.hpp"
#include "../error.hpp"

#include <filesystem>
#include <string>
#include <variant>
#include <vector>
#include <stdfloat>

namespace Lexer {


struct Token {
  using TokenValue = std::variant<std::uint64_t, std::float32_t, std::float64_t, std::string>;
  TokenType type;
  TokenValue value;
  unsigned line_number{};

  Token(const TokenType _type, const unsigned line_num) : type(_type), line_number(line_num) {}
  Token(const TokenType _type, TokenValue &&_value, const unsigned line_num) : type(_type), value(std::move(_value)), line_number(line_num) {}
  Token(Token &&other) noexcept : type(other.type), value(std::move(other.value)), line_number(other.line_number) { other.type = TokenType::INVALID_TOKEN; }
  Token &operator=(Token &&other) noexcept { type = other.type; other.type = TokenType::INVALID_TOKEN; value = std::move(other.value); line_number = other.line_number; return *this; }
  Token(const Token &) = default;
  Token &operator=(const Token &) = default;

  void throw_if(TokenType unwanted_type, const char* err_msg, LOMError::Stage error_stage) const;
  void throw_if_not(TokenType expected_type, const char* err_msg, LOMError::Stage error_stage) const;

  [[nodiscard]] constexpr bool is(const TokenType _type) const { return type == _type; }
  [[nodiscard]] constexpr bool isPrimitive() const { return isCategoryPRIMITIVES(type); }
  [[nodiscard]] constexpr bool isLiteral() const { return isCategoryLITERALS(type); }
  [[nodiscard]] constexpr bool isPointer() const { return isCategoryPOINTERS(type); }
  [[nodiscard]] constexpr bool isTypeModifier() const { return isCategoryTYPE_MODIFIERS(type); }

  [[nodiscard]] std::string toString() const;
  [[nodiscard]] std::string toDebugString() const;

  [[nodiscard]] std::int64_t getInt() const { return std::bit_cast<std::int64_t>(std::get<uint64_t>(value)); }
  [[nodiscard]] std::uint64_t getUint() const { return std::get<uint64_t>(value); }
  [[nodiscard]] float getFloat() const { return std::get<std::float32_t>(value); }
  [[nodiscard]] double getDouble() const { return std::get<std::float64_t>(value); }
  [[nodiscard]] bool getBool() const { return std::get<uint64_t>(value); }
  [[nodiscard]] char getChar() const { return static_cast<char>(std::get<uint64_t>(value)); }
  [[nodiscard]] std::string takeString() { return std::get<std::string>(std::move(value)); }
};

class TokenView {
  using TokenIter = std::vector<Token>::iterator;
  TokenIter begin;
  TokenIter end;

public:
  TokenView(const TokenIter _begin, const TokenIter _end) : begin(_begin), end(_end) {}

  [[nodiscard]] const Token &peek() const noexcept                            { return *begin; }
  [[nodiscard]] bool peek_is(const TokenType type) const noexcept             { return begin->type == type; }
  [[nodiscard]] const Token &peek_ahead(const long distance) const noexcept   { return *(begin + distance); }
  [[nodiscard]] Token take() noexcept                                         { return std::move(*begin++); }
  [[nodiscard]] bool empty() const noexcept                                   { return begin == end; }
  [[nodiscard]] unsigned size() const noexcept                                { return (begin - end); }
  void pop() noexcept                                                         { ++begin; }
  bool pop_if(const TokenType type) noexcept                                  { if (begin == end || begin->type != type) return false; ++begin; return true; }

  void reject_then_pop(TokenType unwanted_type, const char* err_msg, LOMError::Stage error_stage);
  void expect_then_pop(TokenType expected_type, const char* err_msg, LOMError::Stage error_stage);
  void print(unsigned initial_indent = 0) const;


  //expects that opening_token has already been popped
  //does NOT include opening and closing token in returned Handler
  //will pop closing_token, do not use for opening / closing tokens with a value that matters to you, it WILL be lost
  [[nodiscard]] TokenView getTokensBetween(TokenType opening_token, TokenType closing_token);
  [[nodiscard]] TokenView getTokensBetweenBraces() { return getTokensBetween(TokenType::LBRACE, TokenType::RBRACE); }
  [[nodiscard]] TokenView getTokensBetweenParenthesis() { return getTokensBetween(TokenType::LPAREN, TokenType::RPAREN); }
  [[nodiscard]] TokenView getTokensBetweenBrackets() { return getTokensBetween(TokenType::LBRACKET, TokenType::RBRACKET); }
  [[nodiscard]] TokenView getTokensBetweenAngleBrackets() { return getTokensBetween(TokenType::LESS, TokenType::GTR); }

  //Does not return the type specified, but will keep it
  [[nodiscard]] TokenView getAllTokensUntilFirstOf(TokenType _type);
};

[[nodiscard]] std::vector<Token> tokenizeFile(const std::filesystem::path &file_path);
} // namespace Lexer
