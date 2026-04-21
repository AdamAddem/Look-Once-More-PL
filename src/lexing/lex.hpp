#pragma once
#include "edenlib/assume_assert.hpp"
#include "edenlib/releasing_vector.hpp"
#include "edenlib/typedefs.hpp"
#include "tokentype.hpp"

#include <cstring>
#include <filesystem>
#include <string>
#include <vector>

namespace LOM::Lexer {

class Token {
  using TokenValue = u64_t;
  TokenType type;
  u32_t line_number;
  TokenValue value;
  friend class TokenView;

  [[nodiscard]] constexpr char*
  copy_string() const noexcept {
    assume_assert(type == TokenType::STRING_LITERAL or type == TokenType::IDENTIFIER);
    return
    eden::releasing_string::copy_data(
      eden::releasing_string::released_ptr(
        std::bit_cast<char*>(value)
        )
        ).get();
  }

  [[nodiscard]] constexpr std::string
  to_stdstring() const noexcept {
    assume_assert(type == TokenType::STRING_LITERAL or type == TokenType::IDENTIFIER);
    return std::string(eden::owned_ptr<char[]>(std::bit_cast<char*>(value)));
  }

public:

  Token(TokenType type, u32_t line_number) : type(type), line_number(line_number), value() {}
  Token(TokenType type, u64_t value, u32_t line_number) : type(type), line_number(line_number), value(value) {}
  Token(char character_literal, u32_t line_number) : type(TokenType::CHAR_LITERAL), line_number(line_number), value(static_cast<u64_t>(character_literal)) {}
  Token(TokenType type, eden::releasing_string::released_ptr string, u32_t line_number) : type(type), line_number(line_number), value(std::bit_cast<u64_t>(string.release())) {}
  Token(Token &&other) noexcept : type(other.type), line_number(other.line_number), value(other.value) {other.type = TokenType::INVALID_TOKEN; other.value = 0;}
  Token &operator=(Token &&other) noexcept {type = other.type; other.type=TokenType::INVALID_TOKEN; value = other.value; other.value = 0; line_number = other.line_number; return *this;}

  Token(const Token& other)
  : type(other.type), line_number(other.line_number) {
    if (type == TokenType::STRING_LITERAL or type == TokenType::IDENTIFIER)
      value = std::bit_cast<u64_t>(other.copy_string());
    else
      value = other.value;
  }

  Token &operator=(const Token& other) = delete;

  void throw_if(TokenType unwanted_type, const char* err_msg) const;
  void throw_if_not(TokenType expected_type, const char* err_msg) const;

  [[nodiscard]] constexpr TokenType getType() const noexcept           {return type;}
  [[nodiscard]] constexpr u32_t getLN() const noexcept                 {return line_number;}
  [[nodiscard]] constexpr bool is(TokenType token_type) const noexcept {return type == token_type;}
  [[nodiscard]] constexpr bool isPrimitive() const noexcept            {return isCategoryPRIMITIVES(type);}
  [[nodiscard]] constexpr bool isLiteral() const noexcept              {return isCategoryLITERALS(type);}
  [[nodiscard]] constexpr bool isPointer() const noexcept              {return isCategoryPOINTERS(type);}
  [[nodiscard]] constexpr bool isTypeModifier() const noexcept         {return isCategoryTYPE_MODIFIERS(type);}

  [[nodiscard]] std::string toString() const;
  [[nodiscard]] std::string toDebugString() const;

  [[nodiscard]] constexpr u64_t
  getRawValue() const noexcept {return value;}

  [[nodiscard]] constexpr i64_t
  getSigned() const noexcept {assume_assert(type == TokenType::INTEGER_LITERAL); return std::bit_cast<i64_t>(value);}

  [[nodiscard]] constexpr u64_t
  getUnsigned() const noexcept {assume_assert(type == TokenType::UNSIGNED_LITERAL); return value;}

  [[nodiscard]] constexpr float
  getFloat() const noexcept {assume_assert(type == TokenType::FLOAT_LITERAL); return std::bit_cast<float>(static_cast<u32_t>(value));}

  [[nodiscard]] constexpr double
  getDouble() const noexcept {assume_assert(type == TokenType::DOUBLE_LITERAL); return std::bit_cast<double>(value);}

  [[nodiscard]] constexpr bool
  getBool() const noexcept {assume_assert(type == TokenType::BOOL_LITERAL); return value;}

  [[nodiscard]] constexpr char
  getChar() const noexcept {assume_assert(type == TokenType::CHAR_LITERAL); return static_cast<char>(value);}

  [[nodiscard]] constexpr eden::releasing_string::released_ptr
  takeString() noexcept {
    assume_assert(type == TokenType::STRING_LITERAL or type == TokenType::IDENTIFIER);
    const auto retval = std::bit_cast<char*>(value);
    value = 0;
    return eden::releasing_string::released_ptr(retval);
  }
};

class TokenView {
  using TokenIter = std::vector<Token>::iterator;
  TokenIter begin;
  TokenIter end;

public:
  explicit TokenView(std::vector<Token>& tokens) {
    begin = tokens.begin();
    end = tokens.end();
  }

  TokenView(TokenIter begin, TokenIter end) : begin(begin), end(end) {}

  [[nodiscard]] const Token& peek() const noexcept                            {return *begin;}
  [[nodiscard]] bool peek_is(const TokenType type) const noexcept             {return begin->type == type;}
  [[nodiscard]] const Token& peek_ahead(const long distance) const noexcept   {return *(begin + distance);}
  [[nodiscard]] Token take() noexcept                                         {return std::move(*begin++);}
  [[nodiscard]] bool empty() const noexcept                                   {return begin == end;}
  [[nodiscard]] unsigned size() const noexcept                                {return begin - end;}
  [[nodiscard]] const Token& previousToken() const noexcept                   {return *(begin - 1);}
  [[nodiscard]] u32_t current_line_number() const noexcept                    {return empty() ? (begin - 1)->line_number : begin->line_number;}
  void pop() noexcept                                                         {if (not empty()) ++begin;}
  bool pop_if(const TokenType type) noexcept                                  {if (empty() or begin->type not_eq type) return false; ++begin; return true;}

  void expect_then_pop(TokenType expected_type, const char* err_msg);
  void expect(TokenType expected_type, const char* err_msg) const;
  void reject_then_pop(TokenType unwanted_type, const char* err_msg);
  void reject(TokenType unwanted_type, const char* err_msg) const;
  void print(unsigned initial_indent = 0) const;


  //expects that opening_token has already been popped
  //does NOT include opening or closing token in returned Handler
  //will pop closing_token, do not use for opening / closing tokens with a value that matters to you, it WILL be lost
  [[nodiscard]] TokenView getTokensBetween(TokenType opening_token, TokenType closing_token);
  [[nodiscard]] TokenView getTokensBetweenBraces() { return getTokensBetween(TokenType::LBRACE, TokenType::RBRACE); }
  [[nodiscard]] TokenView getTokensBetweenParenthesis() { return getTokensBetween(TokenType::LPAREN, TokenType::RPAREN); }
  [[nodiscard]] TokenView getTokensBetweenBrackets() { return getTokensBetween(TokenType::LBRACKET, TokenType::RBRACKET); }
  [[nodiscard]] TokenView getTokensBetweenAngleBrackets() { return getTokensBetween(TokenType::LESS, TokenType::GTR); }

  //Does not return the type specified and pops it
  [[nodiscard]] TokenView getAllTokensUntilFirstOf(TokenType type);
  [[nodiscard]] u64_t distanceFromFirstOf(TokenType type) const;
};

[[nodiscard]] std::vector<Token> tokenizeFile(const std::filesystem::path &file_path);
} // namespace Lexer
