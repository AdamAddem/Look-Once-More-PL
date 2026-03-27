#pragma once
#include "tokentype.hpp"
#include "utilities/typedefs.hpp"

#include <filesystem>
#include <string>
#include <variant>
#include <vector>

namespace LOM::Lexer {


struct Token {
  using TokenValue = std::variant<u64_t, std::string>;
  TokenType type;
  TokenValue value;
  u64_t line_number;

  Token(TokenType type, u64_t line_number) : type(type), line_number(line_number) {}
  Token(TokenType type, TokenValue &&value, u64_t line_number) : type(type), value(std::move(value)), line_number(line_number) {}
  Token(Token &&other) noexcept : type(other.type), value(std::move(other.value)), line_number(other.line_number) { other.type = TokenType::INVALID_TOKEN; }
  Token &operator=(Token &&other) noexcept { type = other.type; other.type = TokenType::INVALID_TOKEN; value = std::move(other.value); line_number = other.line_number; return *this; }
  Token(const Token &) = default;
  Token &operator=(const Token &) = default;

  void throw_if(TokenType unwanted_type, const char* err_msg) const;
  void throw_if_not(TokenType expected_type, const char* err_msg) const;

  [[nodiscard]] constexpr bool is(TokenType _type) const  {return type eq _type;}
  [[nodiscard]] constexpr bool isPrimitive() const        {return isCategoryPRIMITIVES(type);}
  [[nodiscard]] constexpr bool isLiteral() const          {return isCategoryLITERALS(type);}
  [[nodiscard]] constexpr bool isPointer() const          {return isCategoryPOINTERS(type);}
  [[nodiscard]] constexpr bool isTypeModifier() const     {return isCategoryTYPE_MODIFIERS(type);}

  [[nodiscard]] std::string toString() const;
  [[nodiscard]] std::string toDebugString() const;

  [[nodiscard]] i64_t getInt() const                      {return std::bit_cast<i64_t>(std::get<u64_t>(value));}
  [[nodiscard]] u64_t getUint() const                     {return std::get<u64_t>(value);}
  [[nodiscard]] float getFloat() const                    {return std::bit_cast<float>(static_cast<u32_t>(std::get<u64_t>(value)));}
  [[nodiscard]] double getDouble() const                  {return std::bit_cast<double>(std::get<u64_t>(value));}
  [[nodiscard]] bool getBool() const                      {return std::get<u64_t>(value);}
  [[nodiscard]] char getChar() const                      {return static_cast<char>(std::get<u64_t>(value));}
  [[nodiscard]] std::string takeString()                  {return std::get<std::string>(std::move(value));}
};

class TokenView {
  using TokenIter = std::vector<Token>::iterator;
  TokenIter begin;
  TokenIter end;

public:
  explicit TokenView(std::vector<Token>& tokens) : begin(tokens.begin()) {
    if (tokens.back().type not_eq TokenType::INVALID_TOKEN)
      tokens.emplace_back(TokenType::INVALID_TOKEN, 0);
    end = tokens.end() - 1;
  }
  TokenView(TokenIter begin, TokenIter end) : begin(begin), end(end) {}

  [[nodiscard]] const Token& peek() const noexcept                            { return *begin; }
  [[nodiscard]] bool peek_is(const TokenType type) const noexcept             { return begin->type eq type; }
  [[nodiscard]] const Token& peek_ahead(const long distance) const noexcept   { return *(begin + distance); }
  [[nodiscard]] Token take() noexcept                                         { return std::move(*begin++); }
  [[nodiscard]] bool empty() const noexcept                                   { return begin eq end; }
  [[nodiscard]] unsigned size() const noexcept                                { return begin - end; }
  [[nodiscard]] const Token& previousToken() const noexcept                   { return *(begin - 1); }
  [[nodiscard]] u64_t current_line_number() const noexcept                    { return empty() ? (begin - 1)->line_number : begin->line_number; }
  void pop() noexcept                                                         { if (not empty()) ++begin; }
  bool pop_if(const TokenType type) noexcept                                  { if (empty() or begin->type not_eq type) return false; ++begin; return true; }

  void expect_then_pop(TokenType expected_type, const char* err_msg);
  void expect(TokenType expected_type, const char* err_msg) const;
  void reject_then_pop(TokenType unwanted_type, const char* err_msg);
  void reject(TokenType unwanted_type, const char* err_msg) const;
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
  [[nodiscard]] TokenView getAllTokensUntilFirstOf(TokenType type);
  [[nodiscard]] u64_t distanceFromFirstOf(TokenType type) const;
};

[[nodiscard]] std::vector<Token> tokenizeFile(const std::filesystem::path &file_path);
} // namespace Lexer
