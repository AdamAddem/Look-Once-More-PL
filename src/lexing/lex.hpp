#pragma once
#include "edenlib/macros.hpp"
#include "edenlib/typedefs.hpp"
#include "tokentype.hpp"

#include <filesystem>
#include <string>
#include <vector>

namespace LOM::Lexer {

struct Token {
  TokenType type;
  u16_t length;
  u32_t pos;

  [[nodiscard]] std::string toString() const;
  [[nodiscard]] std::string toDebugString() const;

  // expensive
  [[nodiscard]] constexpr i64_t
  getSigned(std::string const& file_text) const noexcept {
    assume_assert(type == TokenType::SIGNED_LITERAL);
    auto const begin = file_text.data() + pos;
    i64_t res;
    auto const from_chars_res = std::from_chars(begin, begin + length, res);
    assert(from_chars_res.ec == std::errc());
    return res;
  }

  // expensive
  [[nodiscard]] constexpr u64_t
  getUnsigned(std::string const& file_text) const noexcept {
    assume_assert(type == TokenType::UNSIGNED_LITERAL);
    auto const begin = file_text.data() + pos;
    u64_t res;
    auto const from_chars_res = std::from_chars(begin, begin + length, res);
    assert(from_chars_res.ec == std::errc());
    return res;
  }

  // expensive
  [[nodiscard]] constexpr float
  getFloat(std::string const& file_text) const noexcept {
    assume_assert(type == TokenType::FLOAT_LITERAL);
    auto const begin = file_text.data() + pos;
    float res;
    auto const from_chars_res = std::from_chars(begin, begin + length, res);
    assert(from_chars_res.ec == std::errc());
    return res;
  }

  // expensive
  [[nodiscard]] constexpr double
  getDouble(std::string const& file_text) const noexcept {
    assume_assert(type == TokenType::DOUBLE_LITERAL);
    auto const begin = file_text.data() + pos;
    double res;
    auto const from_chars_res = std::from_chars(begin, begin + length, res);
    assert(from_chars_res.ec == std::errc());
    return res;
  }

  [[nodiscard]] constexpr bool
  getBool(std::string const& file_text) const noexcept {
    assume_assert(type == TokenType::BOOL_LITERAL);
    return file_text[pos] == 't';
  }

  [[nodiscard]] constexpr char
  getChar(std::string const& file_text) const noexcept {
    assume_assert(type == TokenType::CHAR_LITERAL);
    return file_text[pos];
  }

  [[nodiscard]] constexpr std::string_view
  getString(std::string const& file_text) const noexcept {
    assume_assert(type == TokenType::STRING_LITERAL or type == TokenType::IDENTIFIER);
    return {file_text.data() + pos, length};
  }

  void throw_if(TokenType unwanted_type, const char* err_msg) const;
  void throw_if_not(TokenType expected_type, const char* err_msg) const;

  [[nodiscard]] constexpr TokenType getType() const noexcept           { return type; }
  [[nodiscard]] constexpr bool is(TokenType token_type) const noexcept { return type == token_type; }
  [[nodiscard]] constexpr bool isPrimitive() const noexcept            { return isCategoryPRIMITIVES(type); }
  [[nodiscard]] constexpr bool isLiteral() const noexcept              { return isCategoryLITERALS(type); }
  [[nodiscard]] constexpr bool isPointer() const noexcept              { return isCategoryPOINTERS(type); }
  [[nodiscard]] constexpr bool isTypeModifier() const noexcept         { return isCategoryTYPE_MODIFIERS(type); }

};

class TokenView {
  using TokenIter = std::vector<Token>::iterator;
  TokenIter begin;
  TokenIter end;

public:
  TokenView(TokenIter begin, TokenIter end) : begin(begin), end(end) {}

  [[nodiscard]] Token const& peek() const noexcept                       { return *begin; }
  [[nodiscard]] bool peek_is(TokenType type) const noexcept              { return begin->type == type; }
  [[nodiscard]] Token const& peek_ahead(sz_t distance) const noexcept    { return *(begin + distance); }
  [[nodiscard]] Token take() noexcept                                    { return *begin++; }
  [[nodiscard]] bool empty() const noexcept                              { return begin == end; }
  void pop() noexcept                                                    { assert(not empty()); ++begin; }
  bool pop_if(const TokenType type) noexcept                             { if (empty() or begin->type not_eq type) return false; ++begin; return true; }

  void expect_then_pop(TokenType expected_type, const char* err_msg);
  void expect(TokenType expected_type, const char* err_msg) const;
  void reject_then_pop(TokenType unwanted_type, const char* err_msg);
  void reject(TokenType unwanted_type, const char* err_msg) const;
  void print(unsigned initial_indent = 0) const;


  // expects that opening_token has already been popped
  // does NOT include opening or closing token in returned view
  // will pop closing_token, do not use for opening / closing tokens with a value that matters to you, it WILL be lost
  [[nodiscard]] TokenView getTokensBetween(TokenType opening_token, TokenType closing_token);
  [[nodiscard]] TokenView getTokensBetweenBraces() { return getTokensBetween(TokenType::LBRACE, TokenType::RBRACE); }
  [[nodiscard]] TokenView getTokensBetweenParenthesis() { return getTokensBetween(TokenType::LPAREN, TokenType::RPAREN); }
  [[nodiscard]] TokenView getTokensBetweenBrackets() { return getTokensBetween(TokenType::LBRACKET, TokenType::RBRACKET); }
  [[nodiscard]] TokenView getTokensBetweenAngleBrackets() { return getTokensBetween(TokenType::LESS, TokenType::GTR); }

  // return does not include the type specified
  [[nodiscard]] TokenView getAllTokensUntilFirstOf(TokenType type);
};

// returns the text of the file and populates out_tokens
[[nodiscard]] std::string
tokenizeFile(std::vector<Token>& out_tokens, std::filesystem::path const& file_path);
}
