#pragma once
#include "file.hpp"
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
  u32_t position;

  // expensive
  [[nodiscard]] constexpr i64_t
  getSigned(File const& file) const noexcept {
    assume_assert(type == TokenType::SIGNED_LITERAL);
    auto const begin = file.contents().data() + position;
    i64_t res;
    auto const from_chars_res = std::from_chars(begin, begin + length, res);
    assert(from_chars_res.ec == std::errc());
    return res;
  }

  // expensive
  [[nodiscard]] constexpr u64_t
  getUnsigned(File const& file) const noexcept {
    assume_assert(type == TokenType::UNSIGNED_LITERAL);
    auto const begin = file.contents().data() + position;
    u64_t res;
    auto const from_chars_res = std::from_chars(begin, begin + length, res);
    assert(from_chars_res.ec == std::errc());
    return res;
  }

  // expensive
  [[nodiscard]] constexpr float
  getFloat(File const& file) const noexcept {
    assume_assert(type == TokenType::FLOAT_LITERAL);
    auto const begin = file.contents().data() + position;
    float res;
    auto const from_chars_res = std::from_chars(begin, begin + length, res);
    assert(from_chars_res.ec == std::errc());
    return res;
  }

  // expensive
  [[nodiscard]] constexpr double
  getDouble(File const& file) const noexcept {
    assume_assert(type == TokenType::DOUBLE_LITERAL);
    auto const begin = file.contents().data() + position;
    double res;
    auto const from_chars_res = std::from_chars(begin, begin + length, res);
    assert(from_chars_res.ec == std::errc());
    return res;
  }

  [[nodiscard]] constexpr bool
  getBool(File const& file) const noexcept {
    assume_assert(type == TokenType::BOOL_LITERAL);
    return file.contents()[position] == 't';
  }

  [[nodiscard]] constexpr char
  getChar(File const& file) const noexcept {
    assume_assert(type == TokenType::CHAR_LITERAL);
    return file.contents()[position]; //TODO: incorrect, doesn't account for escape sequences
  }

  [[nodiscard]] constexpr std::string_view
  getString(File const& file) const noexcept {
    assume_assert(type == TokenType::STRING_LITERAL or type == TokenType::IDENTIFIER);
    return file.view_at(position, length);
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
  using TokenIter = std::vector<Token>::const_iterator;
  TokenIter begin;
  TokenIter end;

public:
  explicit TokenView(std::vector<Token> const& tokens) noexcept
  : begin(tokens.begin()), end(tokens.end()) {}

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
  void print(File& file) const;

  [[nodiscard]] Token viewAsStringToken() const noexcept {
    if (empty()) return {TokenType::INVALID_TOKEN, 0, 0};

    return {TokenType::STRING_LITERAL,
      static_cast<u16_t>((end-1)->position - begin->position + static_cast<u32_t>(end->length)),
      begin->position
    };
  }


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
[[nodiscard]] std::optional<File>
tokenizeFile(std::vector<Token>& out_tokens, std::filesystem::path const& file_path);
}
