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

  [[nodiscard]] constexpr std::string_view
  originalString(File const& file) const noexcept
  { return file.view_at(position, length); }

  [[nodiscard]] constexpr bool is(TokenType token_type) const noexcept { return type == token_type; }
  [[nodiscard]] constexpr bool isIdentifier() const noexcept           { return type == TokenType::IDENTIFIER; }
  [[nodiscard]] constexpr bool isPrimitive() const noexcept            { return isCategoryPRIMITIVES(type); }
  [[nodiscard]] constexpr bool isLiteral() const noexcept              { return isCategoryLITERALS(type); }
  [[nodiscard]] constexpr bool isPointer() const noexcept              { return isCategoryPOINTERS(type); }
  [[nodiscard]] constexpr bool isTypeQualifier() const noexcept        { return isCategoryTYPE_QUALIFIER(type); }

};

class TokenView {
  using TokenIter = std::vector<Token>::const_iterator;
  TokenIter begin;
  TokenIter end;

public:
  explicit TokenView(std::vector<Token> const& tokens) noexcept
  : begin(tokens.begin()), end(tokens.end()) {}

  TokenView(TokenIter begin, TokenIter end) noexcept
  : begin(begin), end(end) {}

  [[nodiscard]] Token peek() const noexcept                     { return *begin; }
  [[nodiscard]] bool peek_is(TokenType type) const noexcept     { return begin->type == type; }
  [[nodiscard]] Token peek_ahead(sz_t distance) const noexcept  { return *(begin + distance); }
  [[nodiscard]] Token take() noexcept                           { return *begin++; }
  // [[nodiscard]] bool empty() const noexcept                     { return begin == end; }
  void pop() noexcept                                           { ++begin; }
  bool pop_if(const TokenType type) noexcept                    { if (begin->type not_eq type) return false; ++begin; return true; }
  void undo() noexcept                                          { --begin; }
  Token previous() const noexcept   /* ub if first token */     { return *(begin - 1); }

  void print(File const& file) const;

  [[nodiscard]] Token viewAsStringToken() const noexcept {
    return {TokenType::STRING_LITERAL,
      static_cast<u16_t>((end-1)->position - begin->position + static_cast<u32_t>(end->length)),
      begin->position
    };
  }


  // expects that opening_token has already been popped
  // does NOT include opening or closing token in returned view
  // will pop closing_token, do not use for opening / closing tokens with a value that matters to you, it WILL be lost
  // [[nodiscard]] TokenView getTokensBetween(TokenType opening_token, TokenType closing_token);
  // [[nodiscard]] TokenView getTokensBetweenBraces() { return getTokensBetween(TokenType::LBRACE, TokenType::RBRACE); }
  // [[nodiscard]] TokenView getTokensBetweenParenthesis() { return getTokensBetween(TokenType::LPAREN, TokenType::RPAREN); }
  // [[nodiscard]] TokenView getTokensBetweenBrackets() { return getTokensBetween(TokenType::LBRACKET, TokenType::RBRACKET); }
  // [[nodiscard]] TokenView getTokensBetweenAngleBrackets() { return getTokensBetween(TokenType::LESS, TokenType::GTR); }

  // return does not include the type specified
  [[nodiscard]] TokenView getAllTokensUntilFirstOf(TokenType type);
};

inline constexpr auto INVALID_TOKEN_PADDING = 8uz;

// assumes the file at file_path is not empty
// returns the text of the file and populates out_tokens
// out_tokens is padded with invalid tokens to prevent parser from repeatedly checking empty
[[nodiscard]] File
tokenizeFile(std::vector<Token>& out_tokens, std::filesystem::path const& file_path);

}
