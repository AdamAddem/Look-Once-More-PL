#pragma once
#include "edenlib/macros.hpp"
#include "edenlib/typedefs.hpp"
#include "file.hpp"
#include "tokentype.hpp"

#include "edenlib/vectors/vector.hpp"
#include <utility>

namespace LOM::Lexer {

struct Token {
  TokenType type;
  u16_t length;
  u32_t position;

  [[nodiscard]] static constexpr Token
  combine(Token leftmost, Token rightmost) noexcept {
    return {
      leftmost.type,
      u16_t(rightmost.position - leftmost.position + rightmost.length),
      leftmost.position
    };
  }

  // expensive
  [[nodiscard]] constexpr u64_t
  getInteger(File const& file) const noexcept {
    assert(type == TokenType::INTEGER_LITERAL);
    auto const begin = file.get_text().data() + position;
    u64_t res;
    [[maybe_unused]] auto const from_chars_res = std::from_chars(begin, begin + length, res);
    assert(from_chars_res.ec == std::errc());
    return res;
  }

  // expensive
  [[nodiscard]] constexpr float
  getFloat(File const& file) const noexcept {
    assert(type == TokenType::FLOAT_LITERAL);
    auto const begin = file.get_text().data() + position;
    float res;
    [[maybe_unused]] auto const from_chars_res = std::from_chars(begin, begin + length, res);
    assert(from_chars_res.ec == std::errc());
    return res;
  }

  // expensive
  [[nodiscard]] constexpr double
  getDouble(File const& file) const noexcept {
    assert(type == TokenType::DOUBLE_LITERAL);
    auto const begin = file.get_text().data() + position;
    double res;
    [[maybe_unused]] auto const from_chars_res = std::from_chars(begin, begin + length, res);
    assert(from_chars_res.ec == std::errc());
    return res;
  }

  [[nodiscard]] constexpr bool
  getBool(File const& file) const noexcept {
    assert(type == TokenType::BOOL_LITERAL);
    return file.get_text()[position] == 't';
  }

  [[nodiscard]] constexpr char
  getChar(File const& file) const noexcept {
    assert(type == TokenType::CHAR_LITERAL);
    return file.get_text()[position]; //TODO: incorrect, doesn't account for escape sequences
  }

  [[nodiscard]] constexpr std::string_view
  getString(File const& file) const noexcept {
    assert(type == TokenType::STRING_LITERAL);
    return file.view_at(length, position);
  }

  [[nodiscard]] constexpr std::string_view
  originalString(File const& file) const noexcept
  { return file.view_at(length, position); }

  edenAlwaysInline [[nodiscard]] constexpr bool is(TokenType token_type) const noexcept { return type == token_type; }
  edenAlwaysInline [[nodiscard]] constexpr bool isIdentifier()           const noexcept { return type == TokenType::IDENTIFIER; }
  edenAlwaysInline [[nodiscard]] constexpr bool isPrimitive()            const noexcept { return isCategoryPRIMITIVES(type); }
  edenAlwaysInline [[nodiscard]] constexpr bool isLiteral()              const noexcept { return isCategoryLITERALS(type); }
  edenAlwaysInline [[nodiscard]] constexpr bool isNumericLiteral()       const noexcept { return isCategoryNUMERIC_LITERALS(type); }
  edenAlwaysInline [[nodiscard]] constexpr bool isPointer()              const noexcept { return isCategoryPOINTERS(type); }
  edenAlwaysInline [[nodiscard]] constexpr bool isVarQualifier()         const noexcept { return isCategoryVAR_QUALIFIERS(type); }
  edenAlwaysInline [[nodiscard]] constexpr bool isInvalid()              const noexcept { return type == TokenType::INVALID_TOKEN; }

};

class TokenView {
  using TokenIter = eden::vector<Token>::iterator;
  TokenIter begin;
  TokenIter end;

public:
  explicit TokenView(eden::vector<Token>& tokens) noexcept
  : begin(tokens.begin()), end(tokens.end()) {}

  TokenView(TokenIter begin, TokenIter end) noexcept
  : begin(begin), end(end) {}

  edenAlwaysInline [[nodiscard]] Token  peek()                    const noexcept  { return *begin; }
  edenAlwaysInline [[nodiscard]] bool   peek_is(TokenType type)   const noexcept  { return begin->type == type; }
  edenAlwaysInline [[nodiscard]] Token  peek_ahead(long distance) const noexcept  { return *(begin + distance); }
  edenAlwaysInline               void   set_peek(TokenType type)        noexcept  { begin->type = type; }
  edenAlwaysInline [[nodiscard]] Token  take()                          noexcept  { return *begin++; }
  edenAlwaysInline [[nodiscard]] Token  previous()                const noexcept  { return *(begin - 1); }
  edenAlwaysInline               void   pop()                           noexcept  { ++begin; }
  edenAlwaysInline               bool   pop_if(TokenType type)          noexcept  { if (begin->type not_eq type) return false; ++begin; return true; }
  edenAlwaysInline               void   undo()                          noexcept  { --begin; }

  void pop_if_valid() noexcept
  { if (not begin->is(TokenType::INVALID_TOKEN)) ++begin; }

  [[nodiscard]] Token
  take_if_valid() noexcept {
    auto const res = *begin;
    if (not res.is(TokenType::INVALID_TOKEN)) ++begin;
    return res;
  }

  void print(File const& file) const;

  [[nodiscard]] Token
  viewAsStringToken() const noexcept {
    return {TokenType::STRING_LITERAL,
      static_cast<u16_t>((end-1)->position - begin->position + static_cast<u32_t>(end->length)),
      begin->position
    };
  }

};

inline constexpr auto INVALID_TOKEN_PADDING = 8uz;

// Returns whether an error occured.
// Populates out_tokens and pads with invalid tokens.
[[nodiscard]] bool
tokenizeFile(eden::vector<Token>& out_tokens, File file);

}
