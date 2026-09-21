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

  edenNodiscardCXPR static Token
  combine(Token leftmost, Token rightmost) noexcept {
    return {
      leftmost.type,
      u16_t(rightmost.position - leftmost.position + rightmost.length),
      leftmost.position
    };
  }

  // expensive
  edenNodiscardCXPR u64_t
  getInteger(File const& file) const noexcept {
    assert(type == TokenType::INTEGER_LITERAL);
    auto const begin = file.get_text().data() + position;
    u64_t res;
    [[maybe_unused]] auto const from_chars_res = std::from_chars(begin, begin + length, res);
    assert(from_chars_res.ec == std::errc());
    return res;
  }

  // expensive
  edenNodiscardCXPR float
  getFloat(File const& file) const noexcept {
    assert(type == TokenType::FLOAT_LITERAL);
    auto const begin = file.get_text().data() + position;
    float res;
    [[maybe_unused]] auto const from_chars_res = std::from_chars(begin, begin + length, res);
    assert(from_chars_res.ec == std::errc());
    return res;
  }

  // expensive
  edenNodiscardCXPR double
  getDouble(File const& file) const noexcept {
    assert(type == TokenType::DOUBLE_LITERAL);
    auto const begin = file.get_text().data() + position;
    double res;
    [[maybe_unused]] auto const from_chars_res = std::from_chars(begin, begin + length, res);
    assert(from_chars_res.ec == std::errc());
    return res;
  }

  edenNodiscardCXPR bool
  getBool(File const& file) const noexcept {
    assert(type == TokenType::BOOL_LITERAL);
    return file.get_text()[position] == 't';
  }

  edenNodiscardCXPR char
  getChar(File const& file) const noexcept {
    assert(type == TokenType::CHAR_LITERAL);
    return file.get_text()[position]; //TODO: incorrect, doesn't account for escape sequences
  }

  edenNodiscardCXPR std::string_view
  getString(File const& file) const noexcept {
    assert(type == TokenType::STRING_LITERAL);
    return file.view_at(length, position);
  }

  edenNodiscardCXPR std::string_view
  originalString(File const& file) const noexcept
  { return file.view_at(length, position); }

  edenInlineNodiscardCXPR bool is(TokenType token_type) const noexcept { return type == token_type; }
  edenInlineNodiscardCXPR bool isIdentifier()           const noexcept { return type == TokenType::IDENTIFIER; }
  edenInlineNodiscardCXPR bool isPrimitive()            const noexcept { return isCategoryPRIMITIVES(type); }
  edenInlineNodiscardCXPR bool isLiteral()              const noexcept { return isCategoryLITERALS(type); }
  edenInlineNodiscardCXPR bool isNumericLiteral()       const noexcept { return isCategoryNUMERIC_LITERALS(type); }
  edenInlineNodiscardCXPR bool isPointer()              const noexcept { return isCategoryPOINTERS(type); }
  edenInlineNodiscardCXPR bool isVarQualifier()         const noexcept { return isCategoryVAR_QUALIFIERS(type); }
  edenInlineNodiscardCXPR bool isInvalid()              const noexcept { return type == TokenType::INVALID_TOKEN; }

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

  edenInlineNodiscardCXPR Token peek()                    const noexcept  { return *begin; }
  edenInlineNodiscardCXPR bool  peek_is(TokenType type)   const noexcept  { return begin->type == type; }
  edenInlineNodiscardCXPR Token peek_ahead(long distance) const noexcept  { return *(begin + distance); }
  edenInlineCXPR          void  set_peek(TokenType type)        noexcept  { begin->type = type; }
  edenInlineNodiscardCXPR Token take()                          noexcept  { return *begin++; }
  edenInlineNodiscardCXPR Token previous()                const noexcept  { return *(begin - 1); }
  edenInlineCXPR          void  pop()                           noexcept  { ++begin; }
  edenInlineCXPR          bool  pop_if(TokenType type)          noexcept  { if (begin->type not_eq type) return false; ++begin; return true; }
  edenInlineCXPR          void  undo()                          noexcept  { --begin; }
  edenInlineCXPR          void  pop_if_valid()                  noexcept { if (not begin->is(TokenType::INVALID_TOKEN)) ++begin; }

  edenInlineNodiscardCXPR Token
  take_if_valid() noexcept {
    auto const res = *begin;
    if (not res.is(TokenType::INVALID_TOKEN)) ++begin;
    return res;
  }

  void print(File const& file) const;

  edenNodiscardCXPR Token
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
[[nodiscard]] bool tokenizeFile(eden::vector<Token>& out_tokens, File file) noexcept;

}
