#pragma once
#include "edenlib/typedefs.hpp"
#include "file.hpp"
#include "tokentype.hpp"

#include "edenlib/shortened_macros.hpp"
#include "edenlib/vectors/vector.hpp"
#include <span>
#include <utility>

namespace LOM::Lexer {

struct Token {
  TokenType type;
  byte_t _pad[1]{};
  u16_t length;
  u32_t position;

  constexpr Token() = default;

  constexpr Token(TokenType type, u16_t length, u32_t position)
  : type(type), length(length), position(position) {}

  NodiscardCXPR static Token
  combine(Token leftmost, Token rightmost) noexcept {
    return {
      leftmost.type,
      u16_t(rightmost.position - leftmost.position + rightmost.length),
      leftmost.position
    };
  }

  // expensive
  NodiscardCXPR u64_t
  getInteger(File const& file) const noexcept {
    assert(type == TokenType::INTEGER_LITERAL);
    auto const begin = file.getText().data() + position;
    u64_t res;
    [[maybe_unused]] auto const from_chars_res = std::from_chars(begin, begin + length, res);
    assert(from_chars_res.ec == std::errc());
    return res;
  }

  // expensive
  NodiscardCXPR float
  getFloat(File const& file) const noexcept {
    assert(type == TokenType::FLOAT_LITERAL);
    auto const begin = file.getText().data() + position;
    float res;
    [[maybe_unused]] auto const from_chars_res = std::from_chars(begin, begin + length, res);
    assert(from_chars_res.ec == std::errc());
    return res;
  }

  // expensive
  NodiscardCXPR double
  getDouble(File const& file) const noexcept {
    assert(type == TokenType::DOUBLE_LITERAL);
    auto const begin = file.getText().data() + position;
    double res;
    [[maybe_unused]] auto const from_chars_res = std::from_chars(begin, begin + length, res);
    assert(from_chars_res.ec == std::errc());
    return res;
  }

  NodiscardCXPR bool
  getBool(File const& file) const noexcept {
    assert(type == TokenType::BOOL_LITERAL);
    return file.getText()[position] == 't';
  }

  NodiscardCXPR char
  getChar(File const& file) const noexcept {
    assert(type == TokenType::CHAR_LITERAL);
    return file.getText()[position];
  }

  NodiscardCXPR char
  getEscapedChar(File const& file) const noexcept {
    assert(type == TokenType::ESCAPED_CHAR_LITERAL);
    assert(file.getText()[position] == '\\');

    auto const c = file.getText()[position + 1];
    switch (c) {
    case '\'': return '\'';
    case '"':  return '\"';
    case '?':  return '\?';
    case '\\': return '\\';
    case 'a':  return '\a';
    case 'b':  return '\b';
    case 'f':  return '\f';
    case 'n':  return '\n';
    case 'r':  return '\r';
    case 't':  return '\t';
    case 'v':  return '\v';
    case '0':  return '\0';

    default:   return c; // not the best behavior but wtv
    }
  }

  NodiscardCXPR std::string_view
  getString(File const& file) const noexcept {
    assert(type == TokenType::STRING_LITERAL);
    return file.viewAt(length, position);
  }

  NodiscardCXPR std::string_view
  originalString(File const& file) const noexcept
  { return file.viewAt(length, position); }

  InlineNodiscardCXPR bool is(TokenType token_type) const noexcept { return type == token_type; }
  InlineNodiscardCXPR bool isIdentifier()           const noexcept { return type == TokenType::IDENTIFIER; }
  InlineNodiscardCXPR bool isPrimitive()            const noexcept { return isCategoryPRIMITIVES(type); }
  InlineNodiscardCXPR bool isLiteral()              const noexcept { return isCategoryLITERALS(type); }
  InlineNodiscardCXPR bool isNumericLiteral()       const noexcept { return isCategoryNUMERIC_LITERALS(type); }
  InlineNodiscardCXPR bool isPointer()              const noexcept { return isCategoryPOINTERS(type); }
  InlineNodiscardCXPR bool isVarQualifier()         const noexcept { return isCategoryVAR_QUALIFIERS(type); }
  InlineNodiscardCXPR bool isInvalid()              const noexcept { return type == TokenType::INVALID_TOKEN; }

};

class TokenView {
  using TokenIter = std::span<Token>::iterator;
  TokenIter begin;
  TokenIter end;

public:
  InlineCXPR explicit TokenView(std::span<Token> tokens) noexcept
  : begin(tokens.begin()), end(tokens.end()) {}

  InlineCXPR TokenView(TokenIter begin, TokenIter end) noexcept
  : begin(begin), end(end) {}

  InlineNodiscardCXPR Token peek()                    const noexcept  { return *begin; }
  InlineNodiscardCXPR bool  peek_is(TokenType type)   const noexcept  { return begin->type == type; }
  InlineNodiscardCXPR Token peek_ahead(long distance) const noexcept  { return *(begin + distance); }
  InlineCXPR          void  set_peek(TokenType type)        noexcept  { begin->type = type; }
  InlineNodiscardCXPR Token take()                          noexcept  { return *begin++; }
  InlineNodiscardCXPR Token previous()                const noexcept  { return *(begin - 1); }
  InlineCXPR          void  pop()                           noexcept  { ++begin; }
  InlineCXPR          bool  pop_if(TokenType type)          noexcept  { if (begin->type not_eq type) return false; ++begin; return true; }
  InlineCXPR          void  undo()                          noexcept  { --begin; }
  InlineCXPR          void  pop_if_valid()                  noexcept { if (not begin->is(TokenType::INVALID_TOKEN)) ++begin; }

  InlineNodiscardCXPR Token
  take_if_valid() noexcept {
    auto const res = *begin;
    if (not res.is(TokenType::INVALID_TOKEN)) ++begin;
    return res;
  }

  NodiscardCXPR Token
  viewAsStringToken() const noexcept {
    return {
      TokenType::STRING_LITERAL,
      u16_t((end-1)->position - begin->position + u32_t(end->length)),
      begin->position
    };
  }

};

static constexpr sz_t INVALID_TOKEN_PADDING = 8;

// Returns whether an error occured.
// Populates out_tokens and pads with invalid tokens.
[[nodiscard]] bool tokenizeFile(eden::vector<Token>& out_tokens, File file) noexcept;

}
