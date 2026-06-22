#include "edenlib/vectors/releasing_vector.hpp"
#include "lex.hpp"
#include "error.hpp"

#include <cassert>
#include <cctype>
#include <unordered_map>
#include <vector>

namespace {
using namespace LOM;
using namespace LOM::Lexer;
using eden::releasing_string;
using eden::flags::reserve_initial;

[[nodiscard]] bool
canStartIdentifier(char c)
{ return std::isalpha(c) or c == '_'; }

[[nodiscard]] bool
is_num(char c)
{ return c >= '0' and c <= '9'; }

struct Tokenizer {
  std::vector<Token>& token_list;
  File file;
  u32_t current_position{};
  bool error_found{};

  static constexpr char FILE_EOF = '\0';
  explicit Tokenizer(std::vector<Token>& token_list, std::filesystem::path const& path)
  : token_list(token_list), file(path) {}

  [[nodiscard]] char
  peek() const noexcept { return file.contents()[current_position]; }

  [[nodiscard]] char
  pop() noexcept { return file.contents()[current_position++]; }

  void skip() noexcept { ++current_position; }
  void undo() noexcept { --current_position; }

  void report_error_at_currentpos(const char* msg) {
    report_lexing_error(file, current_position, 1, msg);
    error_found = true;
  }

  [[nodiscard]] char
  charToEscapeSequenceEquivalent(char c) {
    switch (c) {
    case 'n':   return '\n';
    case 't':   return '\t';
    case 'b':   return '\b';
    case 'r':   return '\r';
    case 'f':   return '\f';
    case '\\':  return '\\';
    case '"':   return '"';
    case '\'':  return '\'';
    case '0':   return '\0';
    case 'v':   return '\v';

    default:
      report_lexing_error(file, current_position - 1, 2, "Unknown escape sequence.");
      error_found = true;
      return '?';
    }
  }

  // called when opening quotes already consumed
  void grabStringLiteral() {
    u16_t length = 0;
    auto const pos = current_position; // grabbing the position after opening quotes
    auto c = pop();
    while (c not_eq FILE_EOF) {
      switch (c) {
      case '\"': goto ending_quote_found;

      case '\n':
      case FILE_EOF:
        report_error_at_currentpos("Expected ending \" in string literal.");
        goto ending_quote_found;
      default:
        ++length;
      }
      c = pop();
    }

    ending_quote_found: // don't crucify me for this pls
      token_list.emplace_back(TokenType::STRING_LITERAL, length, pos);
  }

  // called when opening single-quote already consumed
  void grabCharLiteral() {
    u16_t length = 2; auto const pos = current_position;
    auto const c1 = pop();
    auto const c2 = pop();

    if (c1 == '\\') {
      ++length;
      if (pop() not_eq '\'')
        report_error_at_currentpos("Expected ending ' in char literal.");
    } else if (c2 not_eq '\'')
        report_error_at_currentpos("Expected ending ' in char literal.");

    token_list.emplace_back(TokenType::CHAR_LITERAL, length, pos);
  }

  void grabSymbol() {
    TokenType type;
    u16_t length = 1;
    auto const pos = current_position;
    auto const c = pop();
    auto const peeked = peek();
    switch (c) { using enum TokenType;
    case '+':
      if (peeked == '+') { skip(); type = PLUSPLUS; length = 2; }
      else type = PLUS;
      break;
    case '-':
      if (peeked == '-') { skip(); type = MINUSMINUS; length = 2; }
      else if (peeked == '>') { skip(); type = ARROW; length = 2; }
      else type = MINUS;
      break;
    case '<':
      if (peeked == '=') { skip(); type = LESSEQ; length = 2;  }
      else type = LESS;
      break;
    case '>':
      if (peeked == '=') { skip(); type = GTREQ; length = 2;  }
      else type = GTR;
      break;
    case '!':
      if (peeked == '=') { skip(); type = KEYWORD_NOT_EQUAL; length = 2;  }
      else report_error_at_currentpos("! token only supported in !=, use keyword 'not' instead.");
      break;
    case '=':
      if (peeked == '=') { skip(); type = KEYWORD_EQUALS; length = 2;  }
      else type = ASSIGN;
      break;

    case '/': type = SLASH; break;
    case '*': type = STAR; break;
    case '%': type = MOD; break;
    case '(': type = LPAREN; break;
    case ')': type = RPAREN; break;
    case '{': type = LBRACE; break;
    case '}': type = RBRACE; break;
    case '[': type = LBRACKET; break;
    case ']': type = RBRACKET; break;
    case '@': type = ADDR; break;
    case ',': type = COMMA; break;
    case '.': type = DOT; break;
    case ':': type = COLON; break;
    case ';': type = SEMI_COLON; break;
    case '\"': return grabStringLiteral();
    case '\'': return grabCharLiteral();

    default:
      type = INVALID_TOKEN;
      report_error_at_currentpos("Invalid symbol.");
    }

    token_list.emplace_back(type, length, pos);
  }

  void grabNumber() {
    auto c = peek();
    bool const is_negative = c == '-';
    TokenType newtoken_type = is_negative ? TokenType::SIGNED_LITERAL : TokenType::UNSIGNED_LITERAL;
    u16_t newtoken_length = 0;
    auto const newtoken_pos = static_cast<u32_t>(current_position);

    while ((c = pop()) not_eq '\0') {
      if (c == 'f') {
        newtoken_type = TokenType::FLOAT_LITERAL;
        break;
      }
      if (c == '.') {
        newtoken_type = TokenType::DOUBLE_LITERAL;
        continue;
      }
      if (not is_num(c)) {
        undo();
        break;
      }

      ++newtoken_length;
    }

    token_list.emplace_back(newtoken_type, newtoken_length, newtoken_pos);
  }

  // first letter in front of file
  void grabIdentOrKeyword() {
    Token new_token{TokenType::INVALID_TOKEN, 0, static_cast<u32_t>(current_position)};
    auto c = pop();

    while (c not_eq '\0') {
      if (not std::isalnum(c) and c not_eq '_') break;

      c = pop();
      ++new_token.length;
    }

    undo();
    const std::string_view word_view{file.contents().data() + new_token.length, new_token.length};

    if (stringToTokenType.contains(word_view))
      new_token.type = stringToTokenType.at(word_view);

    else if (word_view == "elif") {
      new_token.type = TokenType::KEYWORD_ELSE; token_list.emplace_back(new_token);
      new_token.type = TokenType::KEYWORD_IF;
    }
    else if (word_view == "true" or word_view == "false")
      new_token.type = TokenType::BOOL_LITERAL;
    else
      new_token.type = TokenType::IDENTIFIER;

    token_list.emplace_back(new_token);
  }

  void skipWS() {
    while (std::isspace(peek())) {
      auto const c = pop();
      if (c == '\0') return;
    }
  }

  [[nodiscard]] bool
  skipComments() {
    skip();
    if (peek() == '/') {
      skip();

      auto c = pop();
      while ( c not_eq '\n' and c not_eq '\0' ) c = pop();
      return true;
    }

    undo();
    return false;
  }
};

}

std::optional<File>
Lexer::tokenizeFile(std::vector<Token>& out_tokens, std::filesystem::path const& path) {
  Tokenizer tokenizer{out_tokens, path};

  while (true) {
    tokenizer.skipWS();
    auto const c = tokenizer.peek();
    if (c == '\0') break;
    if (c == '/' and tokenizer.skipComments()) continue;

    if (is_num(c))
      tokenizer.grabNumber();
    else if (canStartIdentifier(c))
      tokenizer.grabIdentOrKeyword();
    else
      tokenizer.grabSymbol();
  }

  if (tokenizer.error_found)
    return std::nullopt;
  return std::move(tokenizer.file);
}