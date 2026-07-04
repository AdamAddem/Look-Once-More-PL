#include "edenlib/vectors/releasing_vector.hpp"
#include "lex.hpp"
#include "error.hpp"

#include <cassert>
#include <cctype>
#include <unordered_map>
#include <vector>
#include <chrono>

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

  static constexpr char FILE_EOF = '\0';
  explicit Tokenizer(std::vector<Token>& token_list, std::filesystem::path const& path)
  : token_list(token_list), file(path) {}

  [[nodiscard]] char
  peek() const noexcept { return file.contents()[current_position]; }

  [[nodiscard]] char
  peek_ahead(sz_t i = 1) const noexcept { return file.contents()[current_position + i]; }

  [[nodiscard]] char
  previous() const noexcept {
    assert(current_position not_eq 0);
    return file.contents()[current_position - 1];
  }

  [[nodiscard]] char
  take() noexcept { return file.contents()[current_position++]; }

  void pop() noexcept { ++current_position; }
  void undo() noexcept { --current_position; }

  void report_error_at_currentpos(const char* msg) const {
    report_error(file, 1, current_position, msg);
  }

  // called when opening quotes already consumed
  void grabStringLiteral() {
    u16_t length = 0;
    auto const pos = current_position; // grabbing the position after opening quotes
    auto c = take();

    auto string_type = TokenType::STRING_LITERAL;
    while (c not_eq FILE_EOF) {
      switch (c) {
      case '\"': goto ending_quote_found;
      case '\n':
      case FILE_EOF:
        report_error_at_currentpos("Expected ending \" in string literal.");
        goto ending_quote_found;

      case '\\': string_type = TokenType::ESCAPED_STRING_LITERAL; [[fallthrough]];
      default: break;
      }
      ++length;
      c = take();
    }

    ending_quote_found: // don't crucify me for this pls
      token_list.emplace_back(string_type, length, pos);
  }

  // called when opening single-quote already consumed
  void grabCharLiteral() {
    u16_t length = 2; auto const pos = current_position;
    auto const c1 = take();
    auto const c2 = take();

    if (c1 == '\\') {
      ++length;
      if (take() not_eq '\'')
        report_error_at_currentpos("Expected ending ' in char literal.");
    } else if (c2 not_eq '\'')
        report_error_at_currentpos("Expected ending ' in char literal.");

    token_list.emplace_back(TokenType::CHAR_LITERAL, length, pos);
  }

  void grabSymbol() {
    TokenType type;
    u16_t length = 1;
    auto const pos = current_position;
    auto const c = take();
    auto const peeked = peek();
    switch (c) { using enum TokenType;
    case '+':
      if (peeked == '+') { pop(); type = PLUSPLUS; length = 2; }
      else type = PLUS;
      break;
    case '-':
      if (peeked == '-') { pop(); type = MINUSMINUS; length = 2; }
      else if (peeked == '>') { pop(); type = ARROW; length = 2; }
      else type = MINUS;
      break;
    case '<':
      if (peeked == '=') { pop(); type = LESSEQ; length = 2;  }
      else type = LESS;
      break;
    case '>':
      if (peeked == '=') { pop(); type = GTREQ; length = 2;  }
      else type = GTR;
      break;
    case '!':
      if (peeked == '=') { pop(); type = KEYWORD_NOT_EQUAL; length = 2;  }
      else type = KEYWORD_NOT;
      break;
    case '=':
      if (peeked == '=') { pop(); type = KEYWORD_EQUALS; length = 2;  }
      else type = ASSIGN;
      break;

    case '/': type = SLASH; break;
    case '*': type = STAR; break;
    case '%': type = MOD; break;
    case '(': type = LPAREN; break;
    case ')': type = RPAREN; break;
    case '{': type = LBRACE; break;
    case '}': type = RBRACE; break;
    //case '[': type = LBRACKET; break;
    //case ']': type = RBRACKET; break; // re-implement when adding arrays
    case '@': type = ADDR; break;
    case ',': type = COMMA; break;
    case '.': type = DOT; break;
    case ':': type = COLON; break;
    case ';': type = SEMI_COLON; break;
    case '$': type = KEYWORD_MUT; break;
    case '\"': return grabStringLiteral();
    case '\'': return grabCharLiteral();

    default:
      type = INVALID_TOKEN;
      report_error_at_currentpos("Invalid symbol.");
    }

    token_list.emplace_back(type, length, pos);
  }

  void grabNumber() {
    auto newtoken_type = TokenType::INTEGER_LITERAL;
    u16_t newtoken_length = 0;
    auto const newtoken_pos = current_position;

    while (peek() not_eq '\0') {
      auto const c = take();
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

  void grabIdentOrKeyword() {
    Token new_token{TokenType::INVALID_TOKEN, 0, current_position};
    auto c = take();

    while (c not_eq '\0') {
      if (not std::isalnum(c) and c not_eq '_') break;

      c = take();
      ++new_token.length;
    }
    undo();

    const std::string_view word_view{file.contents().data() + new_token.position, new_token.length};
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
      auto const c = take();
      if (c == '\0') return;
    }
  }

#define pre assert(peek() == '#');
  void skipComments() { pre
    pop();

    if (peek() not_eq ':') {
      while ( peek() not_eq '\n' and peek() not_eq FILE_EOF ) pop();
      return;
    }

    pop();
    auto nested{1uz};
    while (peek() not_eq FILE_EOF and nested > 0) {
      auto const first = peek();
      auto const second = peek_ahead();

      if (first == '#' and second == ':')
        ++nested, pop();
      else if (first == ':' and second == '#')
        --nested, pop();

      pop();
    }

  }
#undef pre
};

}

File Lexer::tokenizeFile(std::vector<Token>& out_tokens, std::filesystem::path const& file_path) {
#ifndef NDEBUG
  auto begin_time = std::chrono::high_resolution_clock::now();
#endif

  Tokenizer tokenizer{out_tokens, file_path};

  while (true) {
    tokenizer.skipWS();
    auto const c = tokenizer.peek();
    if (c == Tokenizer::FILE_EOF) break;
    if (c == '#') { tokenizer.skipComments(); continue; }

    if (is_num(c))
      tokenizer.grabNumber();
    else if (canStartIdentifier(c))
      tokenizer.grabIdentOrKeyword();
    else
      tokenizer.grabSymbol();
  }

  assert(not out_tokens.empty());

  auto const INVALID_TOKEN = Token(TokenType::INVALID_TOKEN, 1, out_tokens.back().position);
  out_tokens.reserve(out_tokens.size() + INVALID_TOKEN_PADDING);
  for (auto i{0uz}; i < INVALID_TOKEN_PADDING; ++i)
    out_tokens.push_back(INVALID_TOKEN);

#ifndef NDEBUG
  auto end_time = std::chrono::high_resolution_clock::now();
  std::println("Lexing {}: {} | {} | {}",
    file_path.native(),
    end_time - begin_time,
    std::chrono::duration_cast<std::chrono::microseconds>(end_time - begin_time),
    std::chrono::duration_cast<std::chrono::milliseconds>(end_time - begin_time)
  );
#endif

  return std::move(tokenizer.file);
}