#include "lex.hpp"

#include "../edenlib/vectors/releasing_vector.hpp"
#include "error.hpp"
#include "settings.hpp"

#include <cassert>
#include <cctype>
#include <fstream>
#include <unordered_map>
#include <vector>

namespace {
using namespace LOM;
using namespace LOM::Lexer;
using eden::releasing_string;
using eden::flags::reserve_initial;

struct FileInAnalysis {
  std::vector<Token>& token_list;
  std::string text{};
  u64_t current_position{};

  explicit FileInAnalysis(std::vector<Token>& token_list, std::filesystem::path const& path) : token_list(token_list) {
    std::ifstream stream(path); if (stream.is_open()) throw LexingError("File not found.", path.string(), 0);
    auto const file_size = std::filesystem::file_size(path);
    text.reserve(file_size + 1);
    stream.read(text.data(), file_size);
    text.push_back('\0');
  }

  [[nodiscard]] char
  peek() const noexcept { return text[current_position]; }

  [[nodiscard]] char
  pop() noexcept { return text[current_position++]; }

  void skip() noexcept { ++current_position; }
  void undo() noexcept { --current_position; }

  [[nodiscard]] std::string
  steal_text() noexcept { return std::move(text); }

};

void grabNumber(FileInAnalysis& file);

[[nodiscard]] bool
is_num(char c)
{ return c >= '0' and c <= '9'; }

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
    throw LexingError("Unknown escape sequence.", std::to_string(c), 0);
  }
}

// called when opening quotes already consumed
void grabStringLiteral(FileInAnalysis &file) {
  u16_t length = 0;
  auto const pos = static_cast<u32_t>(file.current_position); // grabbing the position after opening quotes
  auto c = file.pop();
  while (c not_eq '\0') {
    switch (c) {
    case '\"': goto ending_quote_found;

    case '\n':
    case '\0':
      throw LexingError("Expected ending \" in string literal.", "Reached end of file.", 0);

    default:
      ++length;
    }
    c = file.pop();
  }

  ending_quote_found: // don't crucify me for this pls
    file.token_list.emplace_back(TokenType::STRING_LITERAL, length, pos);
}

// called when opening single-quote already consumed
void grabCharLiteral(FileInAnalysis &file) {
  u16_t length = 2; auto const pos = static_cast<u32_t>(file.current_position);
  auto c1 = file.pop();
  auto const c2 = file.pop();

  if (c1 == '\\') {
    c1 = charToEscapeSequenceEquivalent(c2); ++length;
    if (file.pop() not_eq '\'')
      throw LexingError("Expected ending ' in char literal.", std::to_string(c1), 0);
  } else if (c2 not_eq '\'')
    throw LexingError("Expected ending ' in char literal.", std::to_string(c2), 0);

  file.token_list.emplace_back(TokenType::CHAR_LITERAL, length, pos);
}

void grabSymbol(FileInAnalysis &file) {
  TokenType type; u16_t length = 1; auto const pos = static_cast<u32_t>(file.current_position);
  auto const c = file.pop();
  auto const peek = file.peek();
  switch (c) { using enum TokenType;
  case '+':
    if (peek == '+') { file.skip(); type = PLUSPLUS; length = 2; }
    else type = PLUS;
    break;
  case '-':
    if (peek == '-') { file.skip(); type = MINUSMINUS; length = 2; }
    else if (peek == '>') { file.skip(); type = ARROW; length = 2; }
    else type = MINUS;
    break;
  case '<':
    if (peek == '=') { file.skip(); type = LESSEQ; length = 2;  }
    else type = LESS;
    break;
  case '>':
    if (peek == '=') { file.skip(); type = GTREQ; length = 2;  }
    else type = GTR;
    break;
  case '!':
    if (peek == '=') { file.skip(); type = KEYWORD_NOT_EQUAL; length = 2;  }
    else throw LexingError("! token only supported in !=.", "Use keyword 'not' instead.", 0);
    break;
  case '=':
    if (peek == '=') { file.skip(); type = KEYWORD_EQUALS; length = 2;  }
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
  case '\"': return grabStringLiteral(file);
  case '\'': return grabCharLiteral(file);
  default: throw LexingError("Invalid symbol.", std::string(1, c), 0);
  }

  file.token_list.emplace_back(type, length, pos);
}

void grabNumber(FileInAnalysis &file) {
  auto c = file.peek();
  bool const is_negative = c == '-';
  TokenType newtoken_type = is_negative ? TokenType::SIGNED_LITERAL : TokenType::UNSIGNED_LITERAL;
  u16_t newtoken_length = 0;
  auto const newtoken_pos = static_cast<u32_t>(file.current_position);

  while ((c = file.pop()) not_eq '\0') {
    if (c == 'f') {
      newtoken_type = TokenType::FLOAT_LITERAL;
      break;
    }
    if (c == '.') {
      newtoken_type = TokenType::DOUBLE_LITERAL;
      continue;
    }
    if (not is_num(c)) {
      file.undo();
      break;
    }

    ++newtoken_length;
  }

  file.token_list.emplace_back(newtoken_type, newtoken_length, newtoken_pos);
}

// first letter in front of file
void grabIdentOrKeyword(FileInAnalysis &file) {
  Token new_token{TokenType::INVALID_TOKEN, 0, static_cast<u32_t>(file.current_position)};
  auto c = file.pop();

  while (c not_eq '\0') {
    if (not std::isalnum(c) and c not_eq '_') break;

    c = file.pop();
    ++new_token.length;
  }

  file.undo();
  const std::string_view word_view{file.text.data() + new_token.length, new_token.length};

  if (stringToTokenType.contains(word_view))
    new_token.type = stringToTokenType.at(word_view);

  else if (word_view == "elif") {
    new_token.type = TokenType::KEYWORD_ELSE; file.token_list.emplace_back(new_token);
    new_token.type = TokenType::KEYWORD_IF;
  }
  else if (word_view == "true" or word_view == "false")
    new_token.type = TokenType::BOOL_LITERAL;
  else
    new_token.type = TokenType::IDENTIFIER;

  file.token_list.emplace_back(new_token);
}

void skipWS(FileInAnalysis& file) {
  while (std::isspace(file.peek())) {
    auto const c = file.pop();
    if (c == '\0') return;
  }
}

[[nodiscard]] bool
skipComments(FileInAnalysis& file) {
  file.skip();
  if (file.peek() == '/') {
    file.skip();

    auto c = file.pop();
    while ( c not_eq '\n' and c not_eq '\0' ) c = file.pop();
    return true;
  }

  file.undo();
  return false;
}

[[nodiscard]] bool
canStartIdentifier(char c)
{ return std::isalpha(c) or c == '_'; }

}

std::string Lexer::tokenizeFile(std::vector<Token>& out_tokens, std::filesystem::path const& file_path) {
  FileInAnalysis file{out_tokens, file_path};

  while (true) {
    skipWS(file);
    auto const c = file.peek();
    if (c == '\0') break;
    if (c == '/' and skipComments(file)) continue;

    if (is_num(c))
      grabNumber(file);
    else if (canStartIdentifier(c))
      grabIdentOrKeyword(file);
    else
      grabSymbol(file);
  }

  return file.steal_text();
}