#include "lex.hpp"

#include "error.hpp"
#include "settings.hpp"

#include <cassert>
#include <cctype>
#include <fstream>
#include <iostream>
#include <unordered_map>
#include <vector>



namespace {
using namespace LOM;
using namespace LOM::Lexer;

struct FileInAnalysis {
  std::ifstream stream;
  u32_t needs_closing_paren{0};
  u32_t line_number{1};
  std::vector<Token> token_list;
};

void grabNumber(FileInAnalysis& file);

[[nodiscard]] bool
is_num(int c) { return c >= '0' && c <= '9'; }

[[nodiscard]] int
charToEscapeSequenceEquivalent(const FileInAnalysis& file, int c) {
  switch (c) {
  case 'n':
    return '\n';
  case 't':
    return '\t';
  case 'b':
    return '\b';
  case 'r':
    return '\r';
  case 'f':
    return '\f';
  case '\\':
    return '\\';
  case '"':
    return '"';
  case '\'':
    return '\'';
  case '0':
    return '\0';
  case 'v':
    return '\v';
  default:
    throw LexingError("Unknown escape sequence.", std::to_string(c), file.line_number);
  }
}

// called when opening quotes already consumed
void grabStringLiteral(FileInAnalysis &file) {
  std::string literal;
  int c = file.stream.get();
  while (c != EOF) {
    // stupid and dumb
    switch (c) {
    case '\"':
      goto ending_quote_found;

    case '\n':
    case EOF:
      throw LexingError("Expected ending \" in string literal.", "Reached end of file.", file.line_number);

    case '\\':
      c = charToEscapeSequenceEquivalent(file, file.stream.get());
      [[fallthrough]];
    default:
      literal.push_back(static_cast<char>(c));
    }
    c = file.stream.get();
  }

ending_quote_found: // don't crucify me for this pls

  file.token_list.emplace_back(TokenType::STRING_LITERAL, literal, file.line_number);
}

// called when opening single-quote already consumed
void grabCharLiteral(FileInAnalysis &file) {
  int c1 = file.stream.get();
  const int c2 = file.stream.get();

  if (c1 == '\\') {
    c1 = charToEscapeSequenceEquivalent(file, c2);
    if (file.stream.get() != '\'')
      throw LexingError("Expected ending ' in char literal.", std::to_string(static_cast<char>(c1)), file.line_number);
  } else if (c2 != '\'')
    throw LexingError("Expected ending ' in char literal.", std::to_string(static_cast<char>(c2)), file.line_number);

  file.token_list.emplace_back(TokenType::CHAR_LITERAL, static_cast<uint64_t>(c1), file.line_number);
}

void grabSymbol(FileInAnalysis &file) {
  TokenType type;
  const int c = file.stream.get();
  if (c == '-' && isCategorySYMBOLS(file.token_list.back().type) && is_num(file.stream.peek())) {
    file.stream.putback('-');
    return grabNumber(file);
  }

  std::string symbol(1, static_cast<char>(c));
  switch (c) {

  // compound symbols up first, single char symbols down there
  case '+':
  case '-':
    if (file.stream.peek() == c) {
      symbol.push_back(static_cast<char>(file.stream.get()));
      type = stringToTokenType.at(symbol);
      break;
    }

    if (c == '-' && file.stream.peek() == '>') {
      file.stream.get();
      type = TokenType::ARROW;
      break;
    }
    [[fallthrough]];
  case '/':
  case '*':
  case '^':
  case '%':
    if (file.stream.peek() == '=') {
      // desugaring for x _= (...) -> x = x _ (...)
      const auto &d = file.token_list.back();
      if (d.type != TokenType::IDENTIFIER)
        throw LexingError("Expected identifier to the left of assignment operator.", d.toString(), file.line_number);

      file.token_list.emplace_back(TokenType::ASSIGN, file.line_number);
      file.token_list.emplace_back(d);
      file.token_list.emplace_back(stringToTokenType.at(symbol), file.line_number);
      file.token_list.emplace_back(TokenType::LPAREN, file.line_number);
      ++file.needs_closing_paren;
      file.stream.get();
      return;
    }
    [[fallthrough]];


  case '<':
  case '>':
    if (file.stream.peek() == '=')
      symbol.push_back(static_cast<char>(file.stream.get()));
    [[fallthrough]];

  case '(':
  case ')':
  case '{':
  case '}':
  case '[':
  case ']':
  case '@':
  case ',':
    type = stringToTokenType.at(symbol);
    break;

  case '!':
    if (file.stream.peek() == '=') [[likely]] {
      file.stream.get();
      type = TokenType::KEYWORD_NOT_EQUAL;
      break;
    }
    throw LexingError("! token not supported.", "", file.line_number);

  case '=':
    if (file.stream.peek() == '=') {
      file.stream.get();
      type = TokenType::KEYWORD_EQUALS;
    }
    else
      type = TokenType::ASSIGN;
    break;


  case ';':
    while (file.needs_closing_paren) {
      file.token_list.emplace_back(TokenType::RPAREN, file.line_number);
      --file.needs_closing_paren;
    }
    type = TokenType::SEMI_COLON;
    break;

  case '\"':
    grabStringLiteral(file);
    return;
  case '\'':
    grabCharLiteral(file);
    return;

  default:
    throw LexingError("Invalid symbol found.", std::to_string(static_cast<char>(c)), file.line_number);
  }

  file.token_list.emplace_back(type, file.line_number);
}

void grabNumber(FileInAnalysis &file) {
  auto type = TokenType::INT_LITERAL;
  Token::TokenValue value;
  int c = file.stream.get();
  const bool is_negative = c == '-';
  std::string num_stringrep(1, static_cast<char>(c));
  while ((c = file.stream.get()) != EOF) {
    // i hate file handling so much, replace
    // this stupid monkey code eventually
    if (c == 'f') {
      type = TokenType::FLOAT_LITERAL;
      break;
    }
    if (c == 'u') {
      type = TokenType::UINT_LITERAL;
      if (is_negative)
        throw LexingError("Negative unsigned literal.", num_stringrep, file.line_number);
      break;
    }

    if (c == '.') {
      type = TokenType::DOUBLE_LITERAL;
      num_stringrep.push_back('.');
      continue;
    }

    if (c < '0' || c > '9') {
      file.stream.putback(static_cast<char>(c));
      break;
    }

    num_stringrep.push_back(static_cast<char>(c));
  }

  switch (type) {
  case TokenType::INT_LITERAL:
    static_assert((sizeof(long long) == 8) && "The following line of code may cause problems if long longs are not 64 bits in width");
    value = std::bit_cast<std::uint64_t>(static_cast<std::int64_t>(std::stoll(num_stringrep)));
    break;
  case TokenType::UINT_LITERAL:
    static_assert((sizeof(unsigned long long) == 8) && "The following line of code may cause problems if unsigned long longs are not 64 bits in width");
    value = static_cast<std::uint64_t>(std::stoull(num_stringrep));
    break;
  case TokenType::FLOAT_LITERAL:
    static_assert((sizeof(float) == 4) && "You get the idea by now");
    value = static_cast<std::uint64_t>(std::bit_cast<std::uint32_t>(std::stof(num_stringrep)));
    break;
  case TokenType::DOUBLE_LITERAL:
    static_assert(sizeof(double) == 8);
    value = std::bit_cast<std::uint64_t>(std::stod(num_stringrep));
    break;

  default:
    assert(false);
  }

  file.token_list.emplace_back(type, std::move(value), file.line_number);
}

// first letter in front of file
void grabIdentOrKeyword(FileInAnalysis &file) {
  int c = file.stream.get();
  std::string word;
  while (c != EOF) {
    if (!std::isalnum(c) && c != '_')
      break;

    word.push_back(static_cast<char>(c));
    c = file.stream.get();
  }
  file.stream.putback(static_cast<char>(c));

  if (stringToTokenType.contains(word)) {
    file.token_list.emplace_back(stringToTokenType.at(word), file.line_number);
    return;
  }
  if (word == "elif") {
    file.token_list.emplace_back(TokenType::KEYWORD_ELSE, file.line_number);
    file.token_list.emplace_back(TokenType::KEYWORD_IF, file.line_number);
    return;
  }
  if (word == "true") [[unlikely]] {
    file.token_list.emplace_back(TokenType::BOOL_LITERAL, Token::TokenValue(std::in_place_type<std::uint64_t>, 1), file.line_number);
    return;
  }
  if (word == "false") [[unlikely]] {
    file.token_list.emplace_back(TokenType::BOOL_LITERAL, Token::TokenValue(std::in_place_type<std::uint64_t>, 0), file.line_number);
    return;
  }

  file.token_list.emplace_back(TokenType::IDENTIFIER, Token::TokenValue(std::move(word)), file.line_number);
}

void skipWS(FileInAnalysis& file) {
  while (std::isspace(file.stream.peek())) {
    const int c = file.stream.get();
     if (c == '\n')
       ++file.line_number;

     else if (c == EOF)
      return;
  }
}

[[nodiscard]] bool skipComments(FileInAnalysis& file) {
  constexpr static auto stream_max = std::numeric_limits<std::streamsize>::max();
  file.stream.get();
  if (file.stream.peek() == '/') {
    file.stream.get();
    file.stream.ignore(stream_max, '\n');
    ++file.line_number;
    return true;
  }

  file.stream.putback('/');
  return false;
}

[[nodiscard]] bool canStartIdentifier(int c) {
  return std::isalpha(c) || c == '_';
}
}

std::vector<Token> Lexer::tokenizeFile(const std::filesystem::path &file_path) {
  FileInAnalysis file;
  file.stream.open(file_path);
  const auto sz = std::filesystem::file_size(file_path);
  file.token_list.reserve(sz / 4);
  if (!file.stream)
    throw LexingError("File not found.", file_path.string(), 0);

  while (true) {
    skipWS(file);
    const int c = file.stream.peek();
    if (c == EOF)
      break;
    if (c == '/' && skipComments(file))
      continue;


    if (is_num(c))
      grabNumber(file);
    else if (canStartIdentifier(c))
      grabIdentOrKeyword(file);
    else
      grabSymbol(file);
  }
  file.stream.close();

  if (Settings::doOutputLexer()) {
    std::cout << "\n--- Lexer Output ---";
    TokenView(file.token_list.begin(), file.token_list.end()).print();
    std::cout << "\n--- Lexer Output ---\n";
    std::quick_exit(0);
  }

  return std::move(file.token_list);
}
