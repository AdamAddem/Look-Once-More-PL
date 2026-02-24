#include "lex.hpp"

#include "../debug_flags.hpp"
#include "error.hpp"
#include <cctype>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <unordered_map>
#include <vector>

using namespace Lexer;



struct FileInAnalysis {
  std::ifstream stream;
  uint32_t needs_closing_paren{0};
  uint32_t line_number{1};
  std::vector<Token> token_list;
  bool skip_whitespace{false};
};

/* Lexer Functions */

static int charToEscapeSequenceEquivalent(const FileInAnalysis& file, int c) {
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
static void grabStringLiteral(FileInAnalysis &file) {
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
static void grabCharLiteral(FileInAnalysis &file) {
  int c1 = file.stream.get();
  const int c2 = file.stream.get();

  if (c1 == '\\') {
    c1 = charToEscapeSequenceEquivalent(file, c2);
    if (file.stream.get() != '\'')
      throw LexingError("Expected ending ' in char literal.", std::to_string(static_cast<char>(c1)), file.line_number);
  } else if (c2 != '\'')
    throw LexingError("Expected ending ' in char literal.", std::to_string(static_cast<char>(c2)), file.line_number);

  file.token_list.emplace_back(TokenType::CHAR_LITERAL, c1, file.line_number);
}

static void grabSymbol(FileInAnalysis &file) {
  TokenType type;
  const int c = file.stream.get();
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

// first digit in front of file
static void grabNumber(FileInAnalysis &file) {
  auto type = TokenType::INT_LITERAL;
  Token::TokenValue value;
  int c = file.stream.get();
  std::string num_stringrep(1, c);
  while ((c = file.stream.get()) != EOF) {
    // i hate file handling so much, replace
    // this stupid monkey code eventually
    if (c == 'f') {
      type = TokenType::FLOAT_LITERAL;
      break;
    }

    if (c == '.') {
      type = TokenType::DOUBLE_LITERAL;
      num_stringrep += '.';
      continue;
    }

    if (c < '0' || c > '9') {
      file.stream.putback(static_cast<char>(c));
      break;
    }

    num_stringrep += static_cast<char>(c);
  }

  switch (type) {
  case TokenType::INT_LITERAL:
    value = std::stoi(num_stringrep);
    break;
  case TokenType::FLOAT_LITERAL:
    value = std::stof(num_stringrep);
    break;
  case TokenType::DOUBLE_LITERAL:
    value = std::stod(num_stringrep);
    break;

  default:
    throw LexingError("Invalid numeric literal type found in grabNumber, this shouldn't happen.", num_stringrep, file.line_number);
  }

  file.token_list.emplace_back(type, std::move(value), file.line_number);
}

// first letter in front of file
static void grabIdentOrKeyword(FileInAnalysis &file) {
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
    file.token_list.emplace_back(TokenType::BOOL_LITERAL, Token::TokenValue(1), file.line_number);
    return;
  }
  if (word == "false") [[unlikely]] {
    file.token_list.emplace_back(TokenType::BOOL_LITERAL, Token::TokenValue(0), file.line_number);
    return;
  }

  file.token_list.emplace_back(TokenType::IDENTIFIER, Token::TokenValue(std::move(word)), file.line_number);
}

static void skipWS(FileInAnalysis& file) {
  while (std::isspace(file.stream.peek())) {
    const int c = file.stream.get();
     if (c == '\n')
       ++file.line_number;

     else if (c == EOF)
      return;
  }
}

static bool skipComments(FileInAnalysis& file) {
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

static bool canStartIdentifier(const int c) {
  return std::isalpha(c) || c == '_';
}

TokenHandler Lexer::tokenizeFile(const std::string &file_path) {
  FileInAnalysis file;
  file.stream.open(file_path);
  if (!file.stream)
    throw LexingError("File not found.", file_path, 0);

  while (true) {
    skipWS(file);
    const int c = file.stream.peek();
    if (c == EOF)
      break;
    if (c == '/' && skipComments(file))
      continue;


    if (c >= '0' && c <= '9')
      grabNumber(file);
    else if (canStartIdentifier(c))
      grabIdentOrKeyword(file);
    else
      grabSymbol(file);
  }

  file.stream.close();

  std::reverse(// tokens now organized such that back is first-most token.
      file.token_list.begin(),
      file.token_list.end()
      );

  if (lom_debug::output_lexing) {
    TokenHandler(std::move(file.token_list)).print();
    std::cout << "Lexing stage passed!" << std::endl;
    std::exit(0);
  }

  return TokenHandler(std::move(file.token_list));
}

/* Lexer Functions */