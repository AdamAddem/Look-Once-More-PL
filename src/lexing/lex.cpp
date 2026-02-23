#include "lex.hpp"
#include "../debug_flags.hpp"
#include <cctype>
#include <sstream>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <unordered_map>
#include <vector>

using namespace Lexer;



struct FileInAnalysis {
  std::ifstream stream;
  uint32_t needs_closing_paren{0};
  uint32_t line_number{0};
  std::vector<Token> token_list;

  std::stringstream buff;
  bool skip_whitespace{false};

  int getc() {
    if (buff.str().empty())
      fill_buff();

    int c = buff.get();
    if (!skip_whitespace)
      return c;

    while (std::isspace(c)) {
      c = buff.get();
    }

    if (c == EOF) {
      if (buff.str().empty())
        fill_buff();
      return getc();
    }

    return c;
  }

  int peek() {
    if (buff.str().empty())
      fill_buff();

    return buff.peek();
  }

  void putback(char c) {
    if (buff.str().empty())
      fill_buff();

    buff.putback(c);
  }

  void fill_buff() {
    buff.clear();
    std::string newline;
    std::getline(stream, newline);
    buff << newline;
    ++line_number;
  }

  std::string next() {
    if (buff.str().empty())
      fill_buff();

    std::string line;
    buff >> line;
    return line;
  }

  void skipws() {
    skip_whitespace = true;
  }

  void noskipws() {
    skip_whitespace = false;
  }


};

/* Lexer Functions */

static int charToEscapeSequenceEquivalent(int c) {
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
    throw std::runtime_error("Unknown escape sequence.");
  }
}

// called when opening quotes already consumed
static void grabStringLiteral(FileInAnalysis &file) {
  std::string literal;
  file.noskipws();
  int c = file.getc();
  while (c != EOF) {
    // stupid and dumb
    switch (c) {
    case '\"':
      goto ending_quote_found;

    case '\n':
    case EOF:
      throw std::runtime_error("Expected ending \" in string literal");

    case '\\':
      c = charToEscapeSequenceEquivalent(file.getc());
      [[fallthrough]];
    default:
      literal.push_back(static_cast<char>(c));
    }
    c = file.getc();
  }

ending_quote_found: // don't crucify me for this pls

  file.token_list.emplace_back(TokenType::STRING_LITERAL, literal);
  file.skipws();
}

// called when opening single-quote already consumed
static void grabCharLiteral(FileInAnalysis &file) {
  file.noskipws();

  int c1 = file.getc();
  const int c2 = file.getc();

  if (c1 == '\\') {
    c1 = charToEscapeSequenceEquivalent(c2);
    if (file.getc() != '\'')
      throw std::runtime_error("Expected ending ' in char literal.");
  } else if (c2 != '\'')
    throw std::runtime_error("Expected ending ' in char literal.");

  file.token_list.emplace_back(TokenType::CHAR_LITERAL, c1);
  file.skipws();
}

static void grabSymbol(FileInAnalysis &file, char c) {
  file.skipws();
  TokenType type;
  std::string symbol(1, static_cast<char>(c));
  switch (c) {
  // compound symbols up first, single char symbols down there
  case '+':
  case '-':
    if (file.peek() == c) {
      symbol.push_back(static_cast<char>(file.getc()));
      type = stringToTokenType.at(symbol);
      break;
    }

    if (c == '-' && file.peek() == '>') {
      file.getc();
      type = TokenType::ARROW;
      break;
    }
    [[fallthrough]];
  case '/':
  case '*':
  case '^':
  case '%':
    if (file.peek() == '=') {
      // desugaring for x _= (...) -> x = x _ (...)
      const auto &d = file.token_list.back();
      if (d.type != TokenType::IDENTIFIER)
        throw std::runtime_error(
            "Expected identifier to the left of assignment operator");

      file.token_list.emplace_back(TokenType::ASSIGN);
      file.token_list.emplace_back(d);
      file.token_list.emplace_back(stringToTokenType.at(symbol));
      file.token_list.emplace_back(TokenType::LPAREN);
      ++file.needs_closing_paren;
      file.getc();
      return;
    }
    [[fallthrough]];


  case '<':
  case '>':
    if (file.peek() == '=')
      symbol.push_back(static_cast<char>(file.getc()));
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
    if (file.peek() == '=') [[likely]] {
      file.getc();
      type = TokenType::KEYWORD_NOT_EQUAL;
      break;
    }
    throw std::runtime_error("! token not supported");

  case '=':
    if (file.peek() == '=') {
      file.getc();
      type = TokenType::KEYWORD_EQUALS;
    }
    else
      type = TokenType::ASSIGN;
    break;


  case ';':
    while (file.needs_closing_paren) {
      file.token_list.emplace_back(TokenType::RPAREN);
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
    std::string error_msg("Invalid symbol found: ");
      std::cout << c << std::endl;
      std::cout << file.buff.str() << std::endl;
    throw std::runtime_error(error_msg);
  }

  file.token_list.emplace_back(type);
}

// first digit in front of file
static void grabNumber(FileInAnalysis &file, char first_dig) {
  auto type = TokenType::INT_LITERAL;
  TokenValue value;
  std::string num_stringrep(1, first_dig);
  int c;
  while ((c = file.getc()) != EOF) {
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
      file.putback(static_cast<char>(c));
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
    throw std::runtime_error(
        "Invalid numeric literal type found? This shouldn't happen.");
  }

  file.token_list.emplace_back(type, std::move(value));
}

// first letter in front of file
static void grabIdentOrKeyword(FileInAnalysis &file, char first) {
  int c = file.getc();
  std::string word(1, first);
  while (c != EOF) {
    if (!std::isalnum(c))
      break;

    word.push_back(static_cast<char>(c));
    c = file.getc();
  }
  file.putback(static_cast<char>(c));

  if (stringToTokenType.contains(word)) {
    file.token_list.emplace_back(stringToTokenType.at(word));
    return;
  }
  if (word == "elif") {
    file.token_list.emplace_back(TokenType::KEYWORD_ELSE);
    file.token_list.emplace_back(TokenType::KEYWORD_IF);
    return;
  }
  if (word == "true") [[unlikely]] {
    file.token_list.emplace_back(TokenType::BOOL_LITERAL, TokenValue(1));
    return;
  }
  if (word == "false") [[unlikely]] {
    file.token_list.emplace_back(TokenType::BOOL_LITERAL, TokenValue(0));
    return;
  }

  file.token_list.emplace_back(TokenType::IDENTIFIER, TokenValue(std::move(word)));
}

TokenHandler Lexer::tokenizeFile(const std::string &file_path) {
  FileInAnalysis file;
  file.stream.open(file_path);
  if (!file.stream)
    throw std::runtime_error("File Not Found");

  while (true) {
    file.skipws();
    const int c = file.getc();
    if (c == EOF)
      break;

    if (c >= '0' && c <= '9')
      grabNumber(file, c);
    else if (std::isalpha(c))
      grabIdentOrKeyword(file, c);
    else
      grabSymbol(file, c);
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