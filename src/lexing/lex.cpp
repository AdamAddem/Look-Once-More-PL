#include "lex.hpp"
#include "../debug_flags.hpp"
#include <cctype>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <unordered_map>
#include <vector>

using namespace Lexer;

// this is so stupid
#define STRING_TO_KEYWORDS_MAPPING                                             \
  {"and", TokenType::KEYWORD_AND}, {"or", TokenType::KEYWORD_OR},              \
      {"xor", TokenType::KEYWORD_XOR}, {"not", TokenType::KEYWORD_NOT},        \
      {"eq", TokenType::KEYWORD_EQUALS},                                       \
      {"not_eq", TokenType::KEYWORD_NOT_EQUAL},                                \
      {"bitand", TokenType::KEYWORD_BITAND},                                   \
      {"bitor", TokenType::KEYWORD_BITOR},                                     \
      {"bitxor", TokenType::KEYWORD_BITXOR},                                   \
      {"bitnot", TokenType::KEYWORD_BITNOT},                                   \
                                                                               \
      {"i8", TokenType::KEYWORD_i8}, {"i16", TokenType::KEYWORD_i16},          \
      {"i32", TokenType::KEYWORD_i32}, {"i64", TokenType::KEYWORD_i64},        \
      {"u8", TokenType::KEYWORD_u8}, {"u16", TokenType::KEYWORD_u16},          \
      {"u32", TokenType::KEYWORD_u32}, {"u64", TokenType::KEYWORD_u64},        \
      {"f32", TokenType::KEYWORD_f32}, {"f64", TokenType::KEYWORD_f64},        \
      {"char", TokenType::KEYWORD_CHAR},                                       \
      {"string", TokenType::KEYWORD_STRING},                                   \
      {"bool", TokenType::KEYWORD_BOOL},                                       \
      {"devoid", TokenType::KEYWORD_DEVOID},                                   \
                                                                               \
      {"null", TokenType::KEYWORD_NULL}, {"junk", TokenType::KEYWORD_JUNK},    \
      {"selfish", TokenType::KEYWORD_SELFISH},                                 \
      {"sharing", TokenType::KEYWORD_SHARING},                                 \
      {"watching", TokenType::KEYWORD_WATCHING},                               \
      {"raw", TokenType::KEYWORD_RAW}, {"vague", TokenType::KEYWORD_VAGUE},    \
                                                                               \
      {"if", TokenType::KEYWORD_IF}, {"else", TokenType::KEYWORD_ELSE},        \
      {"for", TokenType::KEYWORD_FOR}, {"while", TokenType::KEYWORD_WHILE},    \
      {"do", TokenType::KEYWORD_DO}, {"return", TokenType::KEYWORD_RETURN},    \
      {"switch", TokenType::KEYWORD_SWITCH},                                   \
      {"case", TokenType::KEYWORD_CASE},                                       \
      {"default", TokenType::KEYWORD_DEFAULT},                                 \
      {"goto", TokenType::KEYWORD_GOTO}, {"break", TokenType::KEYWORD_BREAK},  \
      {"continue", TokenType::KEYWORD_CONTINUE},                               \
                                                                               \
      {"cast", TokenType::KEYWORD_CAST},                                       \
      {"cast_if", TokenType::KEYWORD_CAST_IF},                                 \
      {"unsafe_cast", TokenType::KEYWORD_UNSAFE_CAST},                         \
      {"steal", TokenType::KEYWORD_STEAL},                                     \
      {"build_new", TokenType::KEYWORD_BUILD_NEW},                             \
      {"allocate", TokenType::KEYWORD_ALLOCATE},                               \
      {"construct", TokenType::KEYWORD_CONSTRUCT},                             \
      {"mut", TokenType::KEYWORD_MUT}, {"from", TokenType::KEYWORD_FROM},      \
      {"as", TokenType::KEYWORD_AS}, {"global", TokenType::KEYWORD_GLOBAL},    \
      {"globals", TokenType::KEYWORD_GLOBALS},

#define STRING_TO_SYMBOLS_MAPPING                                              \
  {"+", TokenType::PLUS}, {"++", TokenType::PLUSPLUS},                         \
      {"-", TokenType::MINUS}, {"--", TokenType::MINUSMINUS},                  \
      {"/", TokenType::SLASH}, {"*", TokenType::STAR}, {"^", TokenType::POW},  \
      {"%", TokenType::MOD}, {"=", TokenType::ASSIGN},                         \
      {"==", TokenType::KEYWORD_EQUALS}, {"!=", TokenType::KEYWORD_NOT_EQUAL}, \
      {"(", TokenType::LPAREN}, {")", TokenType::RPAREN},                      \
      {"{", TokenType::LBRACE}, {"}", TokenType::RBRACE},                      \
      {"[", TokenType::LBRACKET}, {"]", TokenType::RBRACKET},                  \
      {"<", TokenType::LESS}, {">", TokenType::GTR},                           \
      {"<=", TokenType::LESSEQ}, {">=", TokenType::GTREQ},                     \
      {";", TokenType::SEMI_COLON}, {"@", TokenType::ADDR},                    \
      {",", TokenType::COMMA},

std::unordered_map<std::string, TokenType> keywords{STRING_TO_KEYWORDS_MAPPING};
std::unordered_map<std::string, TokenType> stringToSymbol{
    STRING_TO_SYMBOLS_MAPPING};

struct FileInAnalysis {
  std::ifstream stream;
  uint32_t needs_closing_paren{0};
  std::vector<Token> token_list;
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
  auto &[file_stream, add_closing_paren, token_list] = file;
  std::string literal;
  file_stream >> std::noskipws;
  int c = file_stream.get();
  while (true) {
    // stupid and dumb
    switch (c) {
    case '"':
      goto ending_quote_found;

    /*case '\n': Enable this to restrict string literals to one line.
case '\r':
case '\n\r': \n\r is a weird edge case for windows, not sure how to solve
that.*/
    case EOF:
      throw std::runtime_error("Expected ending \" in string literal");

    case '\\':
      c = file_stream.get();
      c = charToEscapeSequenceEquivalent(c);
      [[fallthrough]];
    default:
      literal.push_back(static_cast<char>(c));
    }
  }

ending_quote_found: // don't crucify me for this pls

  token_list.emplace_back(TokenType::STRING_LITERAL, literal);
  file_stream >> std::ws;
}

// called when opening single-quote already consumed
static void grabCharLiteral(FileInAnalysis &file) {
  auto &[file_stream, add_closing_paren, token_list] = file;
  file_stream >> std::noskipws;

  int c1 = file_stream.get();
  const int c2 = file_stream.get();

  if (c1 == '\\') {
    c1 = charToEscapeSequenceEquivalent(c2);
    if (file_stream.get() != '\'')
      throw std::runtime_error("Expected ending ' in char literal.");
  } else if (c2 != '\'')
    throw std::runtime_error("Expected ending ' in char literal.");

  token_list.emplace_back(TokenType::CHAR_LITERAL,
                          std::string{static_cast<char>(c1)});
  file_stream >> std::ws;
}

static void grabSymbol(FileInAnalysis &file) {
  auto &[file_stream, add_closing_paren, token_list] = file;
  TokenType type;
  const int c = file_stream.get();
  std::string symbol(1, static_cast<char>(c));
  switch (c) {
  // compound symbols up first, single char symbols down there
  case '+':
  case '-':
    if (file_stream.peek() == c) {
      symbol.push_back(static_cast<char>(file_stream.get()));
      type = stringToSymbol[symbol];
      break;
    }
    [[fallthrough]];
  case '/':
  case '*':
  case '^':
  case '%':
    if (file_stream.peek() == '=') {
      // desugaring for x _= (...) -> x = x _ (...)
      const auto &d = token_list.back();
      if (d.type != TokenType::IDENTIFIER)
        throw std::runtime_error(
            "Expected identifier to the left of assignment operator");

      token_list.emplace_back(TokenType::ASSIGN);
      token_list.emplace_back(d);
      token_list.emplace_back(stringToSymbol[symbol]);
      token_list.emplace_back(TokenType::LPAREN);
      ++add_closing_paren;
      file_stream.get();
      return;
    }
    [[fallthrough]];

  case '!':
  case '=':
  case '<':
  case '>':
    if (file_stream.peek() == '=')
      symbol.push_back(static_cast<char>(file_stream.get()));
    [[fallthrough]];

  case '(':
  case ')':
  case '{':
  case '}':
  case '[':
  case ']':
  case '@':
  case ',':
    type = stringToSymbol[symbol];
    break;

  case ';':
    while (add_closing_paren) {
      token_list.emplace_back(TokenType::RPAREN);
      --add_closing_paren;
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
    error_msg.append(1, static_cast<char>(c));
    throw std::runtime_error(error_msg);
  }

  token_list.emplace_back(type);
}

// first digit in front of file
static void grabNumber(FileInAnalysis &file) {
  auto &[file_stream, add_closing_paren, token_list] = file;

  auto type = TokenType::INT_LITERAL;
  TokenValue value;
  std::string num_stringrep;
  int c;
  while ((c = file_stream.get()) != EOF) {
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
      file_stream.putback(static_cast<char>(c));
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

  token_list.emplace_back(type, std::move(value));
}

// first letter in front of file
static void grabIdentOrKeyword(FileInAnalysis &file) {
  auto &[file_stream, add_closing_paren, token_list] = file;

  char c;
  file_stream.get(c);
  std::string word(1, c);
  while (file_stream.get(c)) {
    if (!std::isalnum(c))
      break;

    word.push_back(c);
  }
  file_stream.putback(c);

  if (keywords.contains(word)) {
    token_list.emplace_back(keywords[word]);
    return;
  }
  if (word == "elif") {
    token_list.emplace_back(TokenType::KEYWORD_ELSE);
    token_list.emplace_back(TokenType::KEYWORD_IF);
    return;
  }
  if (word == "true") [[unlikely]] {
    token_list.emplace_back(TokenType::BOOL_LITERAL, TokenValue(1));
    return;
  }
  if (word == "false") [[unlikely]] {
    token_list.emplace_back(TokenType::BOOL_LITERAL, TokenValue(0));
    return;
  }

  token_list.emplace_back(TokenType::IDENTIFIER, TokenValue(std::move(word)));
}

TokenHandler Lexer::tokenizeFile(const std::string &file_path) {
  FileInAnalysis file;
  file.stream.open(file_path);
  if (!file.stream)
    throw std::runtime_error("File Not Found");

  auto &[file_stream, add_closing_paren, token_list] = file;

  while (true) {
    file_stream >> std::ws;
    const int c = file_stream.peek();
    if (c == EOF)
      break;

    if (c >= '0' && c <= '9')
      grabNumber(file);
    else if (std::isalpha(c))
      grabIdentOrKeyword(file);
    else
      grabSymbol(file);
  }
  file_stream.close();

  std::reverse(
      token_list.begin(),
      token_list
          .end()); // tokens now organized such that back is first-most token.

  if (lom_debug::output_lexing) {
    TokenHandler(std::move(token_list)).print();
    std::cout << "Lexing stage passed!" << std::endl;
    std::exit(0);
  }

  return TokenHandler(std::move(token_list));
}

/* Lexer Functions */