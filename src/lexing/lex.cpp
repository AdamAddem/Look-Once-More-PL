#include "lex.hpp"
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
      {"equals", TokenType::KEYWORD_EQUALS},                                   \
      {"bitand", TokenType::KEYWORD_BITAND},                                   \
      {"bitor", TokenType::KEYWORD_BITOR},                                     \
      {"bitxor", TokenType::KEYWORD_BITXOR},                                   \
      {"bitnot", TokenType::KEYWORD_BITNOT}, {"int", TokenType::KEYWORD_INT},  \
      {"uint", TokenType::KEYWORD_UINT}, {"float", TokenType::KEYWORD_FLOAT},  \
      {"double", TokenType::KEYWORD_DOUBLE},                                   \
      {"char", TokenType::KEYWORD_CHAR}, {"uchar", TokenType::KEYWORD_UCHAR},  \
      {"bool", TokenType::KEYWORD_BOOL},                                       \
      {"string", TokenType::KEYWORD_STRING},                                   \
      {"short", TokenType::KEYWORD_SHORT}, {"long", TokenType::KEYWORD_LONG},  \
      {"signed", TokenType::KEYWORD_SIGNED},                                   \
      {"unsigned", TokenType::KEYWORD_UNSIGNED},                               \
      {"null", TokenType::KEYWORD_NULL},                                       \
      {"devoid", TokenType::KEYWORD_DEVOID},                                   \
      {"junk", TokenType::KEYWORD_JUNK},                                       \
      {"selfish", TokenType::KEYWORD_SELFISH},                                 \
      {"sharing", TokenType::KEYWORD_SHARING},                                 \
      {"watching", TokenType::KEYWORD_WATCHING},                               \
      {"raw", TokenType::KEYWORD_RAW}, {"vague", TokenType::KEYWORD_VAGUE},    \
      {"if", TokenType::KEYWORD_IF}, {"else", TokenType::KEYWORD_ELSE},        \
      {"elif", TokenType::KEYWORD_ELIF}, {"for", TokenType::KEYWORD_FOR},      \
      {"while", TokenType::KEYWORD_WHILE}, {"do", TokenType::KEYWORD_DO},      \
      {"return", TokenType::KEYWORD_RETURN},                                   \
      {"switch", TokenType::KEYWORD_SWITCH},                                   \
      {"case", TokenType::KEYWORD_CASE},                                       \
      {"default", TokenType::KEYWORD_DEFAULT},                                 \
      {"goto", TokenType::KEYWORD_GOTO}, {"break", TokenType::KEYWORD_BREAK},  \
      {"continue", TokenType::KEYWORD_CONTINUE},                               \
      {"cast", TokenType::KEYWORD_CAST},                                       \
      {"cast_if", TokenType::KEYWORD_CAST_IF},                                 \
      {"unsafe_cast", TokenType::KEYWORD_UNSAFE_CAST},                         \
      {"very_unsafe_cast", TokenType::KEYWORD_VERY_UNSAFE_CAST},               \
      {"steal", TokenType::KEYWORD_STEAL},                                     \
      {"build_new", TokenType::KEYWORD_BUILD_NEW},                             \
      {"allocate", TokenType::KEYWORD_ALLOCATE},                               \
      {"construct", TokenType::KEYWORD_CONSTRUCT},                             \
      {"auto", TokenType::KEYWORD_AUTO}, {"const", TokenType::KEYWORD_CONST},  \
      {"except", TokenType::KEYWORD_EXCEPT},                                   \
      {"static", TokenType::KEYWORD_STATIC},                                   \
      {"extern", TokenType::KEYWORD_EXTERN},                                   \
      {"true", TokenType::KEYWORD_TRUE}, {"false", TokenType::KEYWORD_FALSE},  \
      {"from", TokenType::KEYWORD_FROM}, {"as", TokenType::KEYWORD_AS},        \
      {"global", TokenType::KEYWORD_GLOBAL},                                   \
      {"globals", TokenType::KEYWORD_GLOBALS},

#define KEYWORDS_TO_STRING_MAPPING                                             \
  {TokenType::KEYWORD_AND, "and"}, {TokenType::KEYWORD_OR, "or"},              \
      {TokenType::KEYWORD_XOR, "xor"}, {TokenType::KEYWORD_NOT, "not"},        \
      {TokenType::KEYWORD_EQUALS, "equals"},                                   \
      {TokenType::KEYWORD_BITAND, "bitand"},                                   \
      {TokenType::KEYWORD_BITOR, "bitor"},                                     \
      {TokenType::KEYWORD_BITXOR, "bitxor"},                                   \
      {TokenType::KEYWORD_BITNOT, "bitnot"}, {TokenType::KEYWORD_INT, "int"},  \
      {TokenType::KEYWORD_UINT, "uint"}, {TokenType::KEYWORD_FLOAT, "float"},  \
      {TokenType::KEYWORD_DOUBLE, "double"},                                   \
      {TokenType::KEYWORD_CHAR, "char"}, {TokenType::KEYWORD_UCHAR, "uchar"},  \
      {TokenType::KEYWORD_BOOL, "bool"},                                       \
      {TokenType::KEYWORD_STRING, "string"},                                   \
      {TokenType::KEYWORD_SHORT, "short"}, {TokenType::KEYWORD_LONG, "long"},  \
      {TokenType::KEYWORD_SIGNED, "signed"},                                   \
      {TokenType::KEYWORD_UNSIGNED, "unsigned"},                               \
      {TokenType::KEYWORD_NULL, "null"},                                       \
      {TokenType::KEYWORD_DEVOID, "devoid"},                                   \
      {TokenType::KEYWORD_JUNK, "junk"},                                       \
      {TokenType::KEYWORD_SELFISH, "selfish"},                                 \
      {TokenType::KEYWORD_SHARING, "sharing"},                                 \
      {TokenType::KEYWORD_WATCHING, "watching"},                               \
      {TokenType::KEYWORD_RAW, "raw"}, {TokenType::KEYWORD_VAGUE, "vague"},    \
      {TokenType::KEYWORD_IF, "if"}, {TokenType::KEYWORD_ELSE, "else"},        \
      {TokenType::KEYWORD_ELIF, "elif"}, {TokenType::KEYWORD_FOR, "for"},      \
      {TokenType::KEYWORD_WHILE, "while"}, {TokenType::KEYWORD_DO, "do"},      \
      {TokenType::KEYWORD_RETURN, "return"},                                   \
      {TokenType::KEYWORD_SWITCH, "switch"},                                   \
      {TokenType::KEYWORD_CASE, "case"},                                       \
      {TokenType::KEYWORD_DEFAULT, "default"},                                 \
      {TokenType::KEYWORD_GOTO, "goto"}, {TokenType::KEYWORD_BREAK, "break"},  \
      {TokenType::KEYWORD_CONTINUE, "continue"},                               \
      {TokenType::KEYWORD_CAST, "cast"},                                       \
      {TokenType::KEYWORD_CAST_IF, "cast_if"},                                 \
      {TokenType::KEYWORD_UNSAFE_CAST, "unsafe_cast"},                         \
      {TokenType::KEYWORD_VERY_UNSAFE_CAST, "very_unsafe_cast"},               \
      {TokenType::KEYWORD_STEAL, "steal"},                                     \
      {TokenType::KEYWORD_BUILD_NEW, "build_new"},                             \
      {TokenType::KEYWORD_ALLOCATE, "allocate"},                               \
      {TokenType::KEYWORD_CONSTRUCT, "construct"},                             \
      {TokenType::KEYWORD_AUTO, "auto"}, {TokenType::KEYWORD_CONST, "const"},  \
      {TokenType::KEYWORD_EXCEPT, "except"},                                   \
      {TokenType::KEYWORD_STATIC, "static"},                                   \
      {TokenType::KEYWORD_EXTERN, "extern"},                                   \
      {TokenType::KEYWORD_TRUE, "true"}, {TokenType::KEYWORD_FALSE, "false"},  \
      {TokenType::KEYWORD_FROM, "from"}, {TokenType::KEYWORD_AS, "as"},        \
      {TokenType::KEYWORD_GLOBAL, "global"},                                   \
      {TokenType::KEYWORD_GLOBALS, "globals"},

#define STRING_TO_SYMBOLS_MAPPING                                              \
  {"+", TokenType::PLUS}, {"++", TokenType::PLUSPLUS},                         \
      {"-", TokenType::MINUS}, {"--", TokenType::MINUSMINUS},                  \
      {"/", TokenType::SLASH}, {"*", TokenType::STAR}, {"^", TokenType::POW},  \
      {"%", TokenType::MOD}, {"+=", TokenType::PLUS_ASSIGN},                   \
      {"-=", TokenType::MINUS_ASSIGN}, {"/=", TokenType::DIV_ASSIGN},          \
      {"*=", TokenType::MULT_ASSIGN}, {"^=", TokenType::POW_ASSIGN},           \
      {"%=", TokenType::MOD_ASSIGN}, {"=", TokenType::ASSIGN},                 \
      {"(", TokenType::LPAREN}, {")", TokenType::RPAREN},                      \
      {"{", TokenType::LBRACE}, {"}", TokenType::RBRACE},                      \
      {"[", TokenType::LBRACKET}, {"]", TokenType::RBRACKET},                  \
      {"<", TokenType::LESS}, {">", TokenType::GTR},                           \
      {"<=", TokenType::LESSEQ}, {">=", TokenType::GTREQ},                     \
      {";", TokenType::SEMI_COLON}, {"@", TokenType::ADDR},                    \
      {",", TokenType::COMMA},

#define SYMBOLS_TO_STRING_MAPPING                                              \
  {TokenType::PLUS, "+"}, {TokenType::PLUSPLUS, "++"},                         \
      {TokenType::MINUS, "-"}, {TokenType::MINUSMINUS, "--"},                  \
      {TokenType::SLASH, "/"}, {TokenType::STAR, "*"}, {TokenType::POW, "^"},  \
      {TokenType::MOD, "%"}, {TokenType::PLUS_ASSIGN, "+="},                   \
      {TokenType::MINUS_ASSIGN, "-="}, {TokenType::DIV_ASSIGN, "/="},          \
      {TokenType::MULT_ASSIGN, "*="}, {TokenType::POW_ASSIGN, "^="},           \
      {TokenType::MOD_ASSIGN, "%="}, {TokenType::ASSIGN, "="},                 \
      {TokenType::LPAREN, "("}, {TokenType::RPAREN, ")"},                      \
      {TokenType::LBRACE, "{"}, {TokenType::RBRACE, "}"},                      \
      {TokenType::LBRACKET, "["}, {TokenType::RBRACKET, "]"},                  \
      {TokenType::LESS, "<"}, {TokenType::GTR, ">"},                           \
      {TokenType::LESSEQ, "<="}, {TokenType::GTREQ, ">="},                     \
      {TokenType::SEMI_COLON, ";"}, {TokenType::ADDR, "@"},                    \
      {TokenType::COMMA, ","},

std::unordered_map<std::string, TokenType> keywords{STRING_TO_KEYWORDS_MAPPING};
std::unordered_map<std::string, TokenType> stringToSymbol{
    STRING_TO_SYMBOLS_MAPPING};

/* Lexer Functions */

char charToEscapeSequenceEquivalent(char c) {
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
void grabStringLiteral(std::vector<Token> &token_list, std::ifstream &file) {
  std::string literal;
  file >> std::noskipws;
  char c = 1;
  while (true) { // stupid and dumb
    c = file.get();
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
      c = file.get();
      c = charToEscapeSequenceEquivalent(c);
    default:
      literal.push_back(c);
    }
  }

ending_quote_found:

  token_list.emplace_back(TokenType::STRING_LITERAL, literal);
  file >> std::ws;
}

// called when opening single-quote already consumed
void grabCharLiteral(std::vector<Token> &token_list, std::ifstream &file) {
  file >> std::noskipws;
  char c1 = file.get();
  char c2 = file.get();

  if (c1 == '\\') { // this is stupid

    c1 = charToEscapeSequenceEquivalent(c2);

    if (file.get() != '\'')
      throw std::runtime_error("Expected ending ' in char literal.");
  } else if (c2 != '\'')
    throw std::runtime_error("Expected ending ' in char literal.");

  token_list.emplace_back(TokenType::CHAR_LITERAL, std::string{c1});
  file >> std::ws;
}

// c contains already popped symbol
void grabSymbol(char c, std::vector<Token> &token_list, std::ifstream &file) {
  TokenType type;
  std::string symbol(1, c);
  switch (c) { // compound symbols up first, single char symbols down there
  case '+':
  case '-':
    if (file.peek() == c) {
      symbol.push_back(file.get());
      type = stringToSymbol[symbol];
      break;
    }
  case '/':
  case '*':
  case '^':
  case '%':
  case '<':
  case '>':
    if (file.peek() == '=')
      symbol.push_back(file.get());
  case '=':
  case '(':
  case ')':
  case '{':
  case '}':
  case '[':
  case ']':
  case ';':
  case '@':
  case ',':
    type = stringToSymbol[symbol];
    break;

  case '\"':
    grabStringLiteral(token_list, file);
    return;
  case '\'':
    grabCharLiteral(token_list, file);
    return;

  default:
    std::string error_msg("Invalid symbol found: ");
    error_msg.append(1, c);
    throw std::runtime_error(error_msg);
  }

  token_list.emplace_back(type);
}

// first digit in front of file
void grabNumber(std::vector<Token> &token_list, std::ifstream &file) {
  TokenType type = TokenType::INT_LITERAL;
  TokenValue value;
  std::string num;
  char c;
  while ((c = file.get()) != EOF) { // i hate file handling so much, replace
                                    // this stupid monkey code eventually
    if (c == 'f') {
      type = TokenType::FLOAT_LITERAL;
      break;
    }

    if (c == '.') {
      type = TokenType::DOUBLE_LITERAL;
      num += '.';
      continue;
    }

    if (c < '0' || c > '9') {
      file.putback(c);
      break;
    }

    num += c;
  }

  switch (type) {
  case TokenType::INT_LITERAL:
    value = std::stoi(num);
    break;
  case TokenType::FLOAT_LITERAL:
    value = std::stof(num);
    break;
  case TokenType::DOUBLE_LITERAL:
    value = std::stod(num);
    break;

  default:
    throw std::runtime_error(
        "Invalid numeric literal type found? This shouldn't happen.");
  }

  token_list.emplace_back(type, std::move(value));
}

// first letter in front of file
void grabIdentOrKeyword(std::vector<Token> &token_list, std::ifstream &file) {
  TokenType type;
  TokenValue value;

  std::string word;
  char c;
  file.get(c);
  word += c;
  while (file.get(c)) {
    if (!std::isalnum(c))
      break;

    word += c;
  }
  file.putback(c);

  if (word == "true" || word == "false") {
    type = TokenType::BOOL_LITERAL;
    value = (word == "true" ? 1 : 0);
  } else if (keywords.contains(word))
    type = keywords[word];
  else {
    type = TokenType::IDENTIFIER;
    value = word;
  }

  token_list.emplace_back(type, std::move(value));
}

void debugPrintList(std::vector<Token> &token_list) {
  static std::unordered_map<TokenType, std::string> keywordToString = {
      KEYWORDS_TO_STRING_MAPPING};
  static std::unordered_map<TokenType, std::string> symbolToString = {
      SYMBOLS_TO_STRING_MAPPING};

  for (auto &t : token_list) {

    switch (t.type) {
    case TokenType::IDENTIFIER:
      std::cout << "IDENTIFIER: " << std::get<std::string>(t.value);
      break;

    case TokenType::INT_LITERAL:
      std::cout << "INT_LITERAL: " << std::get<int>(t.value);
      break;

    case TokenType::FLOAT_LITERAL:
      std::cout << "FLOAT_LITERAL: " << std::get<float>(t.value);
      break;

    case TokenType::DOUBLE_LITERAL:
      std::cout << "DOUBLE_LITERAL: " << std::get<double>(t.value);
      break;

    case TokenType::CHAR_LITERAL:
      std::cout << "CHAR_LITERAL: " << std::get<std::string>(t.value);
      break;

    case TokenType::STRING_LITERAL:
      std::cout << "STRING_LITERAL: " << std::get<std::string>(t.value);
      break;

    case TokenType::BOOL_LITERAL:
      std::cout << "BOOL_LITERAL: "
                << (std::get<int>(t.value) ? "true" : "false");
      break;

    default:
      if (keywordToString.contains(t.type))
        std::cout << "KEYWORD: " << keywordToString[t.type];
      else if (symbolToString.contains(t.type))
        std::cout << "sym: " << symbolToString[t.type];
      else
        std::cout << "ERROR: ";
    }

    std::cout << std::endl;
  }
}

TokenHandler Lexer::tokenizeFile(const std::string &file_path) {
  std::ifstream file(file_path);
  if (!file)
    throw std::runtime_error("File Not Found");

  std::vector<Token> token_list;
  char c = 1;
  while (c != EOF) {
    file >> std::ws;
    c = file.peek();
    if (c == EOF)
      break;
    if (c >= '0' && c <= '9') {
      grabNumber(token_list, file);
    } else if (std::isalpha(c)) {
      grabIdentOrKeyword(token_list, file);
    } else {
      grabSymbol(file.get(), token_list, file);
    }
  }
  file.close();

  std::reverse(
      token_list.begin(),
      token_list
          .end()); // tokens now organized such that back is first-most token.

  return TokenHandler(std::move(token_list));
}

/* Lexer Functions */

/* Token Methods */

bool Token::isPrimitive() const {
  switch (type) {
  case TokenType::KEYWORD_INT:
  case TokenType::KEYWORD_UINT:
  case TokenType::KEYWORD_FLOAT:
  case TokenType::KEYWORD_DOUBLE:
  case TokenType::KEYWORD_CHAR:
  case TokenType::KEYWORD_UCHAR:
  case TokenType::KEYWORD_STRING:
  case TokenType::KEYWORD_BOOL:
  case TokenType::KEYWORD_SHORT:
  case TokenType::KEYWORD_LONG:
  case TokenType::KEYWORD_SIGNED:
  case TokenType::KEYWORD_UNSIGNED:
    return true;

  default:
    return false;
  }
}

bool Token::isLiteral() const {
  switch (type) {
  case TokenType::INT_LITERAL:
  case TokenType::FLOAT_LITERAL:
  case TokenType::DOUBLE_LITERAL:
  case TokenType::CHAR_LITERAL:
  case TokenType::STRING_LITERAL:
  case TokenType::BOOL_LITERAL:
    return true;

  default:
    return false;
  }
}

std::string Token::toString() {
  static std::unordered_map<TokenType, std::string> tokenToString{
      KEYWORDS_TO_STRING_MAPPING SYMBOLS_TO_STRING_MAPPING};
  if (type == TokenType::IDENTIFIER)
    return std::get<std::string>(value);

  if (isLiteral()) {
    switch (type) {
    case TokenType::INT_LITERAL:
      return std::to_string(std::get<int>(value));
    case TokenType::FLOAT_LITERAL:
      return std::to_string(std::get<float>(value));
    case TokenType::DOUBLE_LITERAL:
      return std::to_string(std::get<double>(value));
    case TokenType::CHAR_LITERAL:
      return std::to_string((char)std::get<int>(value));
    case TokenType::STRING_LITERAL:
      return std::get<std::string>(value);
    case TokenType::BOOL_LITERAL:
      return std::get<int>(value) ? "true" : "false";

    default:
      throw std::runtime_error("Error in isLiteral method");
    }
  }

  return tokenToString[type];
}

int Token::getInt() const { return std::get<int>(value); }
float Token::getFloat() const { return std::get<float>(value); }
double Token::getDouble() const { return std::get<double>(value); }
bool Token::getBool() const { return std::get<int>(value); }
std::string Token::takeString() {
  return std::get<std::string>(std::move(value));
}

/* Token Methods */

/* TokenHandler Methods */

void TokenHandler::print() {
  auto curr = token_list.rbegin();
  auto end = token_list.rend();

  while (curr != end) {
    std::cout << curr->toString() << std::endl;
    ++curr;
  }
}

bool TokenHandler::pop_if(TokenType _type) {
  if (token_list.empty())
    return false;

  if (token_list.back().type == _type) {
    token_list.pop_back();
    return true;
  }

  return false;
}

TokenHandler TokenHandler::getTokensBetweenBraces() {
  std::vector<Lexer::Token> body;
  int openbrace = 1;
  while (openbrace) {
    if (token_list.empty())
      throw std::runtime_error("Expected closing rbrace");

    if (token_list.back().type == TokenType::LBRACE)
      ++openbrace;
    else if (token_list.back().type == TokenType::RBRACE)
      --openbrace;

    body.emplace_back(std::move(token_list.back()));
    token_list.pop_back();
  }
  body.pop_back();
  std::reverse(body.begin(), body.end()); // stackify

  return TokenHandler(std::move(body));
}

TokenHandler TokenHandler::getTokensBetweenParenthesis() {
  std::vector<Lexer::Token> body;
  int openbrace = 1;
  while (openbrace) {
    if (token_list.empty())
      throw std::runtime_error("Expected closing rparen");

    if (token_list.back().type == TokenType::LPAREN)
      ++openbrace;
    else if (token_list.back().type == TokenType::RPAREN)
      --openbrace;

    body.emplace_back(std::move(token_list.back()));
    token_list.pop_back();
  }
  body.pop_back();

  std::reverse(body.begin(), body.end()); // stackify

  return TokenHandler(std::move(body));
}

TokenHandler TokenHandler::getTokensBetweenBrackets() {
  std::vector<Lexer::Token> body;
  int openbracket = 1;
  while (openbracket) {
    if (token_list.empty())
      throw std::runtime_error("Expected closing rparen");

    if (token_list.back().type == TokenType::LBRACKET)
      ++openbracket;
    else if (token_list.back().type == TokenType::RBRACKET)
      --openbracket;

    body.emplace_back(std::move(token_list.back()));
    token_list.pop_back();
  }
  body.pop_back();
  std::reverse(body.begin(), body.end()); // stackify

  return TokenHandler(std::move(body));
}

TokenHandler TokenHandler::getAllTokensUntilFirstOf(TokenType _type) {
  std::vector<Token> tokens;

  while (!token_list.back().is(_type)) {
    tokens.emplace_back(std::move(token_list.back()));
    token_list.pop_back();

    if (token_list.empty())
      throw std::runtime_error(
          "Did not find token in getAllTokensUntilFirstOf");
  }
  std::reverse(tokens.begin(), tokens.end());

  return TokenHandler(std::move(tokens));
}

TokenHandler TokenHandler::getAllTokensUntilLastOf(TokenType _type) {
  auto last_of = std::find_if(token_list.begin(), token_list.end(),
                              [_type](const Token &t) { return t.is(_type); });

  if (last_of == token_list.end())
    throw std::runtime_error("Did not find token in getAllTokensUntilLastOf");

  size_t num = std::distance(last_of, token_list.end()) - 1;
  std::vector<Token> tokens;
  tokens.reserve(num);

  for (size_t i = 0; i < num; ++i) {
    tokens.emplace_back(std::move(token_list.back()));
    token_list.pop_back();
  }

  std::reverse(tokens.begin(), tokens.end());
  return TokenHandler(std::move(tokens));
}

void TokenHandler::for_all(std::function<void(Token &)> f) {

  auto curr = token_list.rbegin();
  auto end = token_list.rend();

  while (curr != end) {
    f(*curr);
    ++curr;
  }
}

/* TokenHandler Methods */
