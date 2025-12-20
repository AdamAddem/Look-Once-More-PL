#include "lex.hpp"
#include <cctype>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <unordered_map>
#include <vector>
using namespace Lexer;

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

  token_list.emplace_back(STRING_LITERAL, literal);
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

  token_list.emplace_back(CHAR_LITERAL, std::string{c1});
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
  TokenType type = INT_LITERAL;
  TokenValue value;
  std::string num;
  char c;
  while ((c = file.get()) != EOF) { // i hate file handling so much, replace
                                    // this stupid monkey code eventually
    if (c == 'f') {
      type = FLOAT_LITERAL;
      break;
    }

    if (c == '.') {
      type = DOUBLE_LITERAL;
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
  case INT_LITERAL:
    value = std::stoi(num);
    break;
  case FLOAT_LITERAL:
    value = std::stof(num);
    break;
  case DOUBLE_LITERAL:
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

  if (keywords.contains(word)) {
    type = keywords[word];
  } else {
    type = IDENTIFIER;
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
    case IDENTIFIER:
      std::cout << "IDENTIFIER: " << std::get<std::string>(t.value);
      break;

    case INT_LITERAL:
      std::cout << "INT_LITERAL: " << std::get<int>(t.value);
      break;

    case FLOAT_LITERAL:
      std::cout << "FLOAT_LITERAL: " << std::get<float>(t.value);
      break;

    case DOUBLE_LITERAL:
      std::cout << "DOUBLE_LITERAL: " << std::get<double>(t.value);
      break;

    case CHAR_LITERAL:
      std::cout << "CHAR_LITERAL: " << std::get<std::string>(t.value);
      break;

    case STRING_LITERAL:
      std::cout << "STRING_LITERAL: " << std::get<std::string>(t.value);
      break;

    default:
      if (keywordToString.contains(t.type))
        std::cout << "KEYWORD: " << keywordToString[t.type];
      else if (symbolToString.contains(t.type))
        std::cout << "sym: " << symbolToString[t.type];
      else
        std::cout << "ERROR: " << t.type;
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
  case KEYWORD_INT:
  case KEYWORD_UINT:
  case KEYWORD_FLOAT:
  case KEYWORD_DOUBLE:
  case KEYWORD_CHAR:
  case KEYWORD_UCHAR:
  case KEYWORD_BOOL:
  case KEYWORD_SHORT:
  case KEYWORD_LONG:
  case KEYWORD_SIGNED:
  case KEYWORD_UNSIGNED:
    return true;

  default:
    return false;
  }
}

bool Token::isLiteral() const {
  switch (type) {
  case INT_LITERAL:
  case FLOAT_LITERAL:
  case DOUBLE_LITERAL:
  case CHAR_LITERAL:
  case STRING_LITERAL:
    return true;

  default:
    return false;
  }
}

std::string Token::toString() {
  static std::unordered_map<TokenType, std::string> tokenToString{
      KEYWORDS_TO_STRING_MAPPING SYMBOLS_TO_STRING_MAPPING};
  if (type == IDENTIFIER)
    return std::get<std::string>(value);

  if (isLiteral()) {
    switch (type) {
    case INT_LITERAL:
      return std::to_string(std::get<int>(value));
    case FLOAT_LITERAL:
      return std::to_string(std::get<float>(value));
    case DOUBLE_LITERAL:
      return std::to_string(std::get<double>(value));
    case CHAR_LITERAL:
      return std::to_string((char)std::get<int>(value));
    case STRING_LITERAL:
      return std::get<std::string>(value);

    default:
      throw std::runtime_error("Error in isLiteral method");
    }
  }

  return tokenToString[type];
}

int Token::getInt() const { return std::get<int>(value); }
float Token::getFloat() const { return std::get<float>(value); }
double Token::getDouble() const { return std::get<double>(value); }
bool Token::getBool() const { return std::get<bool>(value); }
std::string Token::takeString() {
  return std::get<std::string>(std::move(value));
}

/* Token Methods */

/* TokenHandler Methods */

TokenHandler TokenHandler::getTokensBetweenBraces() {
  std::vector<Lexer::Token> body;
  int openbrace = 1;
  while (openbrace) {
    if (token_list.empty())
      throw std::runtime_error("Expected closing rbrace");

    if (token_list.back().type == LBRACE)
      ++openbrace;
    else if (token_list.back().type == RBRACE)
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

    if (token_list.back().type == LPAREN)
      ++openbrace;
    else if (token_list.back().type == RPAREN)
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

    if (token_list.back().type == LBRACKET)
      ++openbracket;
    else if (token_list.back().type == RBRACKET)
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
