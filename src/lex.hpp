#pragma once
#include <functional>
#include <iostream>
#include <string>
#include <variant>
#include <vector>

namespace Lexer {

enum TokenType {

  IDENTIFIER,

  // Literals
  INT_LITERAL,    // 5
  FLOAT_LITERAL,  // 5.0f
  DOUBLE_LITERAL, // 5.0
  STRING_LITERAL, // "5"
  CHAR_LITERAL,   // '5'

  // Symbols
  PLUS,         // +
  PLUSPLUS,     // ++
  MINUS,        // -
  MINUSMINUS,   // --
  DIV,          // /
  MULT,         // *
  POW,          // ^
  MOD,          // %
  PLUS_ASSIGN,  // +=
  MINUS_ASSIGN, // -=
  DIV_ASSIGN,   // /=
  MULT_ASSIGN,  // *=
  POW_ASSIGN,   // ^=
  MOD_ASSIGN,   // %=
  ASSIGN,       // =
  LPAREN,       // (
  RPAREN,       // )
  LBRACE,       // {
  RBRACE,       // }
  LBRACKET,     // [
  RBRACKET,     // ]
  LESS,         // <
  GTR,          // >
  LESSEQ,       // <=
  GTREQ,        // >=
  SEMI_COLON,   // ;
  ADDR,         // @
  COMMA,        // ,

  // Keywords
  KEYWORD_AND,
  KEYWORD_OR,
  KEYWORD_XOR,
  KEYWORD_NOT,
  KEYWORD_EQUALS,
  KEYWORD_BITAND,
  KEYWORD_BITOR,
  KEYWORD_BITXOR,
  KEYWORD_BITNOT,
  KEYWORD_INT,
  KEYWORD_UINT,
  KEYWORD_FLOAT,
  KEYWORD_DOUBLE,
  KEYWORD_CHAR,
  KEYWORD_UCHAR,
  KEYWORD_BOOL,
  KEYWORD_SHORT,
  KEYWORD_LONG,
  KEYWORD_SIGNED,
  KEYWORD_UNSIGNED,
  KEYWORD_NULL,
  KEYWORD_DEVOID,
  KEYWORD_JUNK,
  KEYWORD_SELFISH,
  KEYWORD_SHARING,
  KEYWORD_WATCHING,
  KEYWORD_RAW,
  KEYWORD_VAGUE,
  KEYWORD_IF,
  KEYWORD_ELSE,
  KEYWORD_ELIF,
  KEYWORD_FOR,
  KEYWORD_WHILE,
  KEYWORD_DO,
  KEYWORD_RETURN,
  KEYWORD_SWITCH,
  KEYWORD_CASE,
  KEYWORD_DEFAULT,
  KEYWORD_GOTO,
  KEYWORD_BREAK,
  KEYWORD_CONTINUE,
  KEYWORD_CAST,
  KEYWORD_CAST_IF,
  KEYWORD_UNSAFE_CAST,
  KEYWORD_VERY_UNSAFE_CAST,
  KEYWORD_STEAL,
  KEYWORD_BUILD_NEW,
  KEYWORD_ALLOCATE,
  KEYWORD_CONSTRUCT,
  KEYWORD_AUTO,
  KEYWORD_CONST,
  KEYWORD_EXCEPT,
  KEYWORD_STATIC,
  KEYWORD_EXTERN,
  KEYWORD_TRUE,
  KEYWORD_FALSE,
  KEYWORD_FROM,
  KEYWORD_AS,
  KEYWORD_GLOBAL,
  KEYWORD_GLOBALS,
};
// this is so stupid dude

#define STRING_TO_KEYWORDS_MAPPING                                             \
  {"and", KEYWORD_AND}, {"or", KEYWORD_OR}, {"xor", KEYWORD_XOR},              \
      {"not", KEYWORD_NOT}, {"equals", KEYWORD_EQUALS},                        \
      {"bitand", KEYWORD_BITAND}, {"bitor", KEYWORD_BITOR},                    \
      {"bitxor", KEYWORD_BITXOR}, {"bitnot", KEYWORD_BITNOT},                  \
      {"int", KEYWORD_INT}, {"uint", KEYWORD_UINT}, {"float", KEYWORD_FLOAT},  \
      {"double", KEYWORD_DOUBLE}, {"char", KEYWORD_CHAR},                      \
      {"uchar", KEYWORD_UCHAR}, {"bool", KEYWORD_BOOL},                        \
      {"short", KEYWORD_SHORT}, {"long", KEYWORD_LONG},                        \
      {"signed", KEYWORD_SIGNED}, {"unsigned", KEYWORD_UNSIGNED},              \
      {"null", KEYWORD_NULL}, {"devoid", KEYWORD_DEVOID},                      \
      {"junk", KEYWORD_JUNK}, {"selfish", KEYWORD_SELFISH},                    \
      {"sharing", KEYWORD_SHARING}, {"watching", KEYWORD_WATCHING},            \
      {"raw", KEYWORD_RAW}, {"vague", KEYWORD_VAGUE}, {"if", KEYWORD_IF},      \
      {"else", KEYWORD_ELSE}, {"elif", KEYWORD_ELIF}, {"for", KEYWORD_FOR},    \
      {"while", KEYWORD_WHILE}, {"do", KEYWORD_DO},                            \
      {"return", KEYWORD_RETURN}, {"switch", KEYWORD_SWITCH},                  \
      {"case", KEYWORD_CASE}, {"default", KEYWORD_DEFAULT},                    \
      {"goto", KEYWORD_GOTO}, {"break", KEYWORD_BREAK},                        \
      {"continue", KEYWORD_CONTINUE}, {"cast", KEYWORD_CAST},                  \
      {"cast_if", KEYWORD_CAST_IF}, {"unsafe_cast", KEYWORD_UNSAFE_CAST},      \
      {"very_unsafe_cast", KEYWORD_VERY_UNSAFE_CAST},                          \
      {"steal", KEYWORD_STEAL}, {"build_new", KEYWORD_BUILD_NEW},              \
      {"allocate", KEYWORD_ALLOCATE}, {"construct", KEYWORD_CONSTRUCT},        \
      {"auto", KEYWORD_AUTO}, {"const", KEYWORD_CONST},                        \
      {"except", KEYWORD_EXCEPT}, {"static", KEYWORD_STATIC},                  \
      {"extern", KEYWORD_EXTERN}, {"true", KEYWORD_TRUE},                      \
      {"false", KEYWORD_FALSE}, {"from", KEYWORD_FROM}, {"as", KEYWORD_AS},    \
      {"global", KEYWORD_GLOBAL}, {"globals", KEYWORD_GLOBALS},

#define KEYWORDS_TO_STRING_MAPPING                                             \
  {KEYWORD_AND, "and"}, {KEYWORD_OR, "or"}, {KEYWORD_XOR, "xor"},              \
      {KEYWORD_NOT, "not"}, {KEYWORD_EQUALS, "equals"},                        \
      {KEYWORD_BITAND, "bitand"}, {KEYWORD_BITOR, "bitor"},                    \
      {KEYWORD_BITXOR, "bitxor"}, {KEYWORD_BITNOT, "bitnot"},                  \
      {KEYWORD_INT, "int"}, {KEYWORD_UINT, "uint"}, {KEYWORD_FLOAT, "float"},  \
      {KEYWORD_DOUBLE, "double"}, {KEYWORD_CHAR, "char"},                      \
      {KEYWORD_UCHAR, "uchar"}, {KEYWORD_BOOL, "bool"},                        \
      {KEYWORD_SHORT, "short"}, {KEYWORD_LONG, "long"},                        \
      {KEYWORD_SIGNED, "signed"}, {KEYWORD_UNSIGNED, "unsigned"},              \
      {KEYWORD_NULL, "null"}, {KEYWORD_DEVOID, "devoid"},                      \
      {KEYWORD_JUNK, "junk"}, {KEYWORD_SELFISH, "selfish"},                    \
      {KEYWORD_SHARING, "sharing"}, {KEYWORD_WATCHING, "watching"},            \
      {KEYWORD_RAW, "raw"}, {KEYWORD_VAGUE, "vague"}, {KEYWORD_IF, "if"},      \
      {KEYWORD_ELSE, "else"}, {KEYWORD_ELIF, "elif"}, {KEYWORD_FOR, "for"},    \
      {KEYWORD_WHILE, "while"}, {KEYWORD_DO, "do"},                            \
      {KEYWORD_RETURN, "return"}, {KEYWORD_SWITCH, "switch"},                  \
      {KEYWORD_CASE, "case"}, {KEYWORD_DEFAULT, "default"},                    \
      {KEYWORD_GOTO, "goto"}, {KEYWORD_BREAK, "break"},                        \
      {KEYWORD_CONTINUE, "continue"}, {KEYWORD_CAST, "cast"},                  \
      {KEYWORD_CAST_IF, "cast_if"}, {KEYWORD_UNSAFE_CAST, "unsafe_cast"},      \
      {KEYWORD_VERY_UNSAFE_CAST, "very_unsafe_cast"},                          \
      {KEYWORD_STEAL, "steal"}, {KEYWORD_BUILD_NEW, "build_new"},              \
      {KEYWORD_ALLOCATE, "allocate"}, {KEYWORD_CONSTRUCT, "construct"},        \
      {KEYWORD_AUTO, "auto"}, {KEYWORD_CONST, "const"},                        \
      {KEYWORD_EXCEPT, "except"}, {KEYWORD_STATIC, "static"},                  \
      {KEYWORD_EXTERN, "extern"}, {KEYWORD_TRUE, "true"},                      \
      {KEYWORD_FALSE, "false"}, {KEYWORD_FROM, "from"}, {KEYWORD_AS, "as"},    \
      {KEYWORD_GLOBAL, "global"}, {KEYWORD_GLOBALS, "globals"},

#define STRING_TO_SYMBOLS_MAPPING                                              \
  {"+", PLUS}, {"++", PLUSPLUS}, {"-", MINUS}, {"--", MINUSMINUS}, {"/", DIV}, \
      {"*", MULT}, {"^", POW}, {"%", MOD}, {"+=", PLUS_ASSIGN},                \
      {"-=", MINUS_ASSIGN}, {"/=", DIV_ASSIGN}, {"*=", MULT_ASSIGN},           \
      {"^=", POW_ASSIGN}, {"%=", MOD_ASSIGN}, {"=", ASSIGN}, {"(", LPAREN},    \
      {")", RPAREN}, {"{", LBRACE}, {"}", RBRACE}, {"[", LBRACKET},            \
      {"]", RBRACKET}, {"<", LESS}, {">", GTR}, {"<=", LESSEQ}, {">=", GTREQ}, \
      {";", SEMI_COLON}, {"@", ADDR}, {",", COMMA},

#define SYMBOLS_TO_STRING_MAPPING                                              \
  {PLUS, "+"}, {PLUSPLUS, "++"}, {MINUS, "-"}, {MINUSMINUS, "--"}, {DIV, "/"}, \
      {MULT, "*"}, {POW, "^"}, {MOD, "%"}, {PLUS_ASSIGN, "+="},                \
      {MINUS_ASSIGN, "-="}, {DIV_ASSIGN, "/="}, {MULT_ASSIGN, "*="},           \
      {POW_ASSIGN, "^="}, {MOD_ASSIGN, "%="}, {ASSIGN, "="}, {LPAREN, "("},    \
      {RPAREN, ")"}, {LBRACE, "{"}, {RBRACE, "}"}, {LBRACKET, "["},            \
      {RBRACKET, "]"}, {LESS, "<"}, {GTR, ">"}, {LESSEQ, "<="}, {GTREQ, ">="}, \
      {SEMI_COLON, ";"}, {ADDR, "@"}, {COMMA, ","},

using TokenValue = std::variant<int, float, double, bool, std::string>;
struct Token {
  explicit Token(TokenType _type) : type(_type) {};
  Token(TokenType _type, TokenValue &&_value)
      : type(_type), value(std::move(_value)) {};

  Token(const Token &) = default;
  Token(Token &&other) noexcept
      : type(other.type), value(std::move(other.value)) {}

  Token &operator=(const Token &) = default;
  Token &operator=(Token &&other) noexcept {
    type = other.type;
    value = std::move(other.value);
    return *this;
  }

  TokenType type;
  TokenValue value;

  bool is(TokenType _type) const { return type == _type; }

  bool isPrimitive() const;
  bool isLiteral() const;
  std::string toString();

  int getInt() const;
  float getFloat() const;
  double getDouble() const;
  bool getBool() const;
  std::string takeString();
};

class TokenHandler {
  std::vector<Token> token_list;

public:
  explicit TokenHandler(std::vector<Token> &&_tokens)
      : token_list(std::move(_tokens)) {};

  TokenHandler() = default;
  TokenHandler(TokenHandler &&other) noexcept
      : token_list(std::move(other.token_list)) {}

  void operator=(TokenHandler &&other) noexcept {
    token_list = std::move(other.token_list);
  }

  const Lexer::Token &peek() const { return token_list.back(); };
  const Lexer::Token &peek_back() const { return token_list.front(); }
  bool peek_is(TokenType _type) const {
    return token_list.back().type == _type;
  }

  const Lexer::Token &peek_ahead(std::size_t distance) {
    return token_list.at(token_list.size() - distance - 1);
  }

  Lexer::Token eat() {
    Lexer::Token t = std::move(token_list.back());
    token_list.pop_back();
    return t;
  }

  void pop() { token_list.pop_back(); }

  bool pop_if(TokenType _type) {
    if (token_list.empty())
      return false;

    if (token_list.back().type == _type) {
      token_list.pop_back();
      return true;
    }
    return false;
  }

  bool check(TokenType _type) const { return token_list.back().type == _type; }

  bool empty() const { return token_list.empty(); }
  unsigned size() const { return token_list.size(); }

  void print() {
    int back = token_list.size() - 1;

    for (; back >= 0; --back) {

      std::cout << token_list[back].toString() << std::endl;
    }
  }

  void for_all(std::function<void(Token &)>);
  TokenHandler getTokensBetweenBraces();
  TokenHandler getTokensBetweenParenthesis();
  TokenHandler getTokensBetweenBrackets();
  TokenHandler getAllTokensUntilFirstOf(TokenType _type);
  TokenHandler getAllTokensUntilLastOf(TokenType _type);
};
// Vector organized as stack, back is top is first token
[[nodiscard]] TokenHandler tokenizeFile(const std::string &file_path);

} // namespace Lexer
