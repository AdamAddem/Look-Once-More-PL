#pragma once
#include <functional>
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
  BOOL_LITERAL,   // true

  // Symbols
  PLUS,         // +
  PLUSPLUS,     // ++
  MINUS,        // -
  MINUSMINUS,   // --
  SLASH,        // /
  STAR,         // *
  POW,          // ^
  MOD,          // %
  PLUS_ASSIGN,  // +=
  MINUS_ASSIGN, // -=
  MULT_ASSIGN,  // *=
  DIV_ASSIGN,   // /=
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
  KEYWORD_STRING,
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

using TokenValue = std::variant<int, float, double, std::string>;
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
  bool check(TokenType _type) const { return token_list.back().type == _type; }
  bool empty() const { return token_list.empty(); }
  unsigned size() const { return token_list.size(); }

  bool pop_if(TokenType _type);
  void print();
  void for_all(std::function<void(Token &)>);
  TokenHandler getTokensBetweenBraces();
  TokenHandler getTokensBetweenParenthesis();
  TokenHandler getTokensBetweenBrackets();
  TokenHandler getAllTokensUntilFirstOf(TokenType _type);
  TokenHandler getAllTokensUntilLastOf(TokenType _type);
};

[[nodiscard]] TokenHandler tokenizeFile(const std::string &file_path);

} // namespace Lexer
