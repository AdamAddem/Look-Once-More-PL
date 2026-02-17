#pragma once
#include <string>
#include <variant>
#include <vector>

namespace Lexer {

enum class TokenType {

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
  explicit Token(const TokenType _type) : type(_type) {};
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

	void throw_if(TokenType unwanted_type, const char* err_message) const;
	void throw_if_not(TokenType expected_type, const char* err_message) const;

  [[nodiscard]] bool is(const TokenType _type) const { return type == _type; }
  [[nodiscard]] bool isPrimitive() const;
  [[nodiscard]] bool isLiteral() const;
  [[nodiscard]] std::string toString();
  [[nodiscard]] std::string toDebugString() const;
  [[nodiscard]] int getInt() const;
  [[nodiscard]] float getFloat() const;
  [[nodiscard]] double getDouble() const;
  [[nodiscard]] bool getBool() const;
  [[nodiscard]] std::string takeString();
};

class TokenHandler {
  std::vector<Token> token_list;

public:
  explicit TokenHandler(std::vector<Token> &&_tokens)
      : token_list(std::move(_tokens)) {};

  TokenHandler() = default;
  TokenHandler(TokenHandler &&other) noexcept
      : token_list(std::move(other.token_list)) {}

  TokenHandler& operator=(TokenHandler &&other) noexcept {
    token_list = std::move(other.token_list);
    return *this;
  }

  [[nodiscard]] const Token &peek() const { return token_list.back(); };
  [[nodiscard]] const Token &peek_back() const { return token_list.front(); }
  [[nodiscard]] bool peek_is(const TokenType type) const {
    return token_list.back().type == type;
  }
  [[nodiscard]] const Token &peek_ahead(const std::size_t distance) {
    return token_list.at(token_list.size() - distance - 1);
  }

  Token eat() {
    Token t = std::move(token_list.back());
    token_list.pop_back();
    return t;
  }

  [[nodiscard]] bool check(TokenType _type) const { return token_list.back().type == _type; }
  [[nodiscard]] bool empty() const { return token_list.empty(); }
  [[nodiscard]] unsigned size() const { return token_list.size(); }

  void pop() { token_list.pop_back(); }
  bool pop_if(TokenType _type);
	void reject_then_pop(TokenType unwanted_type, const char* throw_message);
	void expect_then_pop(TokenType expected_type, const char* throw_message);

  void print();
  TokenHandler getTokensBetweenBraces();
  TokenHandler getTokensBetweenParenthesis();
  TokenHandler getTokensBetweenBrackets();
  TokenHandler getAllTokensUntilFirstOf(TokenType _type);
  TokenHandler getAllTokensUntilLastOf(TokenType _type);
};

[[nodiscard]] TokenHandler tokenizeFile(const std::string &file_path);

} // namespace Lexer
