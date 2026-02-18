#pragma once
#include <string>
#include <variant>
#include <vector>

namespace Lexer {

//because i'm a monkey, if you want to change this enum, make sure the order is exactly the same
//then, add your enum to the macros in lex.cpp
//then, add your string representation of your enum in tokenhandler.cpp in the EXACT spot you put it here
//reflection cannot come fast enough istg
enum class TokenType : unsigned {
  INVALID_TOKEN,
  IDENTIFIER,
  BEGIN_LITERALS,
  INT_LITERAL,
  FLOAT_LITERAL,
  DOUBLE_LITERAL,
  STRING_LITERAL,
  CHAR_LITERAL,
  BOOL_LITERAL,
  END_LITERALS,
  BEGIN_SYMBOLS,
  PLUS,
  PLUSPLUS,
  MINUS,
  MINUSMINUS,
  SLASH,
  STAR,
  POW,
  MOD,
  ASSIGN,
  LPAREN,
  RPAREN,
  LBRACE,
  RBRACE,
  LBRACKET,
  RBRACKET,
  LESS,
  GTR,
  LESSEQ,
  GTREQ,
  SEMI_COLON,
  ADDR,
  COMMA,
  END_SYMBOLS,
  BEGIN_COMP_BITWISE,
  KEYWORD_AND,
  KEYWORD_OR,
  KEYWORD_XOR,
  KEYWORD_NOT,
  KEYWORD_EQUALS,
  KEYWORD_NOT_EQUAL,
  KEYWORD_BITAND,
  KEYWORD_BITOR,
  KEYWORD_BITXOR,
  KEYWORD_BITNOT,
  END_COMP_BITWISE,
  BEGIN_PRIMITIVES,
  KEYWORD_i8,
  KEYWORD_i16,
  KEYWORD_i32,
  KEYWORD_i64,
  KEYWORD_u8,
  KEYWORD_u16,
  KEYWORD_u32,
  KEYWORD_u64,
  KEYWORD_f32,
  KEYWORD_f64,
  KEYWORD_CHAR,
  KEYWORD_STRING,
  KEYWORD_BOOL,
  KEYWORD_DEVOID,
  END_PRIMITIVES,
  BEGIN_POINTERS,
  KEYWORD_SELFISH,
  KEYWORD_SHARING,
  KEYWORD_WATCHING,
  KEYWORD_RAW,
  KEYWORD_VAGUE,
  END_POINTERS,
  BEGIN_CONTROL_FLOW,
  KEYWORD_IF,
  KEYWORD_ELSE,
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
  END_CONTROL_FLOW,
  BEGIN_CAST,
  KEYWORD_CAST,
  KEYWORD_CAST_IF,
  KEYWORD_UNSAFE_CAST,
  END_CAST,
  BEGIN_ALLOC_LIFETIMES,
  KEYWORD_STEAL,
  KEYWORD_BUILD_NEW,
  KEYWORD_ALLOCATE,
  KEYWORD_CONSTRUCT,
  END_ALLOC_LIFETIMES,
  KEYWORD_FROM,
  KEYWORD_AS,
  KEYWORD_GLOBAL,
  KEYWORD_GLOBALS,
  KEYWORD_MUT,
  KEYWORD_NULL,
  KEYWORD_JUNK
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

  void throw_if(TokenType unwanted_type, const char *err_message) const;

  void throw_if_not(TokenType expected_type, const char *err_message) const;

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

  TokenHandler &operator=(TokenHandler &&other) noexcept {
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

  [[nodiscard]] bool check(TokenType _type) const {
    return token_list.back().type == _type;
  }
  [[nodiscard]] bool empty() const { return token_list.empty(); }
  [[nodiscard]] unsigned size() const { return token_list.size(); }

  void pop() { token_list.pop_back(); }

  bool pop_if(TokenType _type);

  void reject_then_pop(TokenType unwanted_type, const char *throw_message);

  void expect_then_pop(TokenType expected_type, const char *throw_message);

  void print(unsigned initial_indent = 0);

  TokenHandler getTokensBetweenBraces();

  TokenHandler getTokensBetweenParenthesis();

  TokenHandler getTokensBetweenBrackets();

  TokenHandler getAllTokensUntilFirstOf(TokenType _type);

  TokenHandler getAllTokensUntilLastOf(TokenType _type);
};

[[nodiscard]] TokenHandler tokenizeFile(const std::string &file_path);
} // namespace Lexer
