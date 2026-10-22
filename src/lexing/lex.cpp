#include "lex.hpp"
#include "edenlib/shortened_macros.hpp"
#include "tokentype.hpp"
#include <cctype>
#include <cstdlib>
#include <string_view>
#include "error.hpp"
#include "edenlib/casts.hpp"

namespace LOM::Lexer {
namespace {

// code to make a perfect hash function was ai generated, need to verify / optimize
struct WordToken { std::string_view word; TokenType type; };
inline constexpr WordToken wordTokens[]{
  {"!", TokenType::INVALID_TOKEN},
  {"fn", TokenType::KEYWORD_FN},
  {"if", TokenType::KEYWORD_IF},
  {"else", TokenType::KEYWORD_ELSE},
  {"u8", TokenType::KEYWORD_u8},
  {"u16", TokenType::KEYWORD_u16},
  {"u32", TokenType::KEYWORD_u32},
  {"u64", TokenType::KEYWORD_u64},
  {"i8", TokenType::KEYWORD_i8},
  {"i16", TokenType::KEYWORD_i16},
  {"i32", TokenType::KEYWORD_i32},
  {"i64", TokenType::KEYWORD_i64},
  {"f32", TokenType::KEYWORD_f32},
  {"f64", TokenType::KEYWORD_f64},
  {"char", TokenType::KEYWORD_CHAR},
  {"string", TokenType::KEYWORD_STRING},
  {"bool", TokenType::KEYWORD_BOOL},
  {"raw", TokenType::KEYWORD_RAW},
  {"ref", TokenType::KEYWORD_REF},
  {"while", TokenType::KEYWORD_WHILE},
  {"return", TokenType::KEYWORD_RETURN},
  {"true", TokenType::BOOL_LITERAL},
  {"false", TokenType::BOOL_LITERAL},

  {"and", TokenType::KEYWORD_AND},
  {"or", TokenType::KEYWORD_OR},
  {"xor", TokenType::KEYWORD_XOR},
  {"not", TokenType::KEYWORD_NOT},
  {"eq", TokenType::KEYWORD_EQUALS},
  {"not_eq", TokenType::KEYWORD_NOT_EQUAL},
  {"bitand", TokenType::KEYWORD_BITAND},
  {"bitor", TokenType::KEYWORD_BITOR},
  {"bitxor", TokenType::KEYWORD_BITXOR},
  {"bitnot", TokenType::KEYWORD_BITNOT},

  {"cast", TokenType::KEYWORD_CAST},
  {"global", TokenType::KEYWORD_GLOBAL},
  {"null", TokenType::KEYWORD_NULL},
  {"junk", TokenType::KEYWORD_JUNK},
  {"default", TokenType::KEYWORD_DEFAULT},
  {"struct", TokenType::KEYWORD_STRUCT},
  {"pub", TokenType::KEYWORD_PUB},
  {"import", TokenType::KEYWORD_IMPORT},
  {"__C", TokenType::DUNDER_CEXTERN},
  {"__va", TokenType::DUNDER_VA},
};
constexpr sz_t numWordTokens = std::size(wordTokens);
constexpr sz_t INVALID_TOKEN_IDX = 0; static_assert(wordTokens[INVALID_TOKEN_IDX].type == TokenType::INVALID_TOKEN);

struct WordHash {
  u32_t firstWeight;
  u32_t lastWeight;

  std::array<u8_t, 256> indices;
  constexpr WordHash(u32_t a, u32_t b) : firstWeight(a), lastWeight(b) {
    indices.fill(INVALID_TOKEN_IDX);
  }

  constexpr sz_t bucket(std::string_view word) const noexcept {
    auto const a = firstWeight * (u8_t) word.front();
    auto const b = lastWeight * (u8_t) word.back();
    return (word.size() + a + b) & 255u;
  }
};

constexpr auto wordHash = [] consteval {
  static_assert(numWordTokens < 256);
  for (u32_t first{1}; first < 128; first += 2) {
    for (u32_t last{1}; last < 128; last += 2) {
      WordHash hash{first, last};
      bool collision = false;
      for (sz_t i{}; i < numWordTokens; ++i) {
        auto& index = hash.indices[ hash.bucket(wordTokens[i].word) ];
        if (index != INVALID_TOKEN_IDX) { collision = true; break; }
        index = u8_t(i);
      }
      if (not collision) return hash;
    }
  }
  throw "No perfect keyword hash: expand the table or the weight search";
}();

InlineNodiscardCXPR TokenType
classifyWord(std::string_view word) noexcept {
  auto const index = wordHash.indices[wordHash.bucket(word)];
  auto const& candidate = wordTokens[index];

  auto const condition = word == candidate.word;
  auto res = TokenType::IDENTIFIER;
  res = condition ? candidate.type : res;
  return res;
}

namespace Category {
enum CategoryEnum: u8_t {
   IDENT_START = 1 << 0,
IDENT_CONTINUE = 1 << 1,
         DIGIT = 1 << 2,
   WHITE_SPACE = 1 << 3,
       COMMENT = 1 << 4,
      END_LINE = 1 << 5,
END_CHAR_LITERAL = 1 << 6,
END_STRING_LITERAL = 1 << 7,
};
}

constexpr u8_t LETTER_OR_UNDER = Category::IDENT_START | Category::IDENT_CONTINUE;
constexpr u8_t NUMBER          = Category::DIGIT       | Category::IDENT_CONTINUE;
constexpr u8_t NEWLINE         = Category::WHITE_SPACE | Category::END_LINE | Category::END_STRING_LITERAL | Category::END_CHAR_LITERAL;
constexpr u8_t FILE_EOF        = Category::END_LINE    | Category::END_STRING_LITERAL | Category::END_CHAR_LITERAL;
constexpr u8_t WHITE_SPACE     = Category::WHITE_SPACE;
constexpr u8_t COMMENT         = Category::COMMENT;
constexpr u8_t SINGLE_QUOTE    = Category::END_CHAR_LITERAL;
constexpr u8_t DOUBLE_QUOTE    = Category::END_STRING_LITERAL;

constexpr auto charCategoryTable = [] {
  std::array<u8_t, 256> table{};
  static constexpr auto letter_span = 'z' - 'a';
  for (sz_t i{}; i <= letter_span; ++i) {
    table[i + (sz_t) 'a'] = LETTER_OR_UNDER;
    table[i + (sz_t) 'A'] = LETTER_OR_UNDER;
  }
  table['_'] = LETTER_OR_UNDER;

  static constexpr auto num_span = '9' - '0';
  for (sz_t i{}; i <= num_span; ++i)
    table[i + (sz_t) '0'] = NUMBER;

  table[' '] =  WHITE_SPACE;
  table['\t'] = WHITE_SPACE;
  table['\n'] = NEWLINE;
  table['\r'] = WHITE_SPACE;
  table['\f'] = WHITE_SPACE;
  table['\t'] = WHITE_SPACE;

  table['#'] = COMMENT;
  table['\''] = SINGLE_QUOTE;
  table['"'] = DOUBLE_QUOTE;
  table[ File::EOF_CHAR ] = FILE_EOF;
  return table;
}();

InlineNodiscardCXPR u8_t categorize(char c) noexcept { return charCategoryTable[(sz_t)(unsigned char)c]; }

template <Category::CategoryEnum category>
InlineNodiscardCXPR bool is(char c) noexcept {
  return categorize(c) & category;
}

class LexingContext {
  eden::vector<Token>& token_list;
  File file;
  u32_t current_position{};
  bool has_errors{};

  InlineCXPR explicit LexingContext(eden::vector<Token>& token_list, File file)
  : token_list(token_list), file(file) {}

  PureFN InlineNodiscardCXPR char peek() const noexcept { return file.getText()[current_position]; }
  PureFN InlineNodiscardCXPR std::pair<u8_t, u8_t> peek_two() const noexcept { return { file.getText()[current_position], file.getText()[current_position + 1] }; }
  PureFN InlineNodiscardCXPR u16_t peek_two_as_u16() const noexcept { return *(u16_t*) (file.getText().data() + current_position); }
  PureFN InlineNodiscardCXPR char peek_ahead(i64_t i = 1) const noexcept { return file.getText()[current_position + i]; }
  PureFN InlineNodiscardCXPR char previous() const noexcept { return file.getText()[current_position - 1]; }

  InlineNodiscardCXPR char take() noexcept { return file.getText()[current_position++]; }
  InlineCXPR          void pop() noexcept { ++current_position; }
  InlineCXPR          void undo() noexcept { --current_position; }

  NoInlineCold void
  error_at_currentpos(std::string msg) {
    reportError(file, 1, current_position, std::move(msg)); has_errors = true;
  }

#define pre assert(peek() == '"');
  constexpr void grabStringLiteral() noexcept { pre
    pop(); // "

    Token new_token{ TokenType::STRING_LITERAL, 0, current_position };
    auto c = peek();
    while (not is<Category::END_STRING_LITERAL>(c)) {
      if ( c == '\\') {
        new_token.type = TokenType::ESCAPED_STRING_LITERAL;
        pop();
      }

      pop();
      c = peek();
    }

    if (c == '\"') {
      new_token.length = current_position - new_token.position;
      token_list.emplace_back(new_token);
      pop();
    }
    else {
      error_at_currentpos("Expected ending \" in string literal.");
      if (c != File::EOF_CHAR) pop();
    }
  }
#undef pre

#define pre assert(peek() == '\'');
  constexpr void grabCharLiteral() noexcept { pre
    pop(); // '
    Token new_token{ TokenType::CHAR_LITERAL, 1, current_position };

    auto const c = take();
    if( is<Category::END_CHAR_LITERAL>(c) ) {
      error_at_currentpos("Incomplete char literal.");
      if(c == File::EOF_CHAR) undo();
      return;
    }

    if(c == '\\') {
      new_token.type = TokenType::ESCAPED_CHAR_LITERAL;
      ++new_token.length;

      if(auto const next = take(); is<Category::END_LINE>(next)) {
        error_at_currentpos("Incomplete escaped char literal.");
        if(next == File::EOF_CHAR) undo();
        return;
      }
    }

    if(auto const next = take(); next != '\'') {
      error_at_currentpos("Incomplete char literal.");
      if(next == File::EOF_CHAR) undo();
      return;
    }

    token_list.emplace_back(new_token);
  }
#undef pre

  constexpr void grabSymbol() noexcept {
    // all this overengineering shaves like, 1% of the lexing time?
    struct SymbolMapping {
      bool is_double : 1 = false;
      TokenType type : 7 = TokenType::INVALID_TOKEN;

      consteval SymbolMapping() = default;
      consteval SymbolMapping(TokenType t, bool is_double = false) : is_double(is_double), type(t) { }
    };

    static constexpr auto combine = [] (u8_t first, u8_t second) {
      return sz_t( ( u16_t(first) << 7 ) | u16_t(second) );
    };
    static constexpr auto symbol_map = [] consteval {
      using enum TokenType;
      static constexpr auto sz = 0b00111111'11111111;
      std::array<SymbolMapping, sz> twotokens_to_type{};

      // to add a symbol, follow the pattern shown
      for(sz_t i{}; i<i8_max; ++i) {
        twotokens_to_type[ combine('+', i) ].type = PLUS;
        twotokens_to_type[ combine('-', i) ].type = MINUS;
        twotokens_to_type[ combine('<', i) ].type = LESS;
        twotokens_to_type[ combine('>', i) ].type = GTR;
        twotokens_to_type[ combine('!', i) ].type = KEYWORD_NOT;
        twotokens_to_type[ combine('=', i) ].type = ASSIGN;
        twotokens_to_type[ combine('/', i) ].type = SLASH;
        twotokens_to_type[ combine('*', i) ].type = STAR;
        twotokens_to_type[ combine('%', i) ].type = MOD;
        twotokens_to_type[ combine('(', i) ].type = LPAREN;
        twotokens_to_type[ combine(')', i) ].type = RPAREN;
        twotokens_to_type[ combine('{', i) ].type = LBRACE;
        twotokens_to_type[ combine('}', i) ].type = RBRACE;
        twotokens_to_type[ combine('[', i) ].type = LBRACKET;
        twotokens_to_type[ combine(']', i) ].type = RBRACKET;
        twotokens_to_type[ combine('@', i) ].type = ADDR;
        twotokens_to_type[ combine('&', i) ].type = AMPERSAND;
        twotokens_to_type[ combine(',', i) ].type = COMMA;
        twotokens_to_type[ combine(':', i) ].type = COLON;
        twotokens_to_type[ combine('$', i) ].type = DOLLAR;
        twotokens_to_type[ combine(';', i) ].type = SEMI_COLON;
        twotokens_to_type[ combine('.', i) ].type = DOT;
      }

      // add two character symbols here
      twotokens_to_type[ combine('+', '+') ] = {PLUSPLUS, true};
      twotokens_to_type[ combine('-', '-') ] = {MINUSMINUS, true};
      twotokens_to_type[ combine('-', '>') ] = {ARROW, true};
      twotokens_to_type[ combine('<', '=') ] = {LESSEQ, true};
      twotokens_to_type[ combine('>', '=') ] = {GTREQ, true};
      twotokens_to_type[ combine('!', '=') ] = {KEYWORD_NOT_EQUAL, true};
      twotokens_to_type[ combine('=', '=') ] = {KEYWORD_EQUALS, true};
      twotokens_to_type[ combine(':', ':') ] = {DOUBLE_COLON, true};

      return twotokens_to_type;
    }();

    auto const pos = current_position;
    auto const [first, second] = peek_two();
    auto const idx = combine(first, second);
    auto const [is_double, type] = symbol_map[idx];
    current_position += 1 + u32_t(is_double);
    token_list.emplace_back(type, eden::narrow<u16_t>(current_position - pos), pos);

    if (type == TokenType::INVALID_TOKEN)
      error_at_currentpos(std::format("Unrecognized symbol ( {} ).", (char) first));
  }

  constexpr void grabNumber() noexcept {
    auto skip_digits = [&] { do { pop(); } while( categorize(peek()) == NUMBER ); };

    Token new_token{ TokenType::INTEGER_LITERAL, 0, current_position };
    skip_digits();

    if(peek() == '.') {
      new_token.type = TokenType::DOUBLE_LITERAL;
      skip_digits();
      if(peek() == '.') error_at_currentpos("Repeated decimal point in float literal.");
    }

    new_token.length = current_position - new_token.position;

    // cmov my beloved : )
    auto const condition = peek() == 'f';
    new_token.type = condition ? TokenType::FLOAT_LITERAL : new_token.type;
    current_position += u32_t(condition); // pop f suffix if present

    token_list.emplace_back(new_token);
  }

  constexpr void grabIdentOrKeyword() noexcept {
    auto const pos = current_position;
    do { pop(); } while( is<Category::IDENT_CONTINUE>(peek()) );

    auto const length = current_position - pos;
    auto const word = file.getText().substr(pos, length);
    auto const type = classifyWord(word);

    token_list.emplace_back(type, length, pos);
  }

  constexpr void skipWhiteSpace() noexcept {
    while ( is<Category::WHITE_SPACE>(peek()) ) pop();
  }

  constexpr void skipComments() noexcept {
    pop();

    if (peek() != '{') {
      while ( not is<Category::END_LINE>(peek()) ) pop();
      return;
    }
    pop(); // '{'

    static constexpr auto to_u16 = [] (u8_t a, u8_t b) constexpr { return ( u16_t(a) << 8 ) bitor u16_t(b); };
    sz_t nested{1};
    while (true) {
      auto const pair = peek_two();

      auto const as_u16 = to_u16(pair.first, pair.second);
      switch(as_u16) {
      case to_u16('#', '{'): current_position += 2; ++nested; continue;
      case to_u16('}', '#'): current_position += 2; --nested; if (nested == 0) return; continue;

      default:
        if (pair.first == File::EOF_CHAR) return;
        pop();
      }

      /* TODO: Possibly more optimized, test
      auto const is_opening = as_u16 == to_u16('#', '{');
      auto const is_closing = as_u16 == to_u16('}', '#');
      auto const not_eof = pair.first != File::EOF_CHAR;

      current_position += u32_t(not_eof) + u32_t(is_opening) + u32_t(is_closing);
      nested += u32_t(is_opening) - u32_t(is_closing);

      auto const exit_condition1 = nested == 0;
      auto const exit_condition2 = not not_eof;
      if (exit_condition1 | exit_condition2) return;
      */
    }
  }
public:

  constexpr static bool tokenize(eden::vector<Token>& out_tokens, File file) noexcept {
    LexingContext tokenizer{out_tokens, file};

    [&] {
      while (true) {
        tokenizer.skipWhiteSpace();
        auto const c = tokenizer.peek();
        switch(categorize(c)) {
        case LETTER_OR_UNDER: [[likely]] tokenizer.grabIdentOrKeyword(); break;
        case NUMBER: tokenizer.grabNumber(); break;
        case COMMENT: tokenizer.skipComments(); break;
        case SINGLE_QUOTE: tokenizer.grabCharLiteral(); break;
        case DOUBLE_QUOTE: tokenizer.grabStringLiteral(); break;

        default: [[likely]] tokenizer.grabSymbol(); break;

        case FILE_EOF: [[unlikely]] return;
        }
      }
    }();

    auto const invalid_token = Token(TokenType::INVALID_TOKEN, 1, out_tokens.back().position);
    for (sz_t i{}; i < INVALID_TOKEN_PADDING; ++i)
      out_tokens.push_back(invalid_token);

    return tokenizer.has_errors;
  }
};

}

[[nodiscard]] bool tokenizeFile(eden::vector<Token>& out_tokens, File file) noexcept {
  return LexingContext::tokenize(out_tokens, file);
}

}

// TODO: Add more tests
namespace LOM::LexerTest {
namespace {
using namespace std::string_view_literals;

consteval Lexer::Token request(std::string_view single_token_str) {
  std::string file_str{single_token_str};
  file_str += "\0..."sv;
  auto const file = File{ std::string_view(file_str) };

  eden::vector<Lexer::Token> tokens;
  Lexer::LexingContext::tokenize(tokens, file);
  if (tokens.size() != 1 + Lexer::INVALID_TOKEN_PADDING) throw "Expected one token in request.";
  return tokens[0];
}
static_assert(request("::").is(Lexer::TokenType::DOUBLE_COLON));

consteval bool CharLiterals() {
  constexpr auto file = File{
   "'A' 'B''C'"   "\n"
   "\0..."sv
  };

  eden::vector<Lexer::Token> tokens;
  Lexer::LexingContext::tokenize(tokens, file);

  if (tokens.size() != 3 + Lexer::INVALID_TOKEN_PADDING)    throw "Expected three tokens in char literal test.";
  for (sz_t i{}; i<3; ++i) {
    if ( tokens[i].type != Lexer::TokenType::CHAR_LITERAL ) throw "Non-char literal in char literal test.";
    if ( tokens[i].length != 1 )                            throw "Unescaped char literal does not have length 1.";
    if ( (sz_t) tokens[i].getChar(file) != 'A' + i )        throw "Char literal has incorrect value.";
  }

  return true;
}

consteval bool EscapedCharLiterals() {
  constexpr auto file = File{
    "'\\n' '\\t' '\\r'"   "\n"
    "\0..."sv
  };

  eden::vector<Lexer::Token> tokens;
  Lexer::LexingContext::tokenize(tokens, file);

  if (tokens.size() != 3 + Lexer::INVALID_TOKEN_PADDING) throw "Expected three tokens in escaped char literal test.";

  for (sz_t i{}; i<3; ++i) {
    if ( tokens[i].type != Lexer::TokenType::ESCAPED_CHAR_LITERAL ) throw "Non-escaped-char literal in escaped char literal test.";
    if ( tokens[i].length != 2 ) throw "Escaped char does not have length 2.";
  }

  if ( tokens[0].getEscapedChar(file) != '\n' ) throw "Escaped Char literal has incorrect value.";
  if ( tokens[1].getEscapedChar(file) != '\t' ) throw "Escaped Char literal has incorrect value.";
  if ( tokens[2].getEscapedChar(file) != '\r' ) throw "Escaped Char literal has incorrect value.";

  return true;
}

consteval bool StringLiterals() {
  constexpr auto file = File{
    " \"Hello, World!\""      "\n"
    " \"Back\"\"To\"\"Back\"" "\n" // TODO: Implement string literal concatenation
    "\0..."sv
  };

  eden::vector<Lexer::Token> tokens;
  Lexer::LexingContext::tokenize(tokens, file);

  if (tokens.size() != 4 + Lexer::INVALID_TOKEN_PADDING)    throw "Expected four tokens in string literal test.";

  for (sz_t i{}; i<4; ++i)
    if (tokens[i].type != Lexer::TokenType::STRING_LITERAL) throw "Non string literal in string literal test.";

  if (tokens[0].getString(file) != "Hello, World!") throw "String literal differs from text.";
  if (tokens[1].getString(file) != "Back")          throw "String literal differs from text.";
  if (tokens[2].getString(file) != "To")            throw "String literal differs from text.";
  if (tokens[3].getString(file) != "Back")          throw "String literal differs from text.";

  return true;
}

static_assert(CharLiterals());
static_assert(EscapedCharLiterals());
static_assert(StringLiterals());

}
}