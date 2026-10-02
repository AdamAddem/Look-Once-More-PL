#include "lex.hpp"
#include "edenlib/macros.hpp"
#include "tokentype.hpp"
#include <cassert>
#include <cctype>
#include <chrono>
#include <cstdlib>
#include <string_view>
#include "error.hpp"

using namespace LOM;
using namespace LOM::Lexer;


namespace {

// this code to make a perfect hash function was ai generated, need to verify / optimize
struct WordToken { std::string_view word; TokenType type; };
inline constexpr WordToken wordTokens[]{
  {"$", TokenType::INVALID_TOKEN},
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

/*
  {"INTEGER_LITERAL", TokenType::INTEGER_LITERAL},
  {"FLOAT_LITERAL", TokenType::FLOAT_LITERAL},
  {"DOUBLE_LITERAL", TokenType::DOUBLE_LITERAL},
  {"CHAR_LITERAL", TokenType::CHAR_LITERAL},
  {"BOOL_LITERAL", TokenType::BOOL_LITERAL},
  {"STRING_LITERAL", TokenType::STRING_LITERAL},
  {"ESCAPED_STRING_LITERAL", TokenType::ESCAPED_STRING_LITERAL}, */

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
constexpr sz_t INVALID_TOKEN_IDX = 0;

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

edenNodiscardCXPR TokenType classifyWord(std::string_view word) noexcept {
  auto const index = wordHash.indices[wordHash.bucket(word)];
  auto const& candidate = wordTokens[index];
  return word == candidate.word ? candidate.type : TokenType::IDENTIFIER;
}


enum CharCategory : u8_t { 
     IDENT_START = 1 << 0, 
  IDENT_CONTINUE = 1 << 1, 
           DIGIT = 1 << 2, 
           SPACE = 1 << 3, 
         COMMENT = 1 << 4, 
    FILE_EOF_CAT = 1 << 5, 
     NEWLINE_CAT = 1 << 6, 
};
constexpr u8_t LETTER_OR_UNDER = IDENT_START | IDENT_CONTINUE;
constexpr u8_t NUMBER = DIGIT | IDENT_CONTINUE;
constexpr u8_t NEWLINE = SPACE | NEWLINE_CAT;
constexpr u8_t EOF_OR_NEWLINE = FILE_EOF_CAT | NEWLINE_CAT;

constexpr auto charCategoryTable = [] {
  std::array<u8_t, 256> table{};
  static constexpr auto letter_span = 'z' - 'a';
  for (sz_t c{}; c <= letter_span; ++c) {
    table[c + (sz_t) 'a'] = LETTER_OR_UNDER;
    table[c + (sz_t) 'A'] = LETTER_OR_UNDER;
  }
  table['_'] = LETTER_OR_UNDER;

  static constexpr auto num_span = '9' - '0';
  for (sz_t c{}; c <= num_span; ++c)
    table[c + (sz_t) '0'] = NUMBER;
  
  table[' '] = SPACE;
  table['\t'] = SPACE;
  table['\n'] = NEWLINE;
  table['\r'] = SPACE;
  table['\f'] = SPACE;
  table['\t'] = SPACE;
  
  table['#'] = COMMENT;
  table[ File::EOF_CHAR ] = FILE_EOF_CAT;
  return table;
}();

edenInlineNodiscardCXPR u8_t categorize(char c) noexcept { return charCategoryTable[(sz_t) c]; }

struct Tokenizer {
  eden::vector<Token>& token_list;
  File file;
  std::string_view text;
  u32_t current_position{};
  bool has_errors{};

  explicit Tokenizer(eden::vector<Token>& token_list, File file)
  : token_list(token_list), file(file), text(file.get_text()) {}

  edenInlineNodiscardCXPR char peek() const noexcept { return text[current_position]; }
  edenInlineNodiscardCXPR std::pair<u8_t, u8_t> peek_two() const noexcept { return { text[current_position], text[current_position + 1]}; }
  edenInlineNodiscardCXPR char peek_ahead(i64_t i = 1) const noexcept { return text[current_position + i]; }
  
  edenInlineNodiscardCXPR char take() noexcept { return text[current_position++]; }
  edenInlineNodiscardCXPR char previous() const noexcept { return text[current_position - 1]; }
  edenInlineCXPR          void pop() noexcept { ++current_position; }
  edenInlineCXPR          void undo() noexcept { --current_position; }

  edenNoInlineCold void
  error_at_currentpos(std::string_view msg) {
    report_error(file, 1, current_position, std::string(msg)); has_errors = true;
  }

  constexpr void grabStringLiteral() noexcept {
    Token new_token{ TokenType::STRING_LITERAL, 0, current_position };
    while (true) {
      auto const c = peek();
      if( categorize(c) == EOF_OR_NEWLINE ) {
        error_at_currentpos("Expected ending \" in string literal.");
        if(c != File::EOF_CHAR) pop();
        return;
      }
      pop();

      if(c == '"') break;
      if(c == '\\') {
        new_token.type = TokenType::ESCAPED_STRING_LITERAL; 
        if( categorize(peek()) == EOF_OR_NEWLINE ) {
          error_at_currentpos("Incomplete character escape sequence.");
          if(c != File::EOF_CHAR) pop();
          return;
        }
        pop();
      }
    }

    new_token.length = current_position - new_token.position;
    token_list.emplace_back(new_token);
  }

  constexpr void grabCharLiteral() noexcept {
    Token new_token{ TokenType::CHAR_LITERAL, 1, current_position };
    if( categorize(peek()) == EOF_OR_NEWLINE ) {
      error_at_currentpos("Incomplete char literal.");
      if(peek() != File::EOF_CHAR) pop();
      return;
    }

    auto const c = take();
    if( c == '\\' ) {
      if( categorize(peek()) == EOF_OR_NEWLINE ) {
        error_at_currentpos("Incomplete char literal.");
        if(c != File::EOF_CHAR) pop();
        return;
      }

      ++new_token.length;
      pop();
    }

    if( peek() != '\'') {
      error_at_currentpos("Expected ending ' in char literal.");
      if( peek() != File::EOF_CHAR) pop();
      return;
    }

    pop();
    token_list.emplace_back(new_token);
  }

  constexpr void grabSymbol() noexcept {
    TokenType type;
    auto const pos = current_position;
    auto const pair = peek_two();

    // all this overengineering shaves like, 1% of the lexing time?
    // #worthit
    struct SymbolMapping {
      bool is_double : 1 = false;
      TokenType type : 7 = TokenType::INVALID_TOKEN;

      consteval SymbolMapping() = default;
      consteval SymbolMapping(TokenType t, bool is_double = false) : is_double(is_double), type(t) { }
    };

    static constexpr auto combine = [] (u8_t first, u8_t second) {
      return u16_t( ( u16_t(first) << 7 ) | u16_t(second) );
    };
    auto const c = combine(pair.first, pair.second);

    static constexpr auto symbol_map = [] consteval {
      using enum TokenType;
      static constexpr auto sz = 0b00111111'11111111;
      std::array<SymbolMapping, sz> first_to_second{};

      // to add a symbol, follow the pattern shown
      for(sz_t i{}; i<i8_max; ++i) {
        first_to_second[ combine('+', i) ].type = PLUS;
        first_to_second[ combine('-', i) ].type = MINUS;
        first_to_second[ combine('<', i) ].type = LESS;
        first_to_second[ combine('>', i) ].type = GTR;
        first_to_second[ combine('!', i) ].type = KEYWORD_NOT;
        first_to_second[ combine('=', i) ].type = ASSIGN;
        first_to_second[ combine('/', i) ].type = SLASH;
        first_to_second[ combine('*', i) ].type = STAR;
        first_to_second[ combine('%', i) ].type = MOD;
        first_to_second[ combine('(', i) ].type = LPAREN;
        first_to_second[ combine(')', i) ].type = RPAREN;
        first_to_second[ combine('{', i) ].type = LBRACE;
        first_to_second[ combine('}', i) ].type = RBRACE;
        first_to_second[ combine('[', i) ].type = LBRACKET;
        first_to_second[ combine(']', i) ].type = RBRACKET;
        first_to_second[ combine('@', i) ].type = ADDR;
        first_to_second[ combine('&', i) ].type = AMPERSAND;
        first_to_second[ combine(',', i) ].type = COMMA;
        first_to_second[ combine(':', i) ].type = COLON;
        first_to_second[ combine('$', i) ].type = DOLLAR;
        first_to_second[ combine(';', i) ].type = SEMI_COLON;
    
        // these three marked as invalid so custom logic can run
        first_to_second[ combine('\"', i) ].type = INVALID_TOKEN;
        first_to_second[ combine('\'', i) ].type = INVALID_TOKEN;
        first_to_second[ combine('.', i) ] .type = INVALID_TOKEN;
      }

      first_to_second[ combine('-', '-') ] = {MINUSMINUS, 1};
      first_to_second[ combine('-', '>') ] = {ARROW, 1};
      first_to_second[ combine('<', '=') ] = {LESSEQ, 1};
      first_to_second[ combine('>', '=') ] = {GTREQ, 1};
      first_to_second[ combine('!', '=') ] = {KEYWORD_NOT_EQUAL, 1};
      first_to_second[ combine('=', '=') ] = {KEYWORD_EQUALS, 1};
    
      return first_to_second;
    }();
    
    auto const mapping = symbol_map[ sz_t(c) ];
    type = mapping.type;
    current_position += 1 + mapping.is_double;
    
    if(type != TokenType::INVALID_TOKEN)
      return (void) token_list.emplace_back(type, u16_t(current_position - pos), pos);

    switch(pair.first) {
      case '\"': [[likely]] return grabStringLiteral();
      case '\'': return grabCharLiteral();

      case '.':
        type = TokenType::DOT;
        if ( not(categorize(text[pos - 1]) & IDENT_START) || not(categorize(pair.second) & IDENT_START))
          error_at_currentpos("Dot operator requires adjacent alphanumeric characters.");
        break;

      case File::EOF_CHAR: [[unlikely]] current_position = pos; return; // p sure this isn't even possible'
      default: [[unlikely]]
        error_at_currentpos("Unrecognized symbol\n.");
    }
    token_list.emplace_back(type, u16_t(current_position - pos), pos);
  }

  constexpr void grabNumber() noexcept {
    Token new_token{ TokenType::INTEGER_LITERAL, 0, current_position };
    do { pop(); } while( categorize(peek()) == NUMBER );

    if(peek() == '.') {
      new_token.type = TokenType::DOUBLE_LITERAL;
      do { pop(); } while( categorize(peek()) == NUMBER );
      if(peek() == '.') error_at_currentpos("Repeated decimal point in float literal.");
    }

    new_token.length = current_position - new_token.position;
    if(peek() == 'f') new_token.type = TokenType::FLOAT_LITERAL, pop();

    token_list.emplace_back(new_token);
  }

  constexpr void grabIdentOrKeyword() noexcept {
    auto const pos = current_position;
    do { pop(); } while( categorize(peek()) & IDENT_CONTINUE );

    auto const length = u16_t(current_position - pos);
    auto const word = text.substr(pos, length);
    auto const type = classifyWord(word);

    token_list.emplace_back(type, length, pos);
  }

  constexpr void skipWS() noexcept {
    while ( categorize(peek()) & SPACE ) pop();
  }

  constexpr void skipComments() noexcept {
    pop();

    if (peek() != '{') {
      while ( ! (categorize( peek() ) & EOF_OR_NEWLINE) )  pop(); // while not FILE_EOF and not NEWLINE
      return;
    }
    pop(); // '{'
    
    sz_t nested{1};
    do {
      auto const first = peek();
      auto const second = peek_ahead();
      
      switch(first) {
        case '#': if(second == '{') ++nested, pop(); break;
        case '}': if(second == '#') --nested, pop(); break;
      case File::EOF_CHAR: return;
        default: break;
      }
      pop();
    } while(nested != 0);
  }

};

}

bool Lexer::tokenizeFile(eden::vector<Token>& out_tokens, File file) noexcept {
  Tokenizer tokenizer{out_tokens, file};
  if (tokenizer.peek() == '.') {
    tokenizer.error_at_currentpos("File may not start with . for very esoteric reasons.");
    ++tokenizer.current_position;
  }

  loop:
  tokenizer.skipWS();
  auto const c = tokenizer.peek();
  switch(categorize(c)) {
  case FILE_EOF_CAT: break;
    
  case LETTER_OR_UNDER: tokenizer.grabIdentOrKeyword(); goto loop;
  case NUMBER: tokenizer.grabNumber(); goto loop;
    
  case COMMENT: tokenizer.skipComments(); goto loop;
  default: tokenizer.grabSymbol(); goto loop;
  }

  auto const invalid_token = Token(TokenType::INVALID_TOKEN, 1, out_tokens.back().position);
  for (sz_t i{}; i < INVALID_TOKEN_PADDING; ++i)
    out_tokens.push_back(invalid_token);
  
  return false;
}
