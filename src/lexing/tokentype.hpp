#pragma once
#include "edenlib/typedefs.hpp"
#include <utility>

namespace LOM::Lexer {
enum class TokenType : u8_t {
	INVALID_TOKEN,
	IDENTIFIER,
	INTEGER_LITERAL,
	FLOAT_LITERAL,
	DOUBLE_LITERAL,
	CHAR_LITERAL,
	ESCAPED_CHAR_LITERAL,
	BOOL_LITERAL,
	STRING_LITERAL,
	ESCAPED_STRING_LITERAL,
	PLUS,
	PLUSPLUS,
	MINUS,
	MINUSMINUS,
	SLASH,
	STAR,
	MOD,
	ASSIGN,
	LPAREN,
	RPAREN,
	LBRACE,
	RBRACE,
	LBRACKET,
	RBRACKET,
	LESS,
	ARROW,
	GTR,
	LESSEQ,
	GTREQ,
	SEMI_COLON,
	DOUBLE_COLON,
	COLON,
	DOLLAR,
	ADDR,
	AMPERSAND,
	COMMA,
	DOT,
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

	KEYWORD_u8,
	KEYWORD_u16,
	KEYWORD_u32,
	KEYWORD_u64,
	KEYWORD_i8,
	KEYWORD_i16,
	KEYWORD_i32,
	KEYWORD_i64,
	KEYWORD_f32,
	KEYWORD_f64,
	KEYWORD_CHAR,
	KEYWORD_STRING,
	KEYWORD_BOOL,

	KEYWORD_DEVOID,
	KEYWORD_RAW,
	KEYWORD_REF,
	KEYWORD_IF,
	KEYWORD_ELSE,
	KEYWORD_WHILE,
	KEYWORD_RETURN,
	KEYWORD_CAST,
	KEYWORD_GLOBAL,
	KEYWORD_NULL,
	KEYWORD_JUNK,
	KEYWORD_DEFAULT,
	KEYWORD_FN,
	KEYWORD_STRUCT,
	KEYWORD_PUB,
	KEYWORD_IMPORT,
	DUNDER_CEXTERN,
	DUNDER_VA,
};
constexpr bool isCategoryLITERALS(TokenType e) { return std::to_underlying(e) >= 2 && std::to_underlying(e) < 10; }
constexpr bool isCategoryNUMERIC_LITERALS(TokenType e) { return std::to_underlying(e) >= 2 && std::to_underlying(e) < 8; }
constexpr bool isCategorySYMBOLS(TokenType e) { return std::to_underlying(e) >= 10 && std::to_underlying(e) < 37; }
constexpr bool isCategoryVAR_QUALIFIERS(TokenType e) { return std::to_underlying(e) >= 31 && std::to_underlying(e) < 33; }
constexpr bool isCategoryKEYWORDS(TokenType e) { return std::to_underlying(e) >= 37 && std::to_underlying(e) < 76; }
constexpr bool isCategoryLOGICAL(TokenType e) { return std::to_underlying(e) >= 37 && std::to_underlying(e) < 43; }
constexpr bool isCategoryBITWISE(TokenType e) { return std::to_underlying(e) >= 43 && std::to_underlying(e) < 47; }
constexpr bool isCategoryPRIMITIVES(TokenType e) { return std::to_underlying(e) >= 47 && std::to_underlying(e) < 60; }
constexpr bool isCategoryPOINTERS(TokenType e) { return std::to_underlying(e) >= 61 && std::to_underlying(e) < 63; }
constexpr bool isCategoryDUNDER(TokenType e) { return std::to_underlying(e) >= 76 && std::to_underlying(e) < 78; }

#define TOKENTYPE_LITERALS_CASES case INTEGER_LITERAL: case FLOAT_LITERAL: case DOUBLE_LITERAL: case CHAR_LITERAL: case ESCAPED_CHAR_LITERAL: case BOOL_LITERAL: case STRING_LITERAL: case ESCAPED_STRING_LITERAL: 
#define TOKENTYPE_NUMERIC_LITERALS_CASES case INTEGER_LITERAL: case FLOAT_LITERAL: case DOUBLE_LITERAL: case CHAR_LITERAL: case ESCAPED_CHAR_LITERAL: case BOOL_LITERAL: 
#define TOKENTYPE_SYMBOLS_CASES case PLUS: case PLUSPLUS: case MINUS: case MINUSMINUS: case SLASH: case STAR: case MOD: case ASSIGN: case LPAREN: case RPAREN: case LBRACE: case RBRACE: case LBRACKET: case RBRACKET: case LESS: case ARROW: case GTR: case LESSEQ: case GTREQ: case SEMI_COLON: case DOUBLE_COLON: case COLON: case DOLLAR: case ADDR: case AMPERSAND: case COMMA: case DOT: 
#define TOKENTYPE_VAR_QUALIFIERS_CASES case COLON: case DOLLAR: 
#define TOKENTYPE_KEYWORDS_CASES case KEYWORD_AND: case KEYWORD_OR: case KEYWORD_XOR: case KEYWORD_NOT: case KEYWORD_EQUALS: case KEYWORD_NOT_EQUAL: case KEYWORD_BITAND: case KEYWORD_BITOR: case KEYWORD_BITXOR: case KEYWORD_BITNOT: case KEYWORD_u8: case KEYWORD_u16: case KEYWORD_u32: case KEYWORD_u64: case KEYWORD_i8: case KEYWORD_i16: case KEYWORD_i32: case KEYWORD_i64: case KEYWORD_f32: case KEYWORD_f64: case KEYWORD_CHAR: case KEYWORD_STRING: case KEYWORD_BOOL: case KEYWORD_DEVOID: case KEYWORD_RAW: case KEYWORD_REF: case KEYWORD_IF: case KEYWORD_ELSE: case KEYWORD_WHILE: case KEYWORD_RETURN: case KEYWORD_CAST: case KEYWORD_GLOBAL: case KEYWORD_NULL: case KEYWORD_JUNK: case KEYWORD_DEFAULT: case KEYWORD_FN: case KEYWORD_STRUCT: case KEYWORD_PUB: case KEYWORD_IMPORT: 
#define TOKENTYPE_LOGICAL_CASES case KEYWORD_AND: case KEYWORD_OR: case KEYWORD_XOR: case KEYWORD_NOT: case KEYWORD_EQUALS: case KEYWORD_NOT_EQUAL: 
#define TOKENTYPE_BITWISE_CASES case KEYWORD_BITAND: case KEYWORD_BITOR: case KEYWORD_BITXOR: case KEYWORD_BITNOT: 
#define TOKENTYPE_PRIMITIVES_CASES case KEYWORD_u8: case KEYWORD_u16: case KEYWORD_u32: case KEYWORD_u64: case KEYWORD_i8: case KEYWORD_i16: case KEYWORD_i32: case KEYWORD_i64: case KEYWORD_f32: case KEYWORD_f64: case KEYWORD_CHAR: case KEYWORD_STRING: case KEYWORD_BOOL: 
#define TOKENTYPE_POINTERS_CASES case KEYWORD_RAW: case KEYWORD_REF: 
#define TOKENTYPE_DUNDER_CASES case DUNDER_CEXTERN: case DUNDER_VA: 

}; //namespace LOM::Lexer