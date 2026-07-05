#pragma once
#include <string_view>
#include <unordered_map>
#include <utility>
#include "edenlib/typedefs.hpp"

namespace LOM::Lexer {
enum class TokenType : u8_t {
	INVALID_TOKEN,
	IDENTIFIER,
	INTEGER_LITERAL,
	FLOAT_LITERAL,
	DOUBLE_LITERAL,
	CHAR_LITERAL,
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
	COLON,
	ADDR,
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
	KEYWORD_RAW,
	KEYWORD_UNIQUE,
	KEYWORD_VAGUE,
	KEYWORD_MUT,
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
inline const std::unordered_map<std::string_view, TokenType> stringToTokenType{
	{"INVALID_TOKEN", TokenType::INVALID_TOKEN}, {"IDENTIFIER", TokenType::IDENTIFIER}, {"INTEGER_LITERAL", TokenType::INTEGER_LITERAL}, 
	{"FLOAT_LITERAL", TokenType::FLOAT_LITERAL}, {"DOUBLE_LITERAL", TokenType::DOUBLE_LITERAL}, 
	{"CHAR_LITERAL", TokenType::CHAR_LITERAL}, {"BOOL_LITERAL", TokenType::BOOL_LITERAL}, 
	{"STRING_LITERAL", TokenType::STRING_LITERAL}, {"ESCAPED_STRING_LITERAL", TokenType::ESCAPED_STRING_LITERAL}, 
	{"+", TokenType::PLUS}, {"++", TokenType::PLUSPLUS}, 
	{"-", TokenType::MINUS}, {"--", TokenType::MINUSMINUS}, 
	{"/", TokenType::SLASH}, {"*", TokenType::STAR}, 
	{"%", TokenType::MOD}, {"=", TokenType::ASSIGN}, 
	{"(", TokenType::LPAREN}, {")", TokenType::RPAREN}, 
	{"{", TokenType::LBRACE}, {"}", TokenType::RBRACE}, 
	{"[", TokenType::LBRACKET}, {"]", TokenType::RBRACKET}, 
	{"<", TokenType::LESS}, {"->", TokenType::ARROW}, 
	{">", TokenType::GTR}, {"<=", TokenType::LESSEQ}, 
	{">=", TokenType::GTREQ}, {";", TokenType::SEMI_COLON}, 
	{":", TokenType::COLON}, {"@", TokenType::ADDR}, 
	{",", TokenType::COMMA}, {".", TokenType::DOT}, 
	{"and", TokenType::KEYWORD_AND}, {"or", TokenType::KEYWORD_OR}, 
	{"xor", TokenType::KEYWORD_XOR}, {"not", TokenType::KEYWORD_NOT}, 
	{"eq", TokenType::KEYWORD_EQUALS}, {"not_eq", TokenType::KEYWORD_NOT_EQUAL}, 
	{"bitand", TokenType::KEYWORD_BITAND}, {"bitor", TokenType::KEYWORD_BITOR}, 
	{"bitxor", TokenType::KEYWORD_BITXOR}, {"bitnot", TokenType::KEYWORD_BITNOT}, 
	{"i8", TokenType::KEYWORD_i8}, {"i16", TokenType::KEYWORD_i16}, 
	{"i32", TokenType::KEYWORD_i32}, {"i64", TokenType::KEYWORD_i64}, 
	{"u8", TokenType::KEYWORD_u8}, {"u16", TokenType::KEYWORD_u16}, 
	{"u32", TokenType::KEYWORD_u32}, {"u64", TokenType::KEYWORD_u64}, 
	{"f32", TokenType::KEYWORD_f32}, {"f64", TokenType::KEYWORD_f64}, 
	{"char", TokenType::KEYWORD_CHAR}, {"string", TokenType::KEYWORD_STRING}, 
	{"bool", TokenType::KEYWORD_BOOL}, {"", TokenType::KEYWORD_DEVOID}, 
	{"raw", TokenType::KEYWORD_RAW}, {"unique", TokenType::KEYWORD_UNIQUE}, 
	{"vague", TokenType::KEYWORD_VAGUE}, {"mut", TokenType::KEYWORD_MUT}, 
	{"if", TokenType::KEYWORD_IF}, {"else", TokenType::KEYWORD_ELSE}, 
	{"while", TokenType::KEYWORD_WHILE}, {"return", TokenType::KEYWORD_RETURN}, 
	{"cast", TokenType::KEYWORD_CAST}, {"global", TokenType::KEYWORD_GLOBAL}, 
	{"null", TokenType::KEYWORD_NULL}, {"junk", TokenType::KEYWORD_JUNK}, 
	{"default", TokenType::KEYWORD_DEFAULT}, {"fn", TokenType::KEYWORD_FN}, 
	{"struct", TokenType::KEYWORD_STRUCT}, {"pub", TokenType::KEYWORD_PUB}, 
	{"import", TokenType::KEYWORD_IMPORT}, {"__C", TokenType::DUNDER_CEXTERN}, 
	{"__va", TokenType::DUNDER_VA}, 
};

constexpr std::string_view TokenTypeToString(TokenType e) {
	static constexpr std::string_view toString[] = {
	"INVALID_TOKEN","IDENTIFIER","INTEGER_LITERAL",
	"FLOAT_LITERAL","DOUBLE_LITERAL",
	"CHAR_LITERAL","BOOL_LITERAL",
	"STRING_LITERAL","ESCAPED_STRING_LITERAL",
	"+","++",
	"-","--",
	"/","*",
	"%","=",
	"(",")",
	"{","}",
	"[","]",
	"<","->",
	">","<=",
	">=",";",
	":","@",
	",",".",
	"and","or",
	"xor","not",
	"eq","not_eq",
	"bitand","bitor",
	"bitxor","bitnot",
	"i8","i16",
	"i32","i64",
	"u8","u16",
	"u32","u64",
	"f32","f64",
	"char","string",
	"bool","",
	"raw","unique",
	"vague","mut",
	"if","else",
	"while","return",
	"cast","global",
	"null","junk",
	"default","fn",
	"struct","pub",
	"import","__C",
	"__va",
};
	return toString[std::to_underlying(e)];
}
constexpr bool isCategoryLITERALS(TokenType e) { return std::to_underlying(e) >= 2 && std::to_underlying(e) < 9; }
constexpr bool isCategoryNUMERIC_LITERALS(TokenType e) { return std::to_underlying(e) >= 2 && std::to_underlying(e) < 7; }
constexpr bool isCategorySYMBOLS(TokenType e) { return std::to_underlying(e) >= 9 && std::to_underlying(e) < 33; }
constexpr bool isCategoryKEYWORDS(TokenType e) { return std::to_underlying(e) >= 33 && std::to_underlying(e) < 74; }
constexpr bool isCategoryBITWISE(TokenType e) { return std::to_underlying(e) >= 33 && std::to_underlying(e) < 43; }
constexpr bool isCategoryPRIMITIVES(TokenType e) { return std::to_underlying(e) >= 43 && std::to_underlying(e) < 60; }
constexpr bool isCategoryPOINTERS(TokenType e) { return std::to_underlying(e) >= 57 && std::to_underlying(e) < 60; }
constexpr bool isCategoryTYPE_QUALIFIERS(TokenType e) { return std::to_underlying(e) >= 60 && std::to_underlying(e) < 61; }
constexpr bool isCategoryDUNDER(TokenType e) { return std::to_underlying(e) >= 74 && std::to_underlying(e) < 76; }

#define TOKENTYPE_LITERALS_CASES case INTEGER_LITERAL: case FLOAT_LITERAL: case DOUBLE_LITERAL: case CHAR_LITERAL: case BOOL_LITERAL: case STRING_LITERAL: case ESCAPED_STRING_LITERAL: 
#define TOKENTYPE_NUMERIC_LITERALS_CASES case INTEGER_LITERAL: case FLOAT_LITERAL: case DOUBLE_LITERAL: case CHAR_LITERAL: case BOOL_LITERAL: 
#define TOKENTYPE_SYMBOLS_CASES case PLUS: case PLUSPLUS: case MINUS: case MINUSMINUS: case SLASH: case STAR: case MOD: case ASSIGN: case LPAREN: case RPAREN: case LBRACE: case RBRACE: case LBRACKET: case RBRACKET: case LESS: case ARROW: case GTR: case LESSEQ: case GTREQ: case SEMI_COLON: case COLON: case ADDR: case COMMA: case DOT: 
#define TOKENTYPE_KEYWORDS_CASES case KEYWORD_AND: case KEYWORD_OR: case KEYWORD_XOR: case KEYWORD_NOT: case KEYWORD_EQUALS: case KEYWORD_NOT_EQUAL: case KEYWORD_BITAND: case KEYWORD_BITOR: case KEYWORD_BITXOR: case KEYWORD_BITNOT: case KEYWORD_i8: case KEYWORD_i16: case KEYWORD_i32: case KEYWORD_i64: case KEYWORD_u8: case KEYWORD_u16: case KEYWORD_u32: case KEYWORD_u64: case KEYWORD_f32: case KEYWORD_f64: case KEYWORD_CHAR: case KEYWORD_STRING: case KEYWORD_BOOL: case KEYWORD_DEVOID: case KEYWORD_RAW: case KEYWORD_UNIQUE: case KEYWORD_VAGUE: case KEYWORD_MUT: case KEYWORD_IF: case KEYWORD_ELSE: case KEYWORD_WHILE: case KEYWORD_RETURN: case KEYWORD_CAST: case KEYWORD_GLOBAL: case KEYWORD_NULL: case KEYWORD_JUNK: case KEYWORD_DEFAULT: case KEYWORD_FN: case KEYWORD_STRUCT: case KEYWORD_PUB: case KEYWORD_IMPORT: 
#define TOKENTYPE_BITWISE_CASES case KEYWORD_AND: case KEYWORD_OR: case KEYWORD_XOR: case KEYWORD_NOT: case KEYWORD_EQUALS: case KEYWORD_NOT_EQUAL: case KEYWORD_BITAND: case KEYWORD_BITOR: case KEYWORD_BITXOR: case KEYWORD_BITNOT: 
#define TOKENTYPE_PRIMITIVES_CASES case KEYWORD_i8: case KEYWORD_i16: case KEYWORD_i32: case KEYWORD_i64: case KEYWORD_u8: case KEYWORD_u16: case KEYWORD_u32: case KEYWORD_u64: case KEYWORD_f32: case KEYWORD_f64: case KEYWORD_CHAR: case KEYWORD_STRING: case KEYWORD_BOOL: case KEYWORD_DEVOID: case KEYWORD_RAW: case KEYWORD_UNIQUE: case KEYWORD_VAGUE: 
#define TOKENTYPE_POINTERS_CASES case KEYWORD_RAW: case KEYWORD_UNIQUE: case KEYWORD_VAGUE: 
#define TOKENTYPE_TYPE_QUALIFIERS_CASES case KEYWORD_MUT: 
#define TOKENTYPE_DUNDER_CASES case DUNDER_CEXTERN: case DUNDER_VA: 

}; //namespace LOM::Lexer