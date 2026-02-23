#pragma once
#include <unordered_map>
#include <string>
#include <utility>

namespace Lexer {
enum class TokenType : unsigned {
  INVALID_TOKEN,
  IDENTIFIER,
  INT_LITERAL,
  FLOAT_LITERAL,
  DOUBLE_LITERAL,
  STRING_LITERAL,
  CHAR_LITERAL,
  BOOL_LITERAL,
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
  ARROW,
  GTR,
  LESSEQ,
  GTREQ,
  SEMI_COLON,
  ADDR,
  COMMA,
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
  KEYWORD_UNIQUE,
  KEYWORD_SHARING,
  KEYWORD_WATCHING,
  KEYWORD_RAW,
  KEYWORD_VAGUE,
  KEYWORD_MUT,
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
  KEYWORD_CAST,
  KEYWORD_CAST_IF,
  KEYWORD_UNSAFE_CAST,
  KEYWORD_STEAL,
  KEYWORD_BUILD_NEW,
  KEYWORD_ALLOCATE,
  KEYWORD_CONSTRUCT,
  KEYWORD_FROM,
  KEYWORD_AS,
  KEYWORD_GLOBAL,
  KEYWORD_NULL,
  KEYWORD_JUNK,
  KEYWORD_FN,
};


inline const std::unordered_map<std::string, TokenType> stringToTokenType{

	{"INVALID_TOKEN", TokenType::INVALID_TOKEN}, {"IDENTIFIER", TokenType::IDENTIFIER}, {"INT_LITERAL", TokenType::INT_LITERAL}, 
	{"FLOAT_LITERAL", TokenType::FLOAT_LITERAL}, {"DOUBLE_LITERAL", TokenType::DOUBLE_LITERAL}, {"STRING_LITERAL", TokenType::STRING_LITERAL}, 
	{"CHAR_LITERAL", TokenType::CHAR_LITERAL}, {"BOOL_LITERAL", TokenType::BOOL_LITERAL}, {"+", TokenType::PLUS}, 
	{"++", TokenType::PLUSPLUS}, {"-", TokenType::MINUS}, {"--", TokenType::MINUSMINUS}, 
	{"/", TokenType::SLASH}, {"*", TokenType::STAR}, {"^", TokenType::POW}, 
	{"%", TokenType::MOD}, {"=", TokenType::ASSIGN}, {"(", TokenType::LPAREN}, 
	{")", TokenType::RPAREN}, {"{", TokenType::LBRACE}, {"}", TokenType::RBRACE}, 
	{"[", TokenType::LBRACKET}, {"]", TokenType::RBRACKET}, {"<", TokenType::LESS}, 
	{"->", TokenType::ARROW}, {">", TokenType::GTR}, {"<=", TokenType::LESSEQ}, 
	{">=", TokenType::GTREQ}, {";", TokenType::SEMI_COLON}, {"@", TokenType::ADDR}, 
	{",", TokenType::COMMA}, {"and", TokenType::KEYWORD_AND}, {"or", TokenType::KEYWORD_OR}, 
	{"xor", TokenType::KEYWORD_XOR}, {"not", TokenType::KEYWORD_NOT}, {"eq", TokenType::KEYWORD_EQUALS}, 
	{"not_eq", TokenType::KEYWORD_NOT_EQUAL}, {"bitand", TokenType::KEYWORD_BITAND}, {"bitor", TokenType::KEYWORD_BITOR}, 
	{"bitxor", TokenType::KEYWORD_BITXOR}, {"bitnot", TokenType::KEYWORD_BITNOT}, {"i8", TokenType::KEYWORD_i8}, 
	{"i16", TokenType::KEYWORD_i16}, {"i32", TokenType::KEYWORD_i32}, {"i64", TokenType::KEYWORD_i64}, 
	{"u8", TokenType::KEYWORD_u8}, {"u16", TokenType::KEYWORD_u16}, {"u32", TokenType::KEYWORD_u32}, 
	{"u64", TokenType::KEYWORD_u64}, {"f32", TokenType::KEYWORD_f32}, {"f64", TokenType::KEYWORD_f64}, 
	{"char", TokenType::KEYWORD_CHAR}, {"string", TokenType::KEYWORD_STRING}, {"bool", TokenType::KEYWORD_BOOL}, 
	{"CHANGE_ME_TO_EMPTY_NIGGA", TokenType::KEYWORD_DEVOID}, {"unique", TokenType::KEYWORD_UNIQUE}, {"sharing", TokenType::KEYWORD_SHARING}, 
	{"watching", TokenType::KEYWORD_WATCHING}, {"raw", TokenType::KEYWORD_RAW}, {"vague", TokenType::KEYWORD_VAGUE}, 
	{"mut", TokenType::KEYWORD_MUT}, {"if", TokenType::KEYWORD_IF}, {"else", TokenType::KEYWORD_ELSE}, 
	{"for", TokenType::KEYWORD_FOR}, {"while", TokenType::KEYWORD_WHILE}, {"do", TokenType::KEYWORD_DO}, 
	{"return", TokenType::KEYWORD_RETURN}, {"switch", TokenType::KEYWORD_SWITCH}, {"case", TokenType::KEYWORD_CASE}, 
	{"default", TokenType::KEYWORD_DEFAULT}, {"goto", TokenType::KEYWORD_GOTO}, {"break", TokenType::KEYWORD_BREAK}, 
	{"continue", TokenType::KEYWORD_CONTINUE}, {"cast", TokenType::KEYWORD_CAST}, {"cast_if", TokenType::KEYWORD_CAST_IF}, 
	{"unsafe_cast", TokenType::KEYWORD_UNSAFE_CAST}, {"steal", TokenType::KEYWORD_STEAL}, {"build_new", TokenType::KEYWORD_BUILD_NEW}, 
	{"allocate", TokenType::KEYWORD_ALLOCATE}, {"construct", TokenType::KEYWORD_CONSTRUCT}, {"from", TokenType::KEYWORD_FROM}, 
	{"as", TokenType::KEYWORD_AS}, {"global", TokenType::KEYWORD_GLOBAL}, {"null", TokenType::KEYWORD_NULL}, 
	{"junk", TokenType::KEYWORD_JUNK}, {"fn", TokenType::KEYWORD_FN}, 
};


constexpr const char* tokenTypeToString(const TokenType e) {
constexpr const char* toString[] = {

	"INVALID_TOKEN","IDENTIFIER","INT_LITERAL",
	"FLOAT_LITERAL","DOUBLE_LITERAL","STRING_LITERAL",
	"CHAR_LITERAL","BOOL_LITERAL","+",
	"++","-","--",
	"/","*","^",
	"%","=","(",
	")","{","}",
	"[","]","<",
	"->",">","<=",
	">=",";","@",
	",","and","or",
	"xor","not","eq",
	"not_eq","bitand","bitor",
	"bitxor","bitnot","i8",
	"i16","i32","i64",
	"u8","u16","u32",
	"u64","f32","f64",
	"char","string","bool",
	"CHANGE_ME_TO_EMPTY_NIGGA","unique","sharing",
	"watching","raw","vague",
	"mut","if","else",
	"for","while","do",
	"return","switch","case",
	"default","goto","break",
	"continue","cast","cast_if",
	"unsafe_cast","steal","build_new",
	"allocate","construct","from",
	"as","global","null",
	"junk","fn",
};
	return toString[std::to_underlying(e)];
}
constexpr bool isCategoryLITERALS(const TokenType e) { return std::to_underlying(e) >= 2 && std::to_underlying(e) < 8; }

constexpr bool isCategorySYMBOLS(const TokenType e) { return std::to_underlying(e) >= 8 && std::to_underlying(e) < 31; }

constexpr bool isCategoryCOMP_BITWISE(const TokenType e) { return std::to_underlying(e) >= 31 && std::to_underlying(e) < 41; }

constexpr bool isCategoryPRIMITIVES(const TokenType e) { return std::to_underlying(e) >= 41 && std::to_underlying(e) < 55; }

constexpr bool isCategoryPOINTERS(const TokenType e) { return std::to_underlying(e) >= 55 && std::to_underlying(e) < 60; }

constexpr bool isCategoryTYPE_MODIFIERS(const TokenType e) { return std::to_underlying(e) >= 60 && std::to_underlying(e) < 61; }

constexpr bool isCategoryCONTROL_FLOW(const TokenType e) { return std::to_underlying(e) >= 61 && std::to_underlying(e) < 73; }

constexpr bool isCategoryCAST(const TokenType e) { return std::to_underlying(e) >= 73 && std::to_underlying(e) < 76; }

constexpr bool isCategoryALLOC_LIFETIMES(const TokenType e) { return std::to_underlying(e) >= 76 && std::to_underlying(e) < 80; }

constexpr bool isCategoryKEYWORDS(const TokenType e) { return std::to_underlying(e) >= 31 && std::to_underlying(e) < 86; }

}; //namespace Lexer