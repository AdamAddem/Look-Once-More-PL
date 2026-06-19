#pragma once
#include "ast.hpp"
#include "semantic_analysis/symbol_table.hpp"

namespace LOM::Lexer {
struct Token;
}

namespace LOM::Parser {

struct Function {
  std::string_view name;
  AST::SyntaxTree body;
  bool is_public;
};

struct TU {
  std::string file_text;
  Module* module;
  AST::SyntaxTree global_tree;
  std::vector<std::string_view> imports;
  std::vector<Function> functions;
};

void printTU(TU&);

using TokenIter = std::vector<Lexer::Token>::iterator;

void parseTokens(TU& tu, TokenIter begin, TokenIter end);

inline constexpr bool STRUCT_MEMBERS_START_PUBLIC = true;

} // namespace Parser