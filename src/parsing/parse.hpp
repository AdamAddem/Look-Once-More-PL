#pragma once
#include "ast.hpp"
#include "semantic_analysis/symbol_table.hpp"
#include "file.hpp"

namespace LOM::Lexer {
struct Token;
}

namespace LOM::Parser {

struct Function {
  std::string_view name;
  AST::SyntaxTree body;
};

struct TU {
  std::vector<File> source_files; // within parseTokens, the current source file should always be source_files.back();
  Module* module;
  AST::SyntaxTree global_tree;
  std::vector<std::string_view> imports;
  std::vector<Function> functions;
};

void printTU(TU&);

void parseTokens(TU& tu, std::vector<Lexer::Token>& tokens);

inline constexpr bool STRUCT_MEMBERS_START_PUBLIC = true;

} // namespace Parser