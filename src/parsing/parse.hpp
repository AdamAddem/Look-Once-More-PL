#pragma once
#include "ast/ast.hpp"
#include "edenlib/owned.hpp"
#include "semantic_analysis/symbol_table.hpp"

namespace LOM::Lexer {
struct Token;
}

namespace LOM::Parser {

struct Function {
  eden::owned_stringview name;
  AST::SyntaxTree body;
  u64_t line_number{};
};

struct TU {
  SymbolTable table;
  AST::SyntaxTree global_tree;
  std::vector<Function> functions;
};


[[nodiscard]] TU parseTokens(std::vector<Lexer::Token> &&tokens);
} // namespace Parser