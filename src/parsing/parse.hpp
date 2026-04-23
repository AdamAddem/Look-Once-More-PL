#pragma once
#include "ast/ast.hpp"
#include "edenlib/owned.hpp"
#include "semantic_analysis/symbol_table.hpp"

namespace LOM::Lexer {
struct Token;
}

namespace LOM::Parser {

struct Function {
  eden::releasing_string::released_span name;
  AST::SyntaxTree body;
  u64_t line_number{};
};

struct TU {
  Module* table;
  AST::SyntaxTree global_tree;
  std::vector<Function> functions;
};

void printTU(TU&);

[[nodiscard]] TU
parseTokens(std::vector<Lexer::Token>& tokens, Module* table);
} // namespace Parser