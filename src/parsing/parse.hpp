#pragma once
#include "semantic_analysis/symbol_table.hpp"
#include "ast/ast.hpp"

namespace LOM::Lexer {
  struct Token;
}


namespace LOM::Parser {

struct ParsedFunction {
  std::string name;
  AST::SyntaxTree function_body;

  ParsedFunction() = default;
  ParsedFunction(ParsedFunction &&other) noexcept
  : name(std::move(other.name)),
    function_body(std::move(other.function_body)) {}
};

struct ParsedTU {
  SymbolTable table;
  AST::SyntaxTree global_tree;
  std::vector<ParsedFunction> functions;
};


[[nodiscard]] ParsedTU parseTokens(std::vector<Lexer::Token> &&tokens);
} // namespace Parser