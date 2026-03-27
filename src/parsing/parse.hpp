#pragma once
#include "ast/ast.hpp"
#include "semantic_analysis/symbol_table.hpp"

namespace LOM::Lexer { struct Token; }

namespace LOM::Parser {

struct ParsedFunction {
  std::string name;
  AST::SyntaxTree function_body;
  u64_t line_number{};

  ParsedFunction() = default;
  ParsedFunction(ParsedFunction &&other) noexcept
  : name(std::move(other.name)),
    function_body(std::move(other.function_body)),
    line_number(other.line_number){}
};

struct ParsedTU {
  SymbolTable table;
  AST::SyntaxTree global_tree;
  std::vector<ParsedFunction> functions;
};


[[nodiscard]] ParsedTU parseTokens(std::vector<Lexer::Token> &&tokens);
} // namespace Parser