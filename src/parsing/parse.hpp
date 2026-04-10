#pragma once
#include "ast/ast.hpp"
#include "edenlib/owned.hpp"
#include "semantic_analysis/symbol_table.hpp"

namespace LOM::Lexer {
struct Token;
}

namespace LOM::Parser {

struct Function {
  eden::releasing_string::released_ptr name;
  AST::SyntaxTree function_body;
  u64_t line_number{};

  Function() = default;
  Function(Function &&other) noexcept
  : name(std::move(other.name)),
    function_body(std::move(other.function_body)),
    line_number(other.line_number){}

  ~Function() {
    eden::releasing_string::destroy_and_deallocate(std::move(name));
  }
};

struct TU {
  SymbolTable table;
  AST::SyntaxTree global_tree;
  std::vector<Function> functions;
};


[[nodiscard]] TU parseTokens(std::vector<Lexer::Token> &&tokens);
} // namespace Parser