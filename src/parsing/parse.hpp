#pragma once
#include "ast.hpp"
#include "semantic_analysis/symbol_table.hpp"

namespace LOM::Lexer {
class Token;
}

namespace LOM::Parser {

struct Function {
  std::string_view name;
  AST::SyntaxTree body;
  bool is_public;
};

struct TU {

  eden_nonull_args
  TU(Module* module, std::string_view name)
  : name(name), module(module) {}

  TU(TU&&) noexcept = default;

  std::string_view name;
  Module* eden_notnullptr module;
  AST::SyntaxTree global_tree;
  std::vector<std::string_view> imports;
  std::vector<Function> functions;
};

void printTU(TU&);

using TokenIter = std::vector<Lexer::Token>::iterator;

void parseTokens(TU& tu, TokenIter begin, TokenIter end);
} // namespace Parser