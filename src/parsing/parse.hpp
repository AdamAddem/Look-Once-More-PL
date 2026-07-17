#pragma once
#include "ast.hpp"
#include "semantic_analysis/symbol_table.hpp"
#include "file.hpp"

namespace LOM::Lexer {
struct Token;
}

namespace LOM::Parser {
struct TU;

struct Function {
  bool is_public;
  u8_t  file_idx;
  u32_t name_len;
  const char* name_ptr;

  std::vector<AST::ASTNode> body;

  [[nodiscard]] std::string_view
  nameof() const noexcept
  { return {name_ptr, name_len}; }

};

struct TU {
  std::vector<File> source_files;
  Module* module;
  std::vector<Function> functions;
  std::string_view name;
};

void printTU(TU const&) noexcept;

// Populates tu and returns whether an error was encountered.
[[nodiscard]] bool
parseTokens(TU& tu, std::vector<Lexer::Token>& tokens) noexcept;

} // namespace Parser