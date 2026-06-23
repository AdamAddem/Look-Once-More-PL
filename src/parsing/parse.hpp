#pragma once
#include "ast.hpp"
#include "semantic_analysis/symbol_table.hpp"
#include "file.hpp"

namespace LOM::Lexer {
struct Token;
}

namespace LOM::Parser {

struct Function {
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
  std::vector<std::string_view> imports;
  std::vector<Function> functions;
};

void printTU(TU const&);

void parseTokens(TU& tu, std::vector<Lexer::Token> const& tokens);

inline constexpr bool STRUCT_MEMBERS_START_PUBLIC = true;

} // namespace Parser