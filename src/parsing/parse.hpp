#pragma once
#include "ast.hpp"
#include "edenlib/vectors/vector.hpp"
#include "file.hpp"
#include "module/table_and_module.hpp"

namespace LOM::Lexer {
struct Token;
}

namespace LOM::Parser {
struct TU;

struct Function {
  //char _pad[2];
  bool is_public;
  u8_t  file_idx;
  u32_t name_len;
  const char* name_ptr;

  eden::vector<AST::ASTNode> body;

  edenInlineNodiscardCXPR std::string_view
  nameof() const noexcept
  { return {name_ptr, name_len}; }

};

struct TU {
  eden::vector<File> source_files;
  eden::vector<Function> functions;
  std::string_view name;
  // char _pad[4];
  u32_t module_id;
};

void printTU(TU const&) noexcept;

// Populates tu and returns whether an error was encountered.
[[nodiscard]] bool
parseTokens(TU& out_tu, eden::vector<Lexer::Token>& tokens) noexcept;

} // namespace Parser