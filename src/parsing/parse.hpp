#pragma once
#include "edenlib/vectors/vector.hpp"

#include "ast.hpp"
#include "file.hpp"
#include "module/table_and_module.hpp"

namespace LOM::Lexer {
struct Token;
}

namespace LOM::Parser {

struct Function {
  bool is_public;
  u8_t  file_idx;
  u16_t id_in_module;
  u32_t name_len;
  char const* name_ptr;

  eden::vector<AST::ASTNode> body;

  edenInlineNodiscardCXPR std::string_view nameof() const noexcept { return {name_ptr, name_len}; }
};

struct TU {
  eden::vector<File> source_files;
  eden::vector<Function> functions;
  std::string_view name;
  Module* module;
};

void printTU(TU const&) noexcept;

// Populates tu and returns whether an error was encountered.
[[nodiscard]] bool
parseTokens(TU& out_tu, eden::vector<Lexer::Token>& tokens) noexcept;

} // namespace Parser