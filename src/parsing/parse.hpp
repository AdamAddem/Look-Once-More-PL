#pragma once
#include "edenlib/vectors/vector.hpp"

#include "ast.hpp"
#include "file.hpp"
#include "generic_tu.hpp"
#include "module/table_and_module.hpp"

namespace LOM::Lexer { struct Token; }

namespace LOM::Parser {

struct Function {
  GENERIC_FN_DEF

  eden::vector<AST::ASTNode> body;
  byte_t _pad[56]; // TODO: check whether this is being initialized

  InlineNodiscardCXPR std::string_view nameof() const noexcept { return {name_ptr, name_len}; }
}; static_assert(sizeof(Function) == FUNCTION_SIZE);

struct TU {
  GENERIC_TU_DEF
  eden::vector<Function> functions;
  InlineCXPR explicit TU(u16_t tu_id) : module(tu_id) {}

}; static_assert(sizeof(TU) == TU_SIZE); static_assert(alignof(TU) == TU_ALIGN);

void printTU(TU const&) noexcept;

// Populates tu and returns whether an error was encountered.
[[nodiscard]] bool
parseTokens(TU& out_tu, std::span<Lexer::Token> tokens) noexcept;

} // namespace Parser