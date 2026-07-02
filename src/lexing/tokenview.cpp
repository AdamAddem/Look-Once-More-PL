#include "error.hpp"
#include "lex.hpp"
#include <functional>
#include <iostream>
#include <utility>

using namespace LOM::Lexer;

#include <print>
void TokenView::print([[maybe_unused]] File const& file) const {

  auto curr = begin;
  while (curr not_eq end) {
    if (curr->type == TokenType::INVALID_TOKEN) break;
    std::print("\n{}", curr->originalString(file));
    ++curr;
  }
}
