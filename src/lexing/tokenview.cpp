#include "lex.hpp"

using namespace LOM::Lexer;

#include <print>
void TokenView::print([[maybe_unused]] File const& file) const {

  auto curr = begin;
  while (curr not_eq end) {
    if (curr->isInvalid()) break;
    std::print("\n{}", curr->originalString(file));
    ++curr;
  }
}
