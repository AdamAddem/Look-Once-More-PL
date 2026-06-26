#include "error.hpp"
#include "lex.hpp"
#include <functional>
#include <iostream>
#include <utility>

using namespace LOM::Lexer;


/*
void TokenView::expect_then_pop(TokenType expected_type, const char *err_msg) {
  begin->throw_if_not(expected_type, err_msg);
  pop();
}

void TokenView::expect(TokenType expected_type, const char *err_msg) const {
  begin->throw_if_not(expected_type, err_msg);
}

void TokenView::reject_then_pop(TokenType unwanted_type, const char *err_msg) {
  begin->throw_if(unwanted_type, err_msg);
  pop();
}

void TokenView::reject(TokenType unwanted_type, const char *err_msg) const {
  begin->throw_if(unwanted_type, err_msg);
}


TokenView TokenView::getTokensBetween(TokenType opening_token, TokenType closing_token) {
  const auto new_begin = begin;
  auto open = 1z;
  while (true) {
    if (empty()) return {end, end};

    if (begin->type == opening_token)       ++open;
    else if (begin->type == closing_token)  --open;

    if (open == 0) break;
    pop();
  }

  return {new_begin, begin++};
}

TokenView TokenView::getAllTokensUntilFirstOf(TokenType type) {
  auto const new_begin = begin;
  while (true) {
    if (empty()) return {end, end};

    if (begin->is(type)) break;
    pop();
  }

  return {new_begin, begin};
}
*/

void TokenView::print([[maybe_unused]] File const& file) const {}
