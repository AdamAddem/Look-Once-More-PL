#include "error.hpp"
#include "lex.hpp"
#include <functional>
#include <iostream>
#include <utility>

using namespace LOM::Lexer;

eden_nonull_args
void Token::throw_if(const TokenType unwanted_type, const char *err_msg) const {
  if (type == unwanted_type)
    throw ParsingError(err_msg, );
}

eden_nonull_args
void Token::throw_if_not(const TokenType expected_type, const char *err_msg) const {
  if (type not_eq expected_type)
    throw ParsingError(err_msg, *this);
}

/* Token Methods */

/* TokenView Methods */

eden_nonull_args
void TokenView::expect_then_pop(const TokenType expected_type, const char *err_msg) {
  begin->throw_if_not(expected_type, err_msg);
  pop();
}

eden_nonull_args
void TokenView::expect(const TokenType expected_type, const char *err_msg) const {
  begin->throw_if_not(expected_type, err_msg);
}

eden_nonull_args
void TokenView::reject_then_pop(const TokenType unwanted_type, const char *err_msg) {
  begin->throw_if(unwanted_type, err_msg);
  pop();
}

eden_nonull_args
void TokenView::reject(const TokenType unwanted_type, const char *err_msg) const {
  begin->throw_if(unwanted_type, err_msg);
}

TokenView TokenView::getTokensBetween(const TokenType opening_token, const TokenType closing_token) {
  const auto new_begin = begin;
  int open = 1;
  while (true) {
    if (empty()) {
      std::string errmsg = "Expected closing ";
      errmsg.append(tokenTypeToString(opening_token));
      throw ParsingError(errmsg, *(begin - 1));
    }

    if (begin->type == opening_token)
      ++open;
    else if (begin->type == closing_token)
      --open;

    if (open == 0)
      break;
    pop();
  }

  return {new_begin, begin++};
}

TokenView TokenView::getAllTokensUntilFirstOf(const TokenType type) {
  const TokenIter new_begin = begin;
  while (true) {
    if (empty()) {
      std::string errmsg = "Expected ";
      errmsg.append(tokenTypeToString(type));
      throw ParsingError(errmsg, *(begin - 1));
    }
    if (begin->is(type))
      break;
    pop();
  }

  return {new_begin, begin};
}