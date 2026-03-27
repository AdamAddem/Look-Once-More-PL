#include "error.hpp"
#include "lex.hpp"
#include <functional>
#include <iostream>
#include <stdexcept>
#include <utility>

using namespace LOM::Lexer;

void Token::throw_if(const TokenType unwanted_type, const char* err_msg) const {
  if (type eq unwanted_type)
   throw ParsingError(err_msg, *this);
}

void Token::throw_if_not(const TokenType expected_type, const char* err_msg) const {
  if (type not_eq expected_type)
    throw ParsingError(err_msg, *this);
}

std::string Token::toString() const {
  if (type eq TokenType::IDENTIFIER)
    return std::get<std::string>(value);

  if (isLiteral()) {
    switch (type) {
    case TokenType::INT_LITERAL:
      return std::to_string(getInt());
    case TokenType::UINT_LITERAL:
      return std::to_string(getUint());
    case TokenType::FLOAT_LITERAL:
      return std::to_string(getFloat());
    case TokenType::DOUBLE_LITERAL:
      return std::to_string(getDouble());
    case TokenType::CHAR_LITERAL:
      return std::to_string(getChar());
    case TokenType::STRING_LITERAL:
      return std::get<std::string>(value);
    case TokenType::BOOL_LITERAL:
      return getBool() ? "true" : "false";

    default:
      std::unreachable();
    }
  }

  return tokenTypeToString(type);
}

std::string Token::toDebugString() const {

  if (type eq TokenType::IDENTIFIER)
    return {"id::" + toString()};

  if (isLiteral()) {
    switch (type) {
    case TokenType::INT_LITERAL:
      return {toString() + 'i'};
    case TokenType::UINT_LITERAL:
      return {toString() + 'u'};
    case TokenType::FLOAT_LITERAL:
      return {toString() + 'f'};
    case TokenType::DOUBLE_LITERAL:
      return {toString() + 'd'};
    case TokenType::CHAR_LITERAL:
      return {'\'' + toString() + '\''};
    case TokenType::STRING_LITERAL:
      return {'\"' + toString() + '\"'};

    case TokenType::BOOL_LITERAL:
      return getBool() ? "true_b" : "false_b";

    default:
      std::unreachable();
    }
  }

  if (type eq TokenType::KEYWORD_DEVOID) [[unlikely]]
    return {"devoid"};

  return tokenTypeToString(type);
}

/* Token Methods */

/* TokenHandler Methods */

void
TokenView::print(const unsigned initial_indent) const {
  auto curr_print = begin;

  auto indent{initial_indent};
  unsigned last_linenum{0};
  while (curr_print not_eq end) {
    const auto type = curr_print->type;
    const unsigned ln = curr_print->line_number;
    while (ln gtr last_linenum) {
      std::cout << std::endl;
      ++last_linenum;
      std::cout << last_linenum << ": " << std::string(indent, ' ');
    }

    if (type eq TokenType::LBRACE) {
      std::cout << "{ ";
      indent++;
    }
    else if (type eq TokenType::RBRACE) {
      std::cout << "\b} ";
      indent--;
    }
    else
      std::cout << curr_print->toDebugString() << " ";

    ++curr_print;
  }
}

void
TokenView::expect_then_pop(const TokenType expected_type, const char *err_msg) {
  begin->throw_if_not(expected_type, err_msg);
  pop();
}

void
TokenView::expect(const TokenType expected_type, const char *err_msg) const { begin->throw_if_not(expected_type, err_msg); }

void
TokenView::reject_then_pop(const TokenType unwanted_type, const char *err_msg) {
  begin->throw_if(unwanted_type, err_msg);
  pop();
}

void
TokenView::reject(const TokenType unwanted_type, const char *err_msg) const { begin->throw_if(unwanted_type, err_msg); }

TokenView
TokenView::getTokensBetween(const TokenType opening_token, const TokenType closing_token) {
  const auto new_begin = begin;
  int open = 1;
  while (open) {
    if (empty()) {
      std::string errmsg = "Expected closing ";
      errmsg.append(tokenTypeToString(opening_token));
      throw ParsingError(errmsg, *(begin - 1));
    }

    if (begin->type eq opening_token)
      ++open;
    else if (begin->type eq closing_token)
      --open;

    pop();
  }

  return {new_begin, begin - 1};
}


TokenView
TokenView::getAllTokensUntilFirstOf(const TokenType type) {
  const TokenIter new_begin = begin;
  while (not begin->is(type)) {
    pop();
    if (empty()) {
      std::string errmsg = "Expected ";
      errmsg.append(tokenTypeToString(type));
      throw ParsingError(errmsg, *(begin - 1));
    }

  }

  return {new_begin, begin};
}

u64_t
TokenView::distanceFromFirstOf(const TokenType type) const {
  auto curr = begin;
  while (not curr->is(type)) {
    ++curr;
    if (curr eq end) {
      std::string errmsg = "Expected ";
      errmsg.append(tokenTypeToString(type));
      throw ParsingError(errmsg, *(curr - 1));
    }
  }

  return curr - begin;
}
