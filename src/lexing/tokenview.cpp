#include "error.hpp"
#include "lex.hpp"
#include <functional>
#include <iostream>
#include <utility>

using namespace LOM::Lexer;

eden_nonull_args eden_cstr_arg(2)
void Token::throw_if(const TokenType unwanted_type, const char *err_msg) const {
  if (type == unwanted_type)
    throw ParsingError(err_msg, *this);
}

eden_nonull_args eden_cstr_arg(2)
void Token::throw_if_not(const TokenType expected_type, const char *err_msg) const {
  if (type not_eq expected_type)
    throw ParsingError(err_msg, *this);
}

std::string Token::toString() const {
  if (type == TokenType::IDENTIFIER)
    return to_stdstring();

  if (isLiteral()) {
    switch (type) {
    case TokenType::INTEGER_LITERAL:
      return std::to_string(getRawValue());
    case TokenType::SIGNED_LITERAL:
      return std::to_string(getSigned());
    case TokenType::UNSIGNED_LITERAL:
      return std::to_string(getUnsigned());
    case TokenType::FLOAT_LITERAL:
      return std::to_string(getFloat());
    case TokenType::DOUBLE_LITERAL:
      return std::to_string(getDouble());
    case TokenType::CHAR_LITERAL:
      return std::to_string(getChar());
    case TokenType::STRING_LITERAL:
      return to_stdstring();
    case TokenType::BOOL_LITERAL:
      return getBool() ? "true" : "false";

    default:
      std::unreachable();
    }
  }

  return tokenTypeToString(type);
}

std::string Token::toDebugString() const {

  if (type == TokenType::IDENTIFIER)
    return {"id::" + toString()};

  if (isLiteral()) {
    switch (type) {
    case TokenType::INTEGER_LITERAL:
      return toString();
    case TokenType::SIGNED_LITERAL:
      return {toString() + 'i'};
    case TokenType::UNSIGNED_LITERAL:
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

  if (type == TokenType::KEYWORD_DEVOID) [[unlikely]]
    return {"devoid"};

  return tokenTypeToString(type);
}

/* Token Methods */

/* TokenHandler Methods */

void TokenView::print(const unsigned initial_indent) const {
  auto curr_print = begin;

  auto indent{initial_indent};
  unsigned last_linenum{0};
  while (curr_print not_eq end) {
    const auto type = curr_print->type;
    const unsigned ln = curr_print->line_number;
    while (ln > last_linenum) {
      std::cout << std::endl;
      ++last_linenum;
      std::cout << last_linenum << ": " << std::string(indent, ' ');
    }

    if (type == TokenType::LBRACE) {
      std::cout << "{ ";
      indent++;
    } else if (type == TokenType::RBRACE) {
      std::cout << "\b} ";
      indent--;
    } else
      std::cout << curr_print->toDebugString() << " ";

    ++curr_print;
  }
}

eden_nonull_args eden_cstr_arg(2)
void TokenView::expect_then_pop(const TokenType expected_type, const char *err_msg) {
  begin->throw_if_not(expected_type, err_msg);
  pop();
}

eden_nonull_args eden_cstr_arg(2)
void TokenView::expect(const TokenType expected_type, const char *err_msg) const {
  begin->throw_if_not(expected_type, err_msg);
}

eden_nonull_args eden_cstr_arg(2)
void TokenView::reject_then_pop(const TokenType unwanted_type, const char *err_msg) {
  begin->throw_if(unwanted_type, err_msg);
  pop();
}

eden_nonull_args eden_cstr_arg(2)
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

u64_t TokenView::distanceFromFirstOf(const TokenType type) const {
  auto curr = begin;
  while (not curr->is(type)) {
    ++curr;
    if (curr == end) {
      std::string errmsg = "Expected ";
      errmsg.append(tokenTypeToString(type));
      throw ParsingError(errmsg, *(curr - 1));
    }
  }

  return curr - begin;
}
