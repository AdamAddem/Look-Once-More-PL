#include "error.hpp"
#include "lex.hpp"
#include <functional>
#include <iostream>
#include <stdexcept>
#include <utility>

using namespace Lexer;

void Token::throw_if(const TokenType unwanted_type, const char* err_msg, const LOMError::Stage error_stage) const {
  if (type == unwanted_type) {
   if (error_stage == LOMError::Stage::LexingError)
     throw LexingError(err_msg, *this);

   throw ParsingError(err_msg, *this);
  }
}

void Token::throw_if_not(const TokenType expected_type, const char* err_msg, const LOMError::Stage error_stage) const {
  if (type != expected_type) {
    if (error_stage == LOMError::Stage::LexingError)
      throw LexingError(err_msg, *this);

    throw ParsingError(err_msg, *this);
  }
}

std::string Token::toString() const {
  if (type == TokenType::IDENTIFIER)
    return std::get<std::string>(value);

  if (isLiteral()) {
    switch (type) {
    case TokenType::INT_LITERAL:
      return std::to_string(getInt());
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
      throw std::runtime_error("Error in isLiteral method");
    }
  }

  return tokenTypeToString(type);
}

std::string Token::toDebugString() const {

  if (type == TokenType::IDENTIFIER)
    return {"id::" + toString()};

  if (isLiteral()) {
    switch (type) {
    case TokenType::INT_LITERAL:
      return {toString() + 'i'};
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
      throw std::runtime_error("Error in toDebugString method");
    }
    throw std::runtime_error("Error in toDebugString method");
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
  while (curr_print != end) {
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
    }
    else if (type == TokenType::RBRACE) {
      std::cout << "\b} ";
      indent--;
    }
    else
      std::cout << curr_print->toDebugString() << " ";

    ++curr_print;
  }
}



void TokenView::reject_then_pop(const TokenType unwanted_type,
                                const char *err_msg,
                                const LOMError::Stage error_stage) {
  begin->throw_if(unwanted_type, err_msg, error_stage);
  ++begin;
}

void TokenView::expect_then_pop(const TokenType expected_type,
                                const char *err_msg,
                                const LOMError::Stage error_stage) {
  begin->throw_if_not(expected_type, err_msg, error_stage);
  ++begin;
}


TokenView TokenView::getTokensBetween(const TokenType opening_token,
                                      const TokenType closing_token) {
  const TokenIter new_begin = begin;
  int open = 1;
  while (open) {
    if (begin == end) {
      std::string errmsg = "Expected closing ";
      errmsg.append(tokenTypeToString(opening_token));
      throw LexingError(errmsg, *(begin - 1));
    }

    if (begin->type == opening_token)
      ++open;
    else if (begin->type == closing_token)
      --open;

    ++begin;
  }


  return {new_begin, begin - 1};
}


TokenView TokenView::getAllTokensUntilFirstOf(const TokenType _type) {
  const TokenIter new_begin = begin;
  while (!begin->is(_type)) {
    ++begin;
    if (begin == end) {
      std::string errmsg = "Expected ";
      errmsg.append(tokenTypeToString(_type));
      throw LexingError(errmsg, *(begin - 1));
    }

  }

  return {new_begin, begin};
}