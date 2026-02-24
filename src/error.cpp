#include "error.hpp"

#include "grammar/types.hpp"
#include "lexing/lex.hpp"

LOMError::LOMError(const Stage err_stage,
           const std::string& what, const std::string& err_context,
           const unsigned line_num) :
  error_stage(err_stage),
  error_message("error occured on line ")
{
  error_message.append(std::to_string(line_num)  + ":\n");

  error_message.append(what);
  error_message.append("\n\n");

  error_message.append("Context:\n");
  error_message.append(err_context);
}

LexingError::LexingError(const std::string& what, const Lexer::Token& token) :
LOMError(Stage::LexingError, what, token.toString(), token.line_number) {}

ParsingError::ParsingError(const std::string& what, const Type& type, const unsigned line_num) :
LOMError(Stage::ParsingError, what, type.toString(), line_num)  {}


ParsingError::ParsingError(const std::string& what, const Lexer::Token& token) :
LOMError(Stage::ParsingError, what, token.toString(), token.line_number) {}
