#include "error.hpp"
#include "lexing/lex.hpp"
using namespace LOM;

LOMError::LOMError(const Stage err_stage,
           const std::string& what, const std::string& err_context,
           const unsigned line_num) :
  error_stage(err_stage) {

  using enum Stage;
  switch (error_stage) {
  case LexingError:
    error_message.append("Lexing ");
    break;

  case ParsingError:
    error_message.append("Parsing ");
    break;

  case ValidationError:
    error_message.append("Validation ");
    break;

  case BackendError:
    error_message.append("Backend ");
    break;
  }

  error_message.append("error on line ");
  error_message.append(std::to_string(line_num)  + ":\n");

  error_message.append(what);
  error_message.append("\n\n");

  error_message.append("Context:\n");
  error_message.append(err_context);
}

LexingError::LexingError(const std::string& what, const Lexer::Token& token) :
LOMError(Stage::LexingError, what, token.toString(), token.line_number) {}


ParsingError::ParsingError(const std::string& what, const Lexer::Token& token) :
LOMError(Stage::ParsingError, what, token.toString(), token.line_number) {}
