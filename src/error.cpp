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

  error_message.append(std::format("error on line {}:\n{}\n\nContext:\n{}", line_num, what, err_context));
}

LexingError::LexingError(const std::string& what, const Lexer::Token& token) :
LOMError(Stage::LexingError, what, token.toString(), token.getLN()) {}


ParsingError::ParsingError(const std::string& what, const Lexer::Token& token) :
LOMError(Stage::ParsingError, what, token.toString(), token.getLN()) {}
