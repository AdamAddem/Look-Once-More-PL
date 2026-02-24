#pragma once
#include <string>

struct LOMError {
  enum class Stage {LexingError, ParsingError, ValidationError} error_stage;

  LOMError(Stage err_stage,
           const std::string& what, const std::string& err_context,
           unsigned line_num);

  LOMError(const LOMError& other) noexcept = default;
  LOMError(LOMError&& other) noexcept : error_stage(other.error_stage), error_message(std::move(other.error_message)) {}

  std::string error_message;
};


namespace Lexer {
  struct Token;
}

struct LexingError final : LOMError {
  LexingError(const std::string& what, const std::string& err_context, const unsigned line_num) :
  LOMError(Stage::LexingError, what, err_context, line_num) {}

  LexingError(const std::string& what, const Lexer::Token& token);
};

struct Type;
struct ParsingError final : LOMError {
  ParsingError(const std::string& what, const std::string& err_context, const unsigned line_num) :
  LOMError(Stage::ParsingError, what, err_context, line_num) {}

  ParsingError(const std::string& what, const Type& type, unsigned line_num);
  ParsingError(const std::string& what, const Lexer::Token& token);
};

struct ValidationError final : LOMError {
  ValidationError(const std::string& what, const std::string& err_context, const unsigned line_num) :
  LOMError(Stage::ValidationError, what, err_context, line_num) {}
};