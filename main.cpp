#include <stdexcept>
#include <unordered_map>

#include "debug_flags.hpp"
#include "src/lexing/lex.hpp"
#include "src/parsing/parse.hpp"
#include "src/validation/ast_validation.hpp"

using namespace Parser;
using namespace Lexer;
using namespace Validation;

enum class Args {
  OUTPUT_LEXER,
	OUTPUT_PARSE,
	OUTPUT_VALIDATION
};


static void set_flags(const std::string& arg_str) {
  static const std::unordered_map<std::string, Args> stringToArgs {
          {"-L", Args::OUTPUT_LEXER},
  {"-P", Args::OUTPUT_PARSE},
          {"-V", Args::OUTPUT_VALIDATION},
  };

  if (!stringToArgs.contains(arg_str))
          throw std::runtime_error("Argument not recognized");

  switch (stringToArgs.at(arg_str)) {
  case Args::OUTPUT_LEXER:
          lom_debug::output_lexing = true;
          break;
  case Args::OUTPUT_PARSE:
          lom_debug::output_parse = true;
          break;
  case Args::OUTPUT_VALIDATION:
          lom_debug::output_validation = true;
          break;

  default: throw std::runtime_error("error with arg passed, shouldn't happen");
  }
}

int main(const int argc, const char* argv[]) {
  if(argc < 2)
    throw std::runtime_error("Name of file as argument required");

  if (argc == 3) set_flags(argv[2]);

  validateTU(parseTokens(tokenizeFile(argv[1])));
  return 0;
}
