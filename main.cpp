#include <stdexcept>

#include "src/lexing/lex.hpp"
#include "src/parsing/firstparse.hpp"
#include "src/parsing/secondparse.hpp"
#include "src/validation/ast_validation.hpp"

using namespace Parser;
using namespace Lexer;
using namespace Validation;

int main(int argc, char* argv[]) {
  if(argc != 2)
    throw std::runtime_error("Name of file as argument required");
  
  validateTU(secondPassParsing(firstPassParsing(tokenizeFile(argv[1]))));
  return 0;
}
