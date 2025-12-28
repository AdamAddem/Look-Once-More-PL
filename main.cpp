#include "src/lexing/lex.hpp"
#include "src/parsing/firstparse.hpp"
#include "src/parsing/secondparse.hpp"
#include "src/validation/ast_validation.hpp"

using namespace Parser;
using namespace Lexer;
using namespace Validation;

int main() {
  validateTU(secondPassParsing(firstPassParsing(tokenizeFile("code.lom"))));
  return 0;
}
