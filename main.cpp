#include "src/lexing/lex.hpp"
#include "src/parsing/firstparse.hpp"
#include "src/parsing/secondparse.hpp"

using namespace Parser;
using namespace Lexer;

int main() {
  secondPassParsing(firstPassParsing(tokenizeFile("Code.lom")));
  return 0;
}
