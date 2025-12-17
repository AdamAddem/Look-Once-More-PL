#include "src/firstparse.hpp"
#include "src/secondparse.hpp"
int main() {

  Parser::secondPassParsing(
      Parser::firstPassParsing(Lexer::tokenizeFile("Code.lom")));
  return 0;
}
