#include "src/lex.hpp"
#include "src/parse.hpp"
int main() {
  std::vector<Lexer::Token> token_list = Lexer::tokenizeFile("Code.lom");
  Parser::beginParsing(std::move(token_list));
  return 0;
}
