#include <stdexcept>
#include <iostream>

#include "arguments.hpp"
#include "error.hpp"
#include "src/backends/llvm/tollvm.hpp"
#include "src/lexing/lex.hpp"
#include "src/parsing/parse.hpp"
#include "src/validation/ast_validation.hpp"

#include <cassert>
#include <filesystem>

using namespace Parser;
using namespace Lexer;
using namespace Validation;
using namespace ToLLVM;



static ValidatedTU processFile(const std::filesystem::path& filename)
try {
  return validateTU( parseTokens( tokenizeFile(filename)));
}
catch (LOMError& e) {
  std::cout << e.error_message << std::endl;
  assert(false);
}


int main(const int argc, const char* argv[]) {
  if(argc < 2)
    throw std::runtime_error("Arguments required");

  std::vector<std::filesystem::path> filepaths = Arguments::setArgs(argc, argv);

  if (filepaths.empty())
    throw std::runtime_error("At least one file name must be specified");

  if (Arguments::doOutputIR()) {
    for (auto& filename : filepaths)
      filename = createIRFile(processFile(filename), filename);
  }
  else if (Arguments::doOutputAsm()) {
    for (auto& filename : filepaths)
      filename = createASMFile(processFile(filename), filename);
  }
  else {
    for (auto& filename : filepaths)
      filename = createObjectFile(processFile(filename), filename);

    linkObjects(filepaths);
  }


  return 0;
}
