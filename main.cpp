#include <iostream>
#include <stdexcept>

#include "ast/expressions.hpp"
#include "backends/codegen.hpp"
#include "error.hpp"
#include "lexing/lex.hpp"
#include "parsing/parse.hpp"
#include "settings.hpp"
#include "validation/ast_validation.hpp"

#include <cassert>
#include <filesystem>

using namespace Parser;
using namespace Lexer;
using namespace Validation;



static std::unique_ptr<Backend> processLOMFile( const std::filesystem::path& filename )
try { return Backend::codegen(validateTU( parseTokens( tokenizeFile(filename) ) ), filename.stem()); }
catch (LOMError& e) { std::cout << e.error_message << std::endl; assert(false); }


int main(const int argc, const char* argv[]) {
  if(argc < 2)
    throw std::runtime_error("Arguments required");

  std::vector<std::filesystem::path> filepaths = Settings::setArgs(argc, argv);

  if (filepaths.empty())
    throw std::runtime_error("At least one file name must be specified");



  const bool output_asm = Settings::doOutputASM();
  const bool output_ir = Settings::doOutputIR();
  const bool output_obj = Settings::doOutputOBJ() || Settings::doLinking();
  for (auto& filename : filepaths) {
    const auto compiled = processLOMFile(filename);

    if (output_asm)
      compiled->createASMFile(filename);

    if (output_ir)
      compiled->createIRFile(filename);

    if (output_obj)
      filename = compiled->createObjectFile(filename);

  }

  if (Settings::doLinking())
    Backend::linkObjects(filepaths);


  return 0;
}
