#pragma once
#include "statements.hpp"

namespace Parser {

struct ParsedGlobals {
  std::vector<VarDeclaration> globals;
  std::vector<Statement *> global_init_body;

  ParsedGlobals(std::vector<VarDeclaration> &&_globals,
                std::vector<Statement *> _globals_body)
      : globals(std::move(_globals)),
        global_init_body(std::move(_globals_body)) {}

  ParsedGlobals(ParsedGlobals &&other) noexcept
      : globals(std::move(other.globals)),
        global_init_body(std::move(other.global_init_body)) {}
};

struct ParsedFunction {
  Type return_value;
  std::string name;
  std::vector<VarDeclaration>
      parameter_list; // VarDeclarations should have expr = nullptr
  std::vector<Statement *> function_body;

  ParsedFunction(Type &&_return_value, std::string &&_name,
                 std::vector<VarDeclaration> &&_parameter_list,
                 std::vector<Statement *> &&_function_body)
      : return_value(std::move(_return_value)), name(std::move(_name)),
        parameter_list(std::move(_parameter_list)),
        function_body(std::move(_function_body)) {}

  ParsedFunction(ParsedFunction &&other) noexcept
      : return_value(std::move(other.return_value)),
        name(std::move(other.name)),
        parameter_list(std::move(other.parameter_list)),
        function_body(std::move(other.function_body)) {}

  void print();
};

struct ParsedTranslationUnit {
  ParsedGlobals global;
  std::vector<ParsedFunction> functions;

  ParsedTranslationUnit(ParsedGlobals &&_global,
                        std::vector<ParsedFunction> &&_functions)
      : global(std::move(_global)), functions(std::move(_functions)) {}
};

class UnparsedTU;
void secondPassParsing(UnparsedTU &&tu);

} // namespace Parser
