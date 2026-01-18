#pragma once
#include "../grammar/statements.hpp"

namespace Parser {

struct ParsedGlobals {
  std::vector<VarDeclaration> declarations;
  std::vector<Statement *> global_init_body;

  ParsedGlobals(std::vector<VarDeclaration> &&_globals,
                std::vector<Statement *> _globals_body)
      : declarations(std::move(_globals)),
        global_init_body(std::move(_globals_body)) {}

  ParsedGlobals(ParsedGlobals &&other) noexcept
      : declarations(std::move(other.declarations)),
        global_init_body(std::move(other.global_init_body)) {}

  void print();
};

struct ParsedFunction {
  StrictType return_type;
  std::string name;
  std::vector<VarDeclaration>
      parameter_list; // VarDeclarations should have expr = nullptr
  std::vector<Statement *> function_body;

  ParsedFunction(StrictType &&_return_value, std::string &&_name,
                 std::vector<VarDeclaration> &&_parameter_list,
                 std::vector<Statement *> &&_function_body)
      : return_type(std::move(_return_value)), name(std::move(_name)),
        parameter_list(std::move(_parameter_list)),
        function_body(std::move(_function_body)) {}

  ParsedFunction(ParsedFunction &&other) noexcept
      : return_type(std::move(other.return_type)), name(std::move(other.name)),
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

  void print();
};

struct UnparsedTU;
ParsedTranslationUnit secondPassParsing(UnparsedTU &&tu);

} // namespace Parser
