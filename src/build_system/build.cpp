#include "build.hpp"
#include "backends/codegen.hpp"
#include "error.hpp"
#include "lexing/lex.hpp"
#include "parsing/parse.hpp"
#include "peep_mir/peep_mir.hpp"
#include "semantic_analysis/symbol_table.hpp"

#include <filesystem>
#include <fstream>
#include <print>
using namespace LOM;

static Module main_module("");

static std::unordered_map<std::string_view, Module> modules;

[[nodiscard]] Module*
LOM::getModule(std::string_view name) {
  if (not modules.contains(name))
    throw ValidationError("Module name not found!", "", 0);
  return &modules.at(name);
}

namespace fs = std::filesystem;
[[maybe_unused]] static Parser::TU
lex_and_parse_module(const fs::path& directory) {
  std::vector<Lexer::Token> tokens; tokens.reserve(128); //cuz why not
  for (auto const& entry : fs::directory_iterator{directory}) {
    if (not entry.is_regular_file())
      throw std::runtime_error("LookOnceMore: Sorry! Submodules not supported yet.");
    Lexer::tokenizeFile(tokens, entry);
  }
  assert(not modules.contains(directory.c_str()));

  const sz_t n = directory.filename().string().size() + 1; // this is so fucking stupid i hate this language
  const auto key = new char[n]; //purposeful memory leak
  std::strcpy(key, directory.filename().c_str());
  const std::string_view key_view(key, n-1);
  auto [pair, _] = modules.emplace(std::pair(key_view, Module{key_view}));

  const auto module_ptr = &pair->second;
  return Parser::parseTokens(tokens, module_ptr);
}

static void setup_std() {
  //static constinit eden::TemplateString std_module_name{"std"};
  //static constinit eden::owned_stringview x(std_module_name.data.data(), 3);
  //static constinit Module::Variable y[4]{{}, {}, {}, {}};

  modules.emplace(std::pair(std::string_view("__C"), Module{"__C"}));

  //standard_lib.addFunction(x, , nullptr, true);
}

void LOM::build()
try {
  if (not fs::exists("src"))
    throw std::runtime_error("LookOnceMore: src directory not found!");

  setup_std();

  std::vector<Parser::TU> parsed_tus;
  std::vector<fs::path> module_names; bool has_main = false;
  for (auto const& entry : fs::directory_iterator{"src"}) {
    if (entry.is_regular_file()) {
      if (has_main)
        throw ValidationError("There may only be one top level .lom file, named main.lom", entry.path().filename(), 0);
      if (entry.path().filename() != "main.lom")
        throw ValidationError("Top level .lom file must be called main.lom", entry.path().filename(), 0);
      has_main = true;
      std::vector<Lexer::Token> main_tokens;
      Lexer::tokenizeFile(main_tokens, entry);
      module_names.emplace_back(entry.path().filename());
      parsed_tus.emplace_back(Parser::parseTokens(main_tokens, &main_module));
    }
    else if (entry.is_directory()) {
      module_names.emplace_back(entry.path().filename());
      parsed_tus.emplace_back(lex_and_parse_module(entry));
    }
  }

  const bool output_asm = Settings::doOutputASM();
  const bool output_ir = Settings::doOutputIR();
  const bool output_obj = Settings::doOutputOBJ() or Settings::doLinking();
  std::vector<std::unique_ptr<Backend>> compiled_tus;
  for (auto i{0uz}; i<parsed_tus.size(); ++i) {
    auto& module_name = module_names[i];
    auto& compiled = compiled_tus.emplace_back(
      Backend::codegen(
        PeepMIR::lowerToPeep(std::move(parsed_tus[i])),
        module_names[i]));

    if (output_asm)
      compiled->createASMFile(module_name);
    if (output_ir)
      compiled->createIRFile(module_name);
    if (output_obj)
      module_name = compiled->createObjectFile(module_name);
  }

  if (Settings::doLinking() and has_main)
    Backend::linkObjects(module_names);
}
catch (LOMError& e) { std::cout << e.error_message << std::endl; std::quick_exit(1); }
