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
  std::vector<Lexer::Token> tokens; tokens.reserve(128); // cuz why not
  auto file_start = 0;
  const auto output_lexer = Settings::doOutputLexer();

  //set up module
  assert(not modules.contains(directory.c_str()));
  const sz_t n = directory.filename().string().size() + 1; // this is so stupid i hate this language
  const auto module_name = new char[n]; //purposeful memory leak
  std::strcpy(module_name, directory.filename().c_str());
  const std::string_view key_view(module_name, n-1);
  auto module_ptr = &modules.emplace(std::pair(key_view, Module{key_view})).first->second;

  Parser::TU module_tu(module_ptr, module_name);
  for (auto const& entry : fs::directory_iterator{directory}) {
    if (not entry.is_regular_file())
      throw std::runtime_error("LookOnceMore: Sorry! Submodules not supported yet.");

    Lexer::tokenizeFile(tokens, entry);
    if (output_lexer) {
      std::cout << "\n--- Lexer Output --- " << directory.string();
      Lexer::TokenView(tokens.begin() + file_start, tokens.end()).print();
      std::cout << "\n--- Lexer Output ---\n";
    }

    Parser::parseTokens(module_tu, tokens.begin() + file_start, tokens.end());
  }

  return module_tu;
}

static void setup_std() {
  auto p = &modules.emplace(std::pair(std::string_view("__C"), Module{"__C"})).first->second;
  Module::Variable chptr{{p->addRawPointer({PrimitiveType::char_(), {}}), {}}, ""};
  p->addFunction("puts", std::span(&chptr, 1), Type::devoid(), true, false);
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

      static Module main_module(""); has_main = true;
      std::vector<Lexer::Token> main_tokens;
      Lexer::tokenizeFile(main_tokens, entry);
      if (Settings::doOutputLexer()) {
        std::cout << "\n--- Lexer Output --- " << "main.lom";
        Lexer::TokenView(main_tokens.begin(), main_tokens.end()).print();
        std::cout << "\n--- Lexer Output ---\n";
      }

      module_names.emplace_back(entry.path().filename());
      parsed_tus.emplace_back(&main_module, "main");
      Parser::parseTokens(parsed_tus.back(), main_tokens.begin(), main_tokens.end());
    }
    else if (entry.is_directory()) [[likely]] {
      module_names.emplace_back(entry.path().filename());
      parsed_tus.emplace_back(lex_and_parse_module(entry));
    }
  }

  if (not has_main)
    throw std::runtime_error("Expected src/main.lom.");

  const bool emit_parser = Settings::doOutputParser();
  const bool emit_peep = Settings::doOutputPeep();
  const bool output_asm = Settings::doOutputASM();
  const bool output_ir = Settings::doOutputIR();
  const bool output_obj = Settings::doOutputOBJ() or Settings::doLinking();
  std::vector<std::unique_ptr<Backend>> compiled_tus;
  for (auto i{0uz}; i<parsed_tus.size(); ++i) {
    auto& module_name = module_names[i];
    if (emit_parser) {
      std::println("\n--- Parser Output --- {}", module_name.string());
      Parser::printTU(parsed_tus[i]);
      std::println("\n--- Parser Output ---");
      continue;
    }
    if (emit_peep) {
      std::println("\n--- Peep Output --- {}", module_name.string());
      auto x = (PeepMIR::lowerToPeep(std::move(parsed_tus[i])));
      PeepMIR::printPeep(x);
      std::println("\n--- Peep Output ---");
      continue;
    }
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

  if (Settings::doLinking())
    Backend::linkObjects(module_names);
}
catch (LOMError& e) { std::cout << e.error_message << std::endl; std::quick_exit(1); }
