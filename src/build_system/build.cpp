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

/*
[[maybe_unused]] static std::unique_ptr<Backend>
processLOMFile(const std::filesystem::path& filename)
try {
  return Backend::codegen(
                  PeepMIR::lowerToPeep(
                  Parser::parseTokens(
                  Lexer::tokenizeFile(filename)
                  )),filename.stem()); }
catch (LOMError& e) { std::cout << e.error_message << std::endl; std::quick_exit(1); } */


namespace fs = std::filesystem;
[[maybe_unused]] static Parser::TU
lex_and_parse_module(const fs::path& directory) {
  std::vector<Lexer::Token> tokens; tokens.reserve(128); //cuz why not
  for (auto const& entry : fs::directory_iterator{directory}) {
    if (not entry.is_regular_file())
      throw std::runtime_error("LookOnceMore: Sorry! Submodules not supported yet.");
    Lexer::tokenizeFile(tokens, entry);
  }
  const auto module_ptr = &modules[directory.c_str()];

  return Parser::parseTokens(tokens, module_ptr);
}

void LOM::build() {
  //1: Lex each file seperately, filling up one total list of tokens
  //2: Parse tokens together as one module
  //3: When all modules have been parsed, then peeping can begin
  if (not fs::exists("src"))
    throw std::runtime_error("LookOnceMore: src directory not found!");

  std::vector<Parser::TU> parsed_tus;
  std::vector<fs::path> module_names;
  for (auto const& entry : fs::directory_iterator{"src"}) {
    if (entry.is_regular_file())
      std::println("{}", entry.path().string());
    else if (entry.is_directory()) {
      module_names.emplace_back(entry.path().filename());
      parsed_tus.emplace_back(lex_and_parse_module(entry));
    }
  }

  std::vector<std::unique_ptr<Backend>> compiled_tus;
  for (auto i{0uz}; i<parsed_tus.size(); ++i) {
    compiled_tus.emplace_back(Backend::codegen(PeepMIR::lowerToPeep(std::move(parsed_tus[i])), module_names[i]))->createObjectFile(module_names[i]);
  }
}