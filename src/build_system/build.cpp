#include "build.hpp"
#include "backends/codegen.hpp"
#include "error.hpp"
#include "lexing/lex.hpp"
#include "parsing/parse.hpp"
#include "peepir/peepir.hpp"
#include "semantic_analysis/symbol_table.hpp"

#include <filesystem>
#include <fstream>
#include <print>
using namespace LOM;


static TypeContext types;
static std::unordered_map<std::string_view, Module> modules;
static Module main_module("", &types);

[[nodiscard]] Module*
LOM::getModule(std::string_view name) {
  if (not modules.contains(name))
    throw std::runtime_error("Module name not found!");
  return &modules.at(name);
}

namespace fs = std::filesystem;

// returns whether an error occured
[[nodiscard]] bool
lex_and_parse_file(
  Parser::TU& tu,
  std::vector<Lexer::Token>& tokens,
  fs::path const& file_path) {

  auto file_opt = Lexer::tokenizeFile(tokens, file_path);
  if (not file_opt.has_value()) {
    std::println("{}", get_file_errors(file_path.native()) );
    return false;
  }

  auto file = std::move(file_opt.value());
  if (Settings::output_lexer) {
    std::print("\n--- Lexer Output --- {}", file_path.string());
    Lexer::TokenView(tokens.begin() + file_start, tokens.end()).print(file);
    std::println("\n--- Lexer Output ---");
    return true;
  }

  tu.source_files.emplace_back(std::move(file));
  Parser::parseTokens(tu, tokens.begin(), tokens.end());
  return true;
}

[[nodiscard]] static Parser::TU
lex_and_parse_module(fs::path const& directory) {
  std::vector<Lexer::Token> tokens; tokens.reserve(64);
  auto file_start{0uz};

  Parser::TU tu;
  { // set up module
    assert(not modules.contains(directory.c_str()));
    const sz_t n = directory.filename().string().size() + 1; // this is so stupid i hate this language
    auto const module_name = new char[n]; // TODO: fix purposeful memory leak
    std::strcpy(module_name, directory.filename().c_str());
    const std::string_view key_view(module_name, n-1);
    auto const module_ptr = &modules.emplace(std::pair(key_view, Module{key_view, &types})).first->second;
    tu.module = module_ptr;
  }

  bool success = true;
  for (auto const& entry : fs::directory_iterator{directory}) {
    if (not entry.is_regular_file())
      throw std::runtime_error("LookOnceMore: Sorry! Submodules not supported yet.");

    success = lex_and_parse_file(tu, tokens, file_start, entry.path()) and success;
    file_start = tokens.size();
  }
  if (not success) std::quick_exit(1);

  return tu;
}

void LOM::build() {
  if (not fs::exists("src"))
    throw std::runtime_error("LookOnceMore: src directory not found!");

  modules.emplace(std::pair(std::string_view("__C"), Module{"__C", &types}));

  std::vector<Parser::TU> parsed_tus;
  std::vector<fs::path> module_names;
  for (auto const& entry : fs::directory_iterator{"src"}) {
    if (entry.is_directory()) [[likely]] {
      module_names.emplace_back(entry.path().filename());
      parsed_tus.emplace_back(lex_and_parse_module(entry));
      continue;
    }

    std::vector<Lexer::Token> main_tokens; main_tokens.reserve(64);
    Parser::TU main_tu; main_tu.module = &main_module;
    if (lex_and_parse_file(main_tu, main_tokens, 0, entry.path()) == false) std::quick_exit(1);

  }

  std::vector<std::unique_ptr<Backend>> compiled_tus;
  for (auto i{0uz}; i<parsed_tus.size(); ++i) {
    auto& module_name = module_names[i];
    if (Settings::output_parser) {
      std::println("\n--- Parser Output --- {}", module_name.string());
      Parser::printTU(parsed_tus[i]);
      std::println("\n--- Parser Output ---");
      continue;
    }
    if (Settings::output_peep) {
      std::println("\n--- Peep Output --- {}", module_name.string());
      auto x = (PeepMIR::lowerToPeep(std::move(parsed_tus[i])));
      PeepMIR::printPeep(x);
      std::println("\n--- Peep Output ---");
      continue;
    }
    auto const& compiled = compiled_tus.emplace_back(
      Backend::codegen(
        PeepMIR::lowerToPeep(std::move(parsed_tus[i])),
        module_names[i]));

    if (Settings::output_asm)
      compiled->createASMFile(module_name);
    if (Settings::output_llvmir)
      compiled->createIRFile(module_name);
    if (Settings::output_obj)
      module_name = compiled->createObjectFile(module_name);
  }

  if (Settings::do_linking or Settings::output_obj)
    Backend::linkObjects(module_names);
}
