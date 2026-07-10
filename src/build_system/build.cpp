#include "build.hpp"
#include "backends/codegen.hpp"
#include "error.hpp"
#include "lexing/lex.hpp"
#include "parsing/parse.hpp"
#include "peepir/peepir.hpp"
#include "semantic_analysis/symbol_table.hpp"

#include <filesystem>
#include <print>
#include <chrono>
#include <thread>
using namespace LOM;
namespace fs = std::filesystem;


static TypeContext types;
static std::unordered_map<std::string_view, Module> modules;
static Module main_module("", &types);
static const fs::path src_path{"src"};
static const fs::path extern_path{"extern"};

[[nodiscard]] Module*
LOM::getModule(std::string_view name) {
  if (not modules.contains(name))
    throw std::runtime_error("Module name not found!");
  return &modules.at(name);
}

#ifdef PROFILE
void LOM::reset_state() noexcept {
  for (auto& kv : modules) {
    if (kv.first not_eq std::string_view("__C"))
      delete[] kv.first.data();
  }

  modules.~unordered_map<std::string_view, Module>();
  main_module.~Module();
  types.~TypeContext();

  new (&types) TypeContext;
  new (&main_module) Module("", &types);
  new (&modules) std::unordered_map<std::string_view, Module>();
}
#endif

// Print Output
namespace {

eden_noinline_cold void
print_parser(std::vector<Parser::TU> const& tus, std::vector<fs::path> const& paths) {
  assert(tus.size() == paths.size()); assert(Settings::do_output_parser);
  for (auto i{0uz}; i<tus.size(); ++i) {
    std::println("\n--- Parser Output --- {}", paths[i].native());
    Parser::printTU(tus[i]);
    std::println("\n--- Parser Output ---");
  }
}

eden_noinline_cold void
print_peep(std::vector<PeepIR::TU> const& tus, std::vector<fs::path> const& paths) {
  assert(tus.size() == paths.size()); assert(Settings::do_output_peep);
  for (auto i{0uz}; i<tus.size(); ++i) {
    std::println("\n--- Peep Output --- {}", paths[i].native());
    PeepIR::printPeep(tus[i]);
    std::println("\n--- Peep Output ---");
  }
}

}

// Print Error Output
namespace {

eden_noinline_cold void
print_lexer_errors(File file) {
  std::println("\n--- Lexer Errors --- {}", file.path());
  std::println("{}", get_file_errors(file));
  std::println("\n--- Lexer Errors ---");
}

eden_noinline_cold void
print_parser_errors(File file) {
  std::println("\n--- Parser Errors --- {}", file.path());
  std::println("{}", get_file_errors(file));
  std::println("\n--- Parser Errors ---");
}

eden_noinline_cold void
print_peep_errors(PeepIR::TU const& peep_tu) {
  for (auto& file : peep_tu.source_files) {
    std::println("\n--- Peepir Errors --- {}", file.path());
    std::println("{}", get_file_errors(file));
    std::println("\n--- Peepir Errors ---");
  }
}

}

// LOL
static std::vector<fs::path> extern_objects_paths;
static void compileC() {
  if (not fs::exists(extern_path) or is_empty(fs::directory_entry(extern_path))) return;

  std::string command =
    std::format("(cd build/obj && {} ", Settings::external_compiler);
  switch (Settings::getOptimizationLevel()) {
  case 0: break;
  case 1: command.append(" -O1 "); break;
  case 2: command.append(" -O2 "); break;
  case 3: command.append(" -O3 "); break;
  default: eden_unreachable("Invalid optimization level.");
  }
  command.append("-c ");
  for (auto& file : fs::directory_iterator{extern_path}) {
    if (file.path().extension() != ".c") continue;
    extern_objects_paths.emplace_back(file.path());
    command.append(
      std::format("../../{} ",
        file.path().native())
      );
  }

  command.push_back(')');
  system(command.c_str());
}

// populates tu and returns whether an error was encountered
[[nodiscard]] bool
lex_and_parse_module(Parser::TU& tu, fs::path const& directory)  {
  std::vector<Lexer::Token> tokens; tokens.reserve(64);

  { // set up module, this is horrible please change
    assert(not modules.contains(directory.c_str()));
    auto const n = directory.filename().native().size() + 1; // this is so stupid i hate this language
    auto const module_name = new char[n]; // TODO: fix purposeful memory leak
    std::strcpy(module_name, directory.filename().c_str());
    std::string_view const key_view(module_name, n-1);
    auto const module_ptr = &modules.emplace(std::pair(key_view, Module{key_view, &types})).first->second;
    tu.module = module_ptr;
  }

  bool has_error = false;
  for (auto const& entry : fs::directory_iterator{directory}) {
    auto const& path = entry.path();
    if (not entry.is_regular_file())          throw std::runtime_error( std::format("LookOnceMore: Sorry! Submodules not supported yet.\nModule Path: {}", path.string() ));
    if (path.extension() != ".lom") continue;
    if (is_empty(entry))                    throw std::runtime_error( std::format("LookOnceMore: Sorry! Empty files not supported.\nFile Path: {}", path.string() ));

    auto const file = tu.source_files.emplace_back(path);
    if      (Lexer::tokenizeFile(tokens, file))  print_lexer_errors(file),  has_error = true;
    else if (Parser::parseTokens(tu, tokens)) print_parser_errors(file), has_error = true;

    tokens.clear();
  }

  return has_error;
}

void LOM::build() {
  if (not fs::exists(src_path)) throw std::runtime_error("LookOnceMore: src directory not found!");
  std::thread comp_extern;
  if constexpr (not Settings::external_compiler.empty()) comp_extern = std::thread(compileC);
#ifdef STAGE_BENCHMARKS
  auto begin_time = std::chrono::high_resolution_clock::now();
#endif
  modules.emplace(std::pair(std::string_view("__C"), Module{"__C", &types}));

  std::vector<Parser::TU> parsed_tus;   parsed_tus.reserve(4);
  std::vector<fs::path> module_paths; module_paths.reserve(4);
  bool has_error = false;
  for (auto const& entry : fs::directory_iterator{src_path}) {
    if (entry.is_directory()) {
      if (is_empty(entry) or entry.path().filename().native()[0] == '.') continue;

      module_paths.emplace_back(entry.path().stem());
      auto& ptu = parsed_tus.emplace_back();
      if (lex_and_parse_module(ptu, entry)) has_error = true;
      continue;
    }

    std::vector<Lexer::Token> main_tokens; main_tokens.reserve(64);
    Parser::TU main_tu; main_tu.module = &main_module;
    auto const& main_path = entry.path();
    auto const main_file = main_tu.source_files.emplace_back(main_path);

    if      (Lexer::tokenizeFile(main_tokens,  main_file))  print_lexer_errors(main_file),  has_error = true;
    else if (Parser::parseTokens(main_tu, main_tokens))  print_parser_errors(main_file), has_error = true;

    module_paths.emplace_back("main.lom");
    parsed_tus.emplace_back(std::move(main_tu));
  }
  if (has_error) std::quick_exit(1);
  if (Settings::do_output_parser) return print_parser(parsed_tus, module_paths);


  std::vector<PeepIR::TU> peeped_tus; peeped_tus.reserve(parsed_tus.size());
  for (auto& parsed_tu : parsed_tus) {
    auto& tu = peeped_tus.emplace_back();
    auto const error = PeepIR::lowerToPeep(tu, std::move(parsed_tu));
    if (error) print_peep_errors(tu), has_error = true;
  }
  if (has_error) std::quick_exit(1);
  if (Settings::do_output_peep) return print_peep(peeped_tus, module_paths);

  std::vector<std::unique_ptr<Backend>> compiled_tus; peeped_tus.reserve(parsed_tus.size());
  for (auto i{0uz}; i<peeped_tus.size(); ++i) {
    auto& module_path = module_paths[i];
    auto& peeped = peeped_tus[i];

    [[maybe_unused]]
    auto const& compiled = compiled_tus.emplace_back(
      Backend::codegen( std::move(peeped), module_path )
      );

#ifdef NO_MEASUREMENT
    if (Settings::do_output_asm)
      compiled->createASMFile(module_path);
    if (Settings::do_output_llvmir)
      compiled->createIRFile(module_path);
    if (Settings::do_output_obj)
      module_path = compiled->createObjectFile(module_path);
#endif
  }

#ifdef STAGE_BENCHMARKS
  auto end_time = std::chrono::high_resolution_clock::now();
  std::println("{:>10}, {:>10} | FULL",
    end_time - begin_time,
    std::chrono::duration_cast<std::chrono::microseconds>(end_time - begin_time)
  );
  std::println("{:>10} Full Parsing Duration.", Parser::parsing_durr);
#endif

  if (comp_extern.joinable()) {
    comp_extern.join();
    for (auto& extern_path : extern_objects_paths) {
      module_paths.reserve(module_paths.size() + extern_objects_paths.size());
      module_paths.emplace_back(std::move(extern_path));
    }
  }

#ifdef NO_MEASUREMENT
  if (Settings::do_output_obj)
    Backend::linkObjects(module_paths);
#endif

}
