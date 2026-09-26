#include "build.hpp"
#include "backends/codegen.hpp"
#include "edenlib/multi_iter.hpp"
#include "lexing/lex.hpp"
#include "module/table_and_module.hpp"
#include "parsing/parse.hpp"
#include "peepir/peepir.hpp"

#include <chrono>
#include <cstdio>
#include <filesystem>
#include <print>
#include <thread>

using namespace LOM;
namespace fs = std::filesystem;

namespace {
constexpr auto C_TU_IDX = 0uz;
constexpr auto MAIN_TU_IDX = 1uz;
fs::path const src_path{"src"};
fs::path const extern_path{"extern"};

union TUnion {
  GenericTU  generic_tu;
  Parser::TU parser_tu;
  PeepIR::TU peepir_tu;

  edenInlineCXPR explicit TUnion(u16_t tu_id) : parser_tu(tu_id) { generic_tu.source_files.reserve(1); }
  edenInlineCXPR ~TUnion() { peepir_tu.~TU(); }
};
static_assert( sizeof(Parser::TU) == sizeof(PeepIR::TU) );

// using a global struct so state can be easily reset when profiling
// kinda dumb, but so is this language
struct GlobalStruct {
  std::unordered_map<std::string_view, u16_t> module_map;
  eden::vector<fs::path> module_paths;
  eden::vector<fs::path> extern_objects_paths;
  sz_t num_external_objects;

private:
  eden::owned_span<TUnion> tu_list;

  void init_module_paths() noexcept {
    module_paths.reserve(4);
    module_paths.emplace_back(extern_path);
    module_paths.emplace_back(src_path);
    for (auto const& entry : fs::directory_iterator{src_path}) {
      if (not entry.is_directory()) continue;

      if (is_empty(entry) or entry.path().filename().native()[0] == '.') continue; // jank, ignores directories starting with .
      auto const& p = entry.path();
      module_paths.emplace_back(p);
    }

#ifndef NDEBUG
    std::println("Module paths: ");
    for (auto const& path : module_paths)
      std::println("\t- '{}'", path.native());
    std::println();
#endif
  }

  void init_tu_list() noexcept {
    auto const num_tus = module_paths.size();

    // we do this malarky instead of using a vector because TUnion's move/copy constructor cannot be implemented in any satisfactory way
    // and vector requires a movable object even if we reserve upfront
    tu_list.reset(
        std::start_lifetime_as_array<TUnion>(
            ::operator new(num_tus * sizeof(TUnion), align_t{64})
            , num_tus),
        num_tus
        );

    ( new (tu_list.data()) TUnion(C_TU_IDX) ) ->generic_tu.name = "__C";

    auto const n = (u16_t)num_tus;
    for (u16_t i{1}; i<n; ++i)
      new (tu_list.data() + i) TUnion(i);
  }

  void init_module_map() noexcept {
    module_map.reserve(module_paths.size());
    module_map.emplace("__C", C_TU_IDX);
  }
public:

  edenInlineNodiscardCXPR sz_t numTUs() const noexcept { return tu_list.size(); }
  edenInlineNodiscardCXPR TUnion& tuAt(u16_t tu_id) noexcept { return tu_list[tu_id]; }
  edenInlineNodiscardCXPR Module& moduleAt(u16_t module_id) noexcept { return tuAt(module_id).generic_tu.module; }

  edenInlineNodiscardCXPR std::span<TUnion> getTUs() noexcept { return tu_list.to_span().subspan(MAIN_TU_IDX); } // does not include the CModule TU
  edenInlineNodiscardCXPR std::span<fs::path> getTUPaths() noexcept { return module_paths.to_span().subspan(MAIN_TU_IDX); } // does not include the CModule path

  constexpr GlobalStruct() noexcept {
    init_module_paths();
    init_tu_list();
    init_module_map();
    num_external_objects = extern_objects_paths.size();
  }

#ifdef PROFILE
  constexpr ~GlobalStruct() noexcept {
    for (auto& kv : module_map) {
      if (kv.first not_eq std::string_view("__C"))
        delete[] kv.first.data();
    }

    for (auto& tu : tu_list) tu.~TUnion();
    ::operator delete(tu_list.data(), tu_list.size() * sizeof(TUnion), align_t{64});
  }
#endif

}globals;

// LOL
void compileC() {
  if (not fs::exists(extern_path) or is_empty(fs::directory_entry(extern_path))) return;

  std::string command = std::format("(cd build/obj && {} ", Settings::external_compiler);
  switch (Settings::getOptimizationLevel()) {
  case 0: break;
  case 1: command.append(" -O1 "); break;
  case 2: command.append(" -O2 "); break;
  case 3: command.append(" -O3 "); break;
  default: edenUnreachable("Invalid optimization level.");
  }

  command.append("-c ");
  for (auto& file : fs::directory_iterator{extern_path}) {
    if (file.path().extension() != ".c") continue;
    globals.extern_objects_paths.emplace_back(
      file.path().filename()).replace_extension(obj_extension);
    command.append(
      std::format("../../{} ", file.path().native())
      );
  }

  command.push_back(')');
  system(command.c_str());
}

edenNoInlineCold void
print_parsed(std::span<TUnion const> tus) noexcept {
  for (auto const& tu : tus) {
    std::println("\n--- Parser Output --- {}", tu.generic_tu.name);
    Parser::printTU(tu.parser_tu);
    std::println("\n--- Parser Output ---");
  }
}

edenNoInlineCold void
print_peeped(std::span<TUnion const> tus) noexcept {
  for (auto const& tu : tus) {
    std::println("\n--- Peep Output --- {}", tu.generic_tu.name);
    PeepIR::printPeep(tu.peepir_tu);
    std::println("\n--- Parser Output ---");
  }
}

edenNoInlineCold void
print_errors(File file) {
  std::println("\n--- Errors --- {}", file.path());
  std::println("{}", get_file_errors(file));
  std::println("\n--- Errors ---");
}

}

// returns nullptr if not found
edenPure [[nodiscard]] Module*
LOM::getModule(std::string_view module_name) noexcept {
  auto const iter = globals.module_map.find(module_name);
  if (iter == globals.module_map.end()) return nullptr;
  return &globals.moduleAt(iter->second);
}


edenHot edenPure [[nodiscard]] Module&
LOM::getModule(u16_t module_id) noexcept {
  assert(module_id < globals.numTUs());
  return globals.moduleAt(module_id);
}

edenPure [[nodiscard]] std::string_view
LOM::getNameOfModule(u16_t module_id) noexcept {
  assert(module_id < globals.numTUs());
  return globals.tuAt(module_id).generic_tu.name;
}

edenPure [[nodiscard]] Module& LOM::getCModule() noexcept { return globals.moduleAt(C_TU_IDX); }

namespace {

// This is horrible please change. TODO: Eradicate.
void setup_module(Parser::TU& tu, u16_t module_id, fs::path const& path) noexcept {
  assert(not globals.module_map.contains(path.c_str()));
  auto const n = path.filename().native().size() + 1; // this is so stupid i hate this language
  auto const module_name_cstr = new char[n]; // TODO: fix purposeful memory leak
  std::strcpy(module_name_cstr, path.filename().c_str());
  auto const module_name = std::string_view{module_name_cstr, n-1};

  globals.module_map.emplace(module_name, module_id);
  tu.name = module_name;
}

[[nodiscard]] bool
lex_and_parse_file(eden::vector<Lexer::Token>& tokens, Parser::TU& tu, fs::path const& path) noexcept {
  auto const file = tu.source_files.emplace_back(path);
  if (Lexer::tokenizeFile(tokens, file))         { print_errors(file); return true; }
  if (Parser::parseTokens(tu, tokens.to_span())) { print_errors(file); return true; }
  return false;
}

// populates tu and returns whether an error was encountered
[[nodiscard]] bool
lex_and_parse_module(Parser::TU& tu, u16_t module_id) noexcept {
  eden::vector<Lexer::Token> tokens; tokens.reserve(64);
  auto const& directory = globals.module_paths[module_id];
  setup_module(tu, module_id, directory);

#ifndef NDEBUG
  std::println("lex_and_parse_module '{}' with id '{}'", tu.name, module_id);
#endif

  bool has_error = false;
  for (auto const& entry : fs::directory_iterator{directory}) {
    auto const& path = entry.path();
    if (not entry.is_regular_file()) std::print(stderr, "LookOnceMore: Sorry! Submodules not supported yet.\nModule Path: {}", path.string()), std::abort();
    if (path.extension() != ".lom")  continue;

    has_error |= lex_and_parse_file(tokens, tu, path);
    tokens.clear();
  }

  return has_error;
}

// returns whether compilation should stop (due to error or outputing parser)
[[nodiscard]] bool
parse_modules() noexcept {
  bool has_error = false;

  {
    auto& main_tu = globals.tuAt(MAIN_TU_IDX);
    auto const main_path = fs::path{"src/main.lom"};
    if (not fs::exists(main_path)) std::println(stderr, "LookOnceMore: main.lom not found."), std::abort();

#ifndef NDEBUG
    std::println("lexing and parsing main module");
#endif

    setup_module(main_tu.parser_tu, MAIN_TU_IDX, main_path);
    eden::vector<Lexer::Token> main_tokens; main_tokens.reserve(64);
    has_error = lex_and_parse_file(main_tokens, main_tu.parser_tu, main_path);
  }

  auto const num_tus = globals.numTUs();
  for (auto i{MAIN_TU_IDX + 1}; i<num_tus; ++i) {
    auto& ptu = globals.tuAt((u16_t) i).parser_tu;
    if (lex_and_parse_module(ptu, i)) has_error = true;
  }

  if (has_error) { return true; }
  if (Settings::do_output_parser) { print_parsed(globals.getTUs()); return true; }
  return false;
}

// returns whether compilation should stop (due to error, or outputing peep)
[[nodiscard]] bool
peep_modules() noexcept {
  bool has_error = false;
  for (auto& tu : globals.getTUs()) {
    auto const error = PeepIR::lowerToPeep(tu.parser_tu);
    if (error) {
      for (auto file : tu.generic_tu.source_files) print_errors(file);
      has_error = true;
    }
  }

  if (has_error) { return true; }
  if (Settings::do_output_peep) { print_peeped(globals.getTUs()); return true; }
  return false;
}

void compile_modules() noexcept {
  auto const tu_paths = globals.getTUPaths();
  auto const tus = globals.getTUs();

  for (auto [tu, path] : eden::iter_over_both(tus, tu_paths)) {
    auto& peeped = tu.peepir_tu;
    [[maybe_unused]]
    auto const compiled = Backend::codegen( std::move(peeped), path );

#ifdef NO_MEASUREMENT
    if (Settings::do_output_asm)
      compiled->createASMFile(path);
    if (Settings::do_output_llvmir)
      compiled->createIRFile(path);
    if (Settings::do_output_obj)
      path = compiled->createObjectFile(path);
#endif
  }
}

void output_benchmark([[maybe_unused]] auto begin_time) {
#ifdef STAGE_BENCHMARKS
  auto end_time = std::chrono::high_resolution_clock::now();
  std::println("{:>10}, {:>10} | FULL",
    end_time - begin_time,
    std::chrono::duration_cast<std::chrono::microseconds>(end_time - begin_time)
  );
  std::println("{:>10} Full Parsing Duration.", Parser::parsing_durr);
#endif
}

}

void LOM::build() {
  if (not fs::exists(src_path)) throw std::runtime_error("LookOnceMore: src directory not found!");
  auto const begin_time = std::chrono::high_resolution_clock::now();

  std::thread compile_extern;
  if constexpr (not Settings::external_compiler.empty()) {
    if (Settings::do_output_obj)
      compile_extern = std::thread(compileC);
  }

  if (parse_modules()) {
    if (compile_extern.joinable()) compile_extern.join();
    std::quick_exit(1);
  }

  if (peep_modules()) {
    if (compile_extern.joinable()) compile_extern.join();
    std::quick_exit(1);
  }

  compile_modules();
  output_benchmark(begin_time);

  if (compile_extern.joinable()) {
    compile_extern.join();
    globals.module_paths.reserve(globals.numTUs() + globals.num_external_objects);
    for (auto& extern_path : globals.extern_objects_paths)
      globals.module_paths.emplace_back(std::move(extern_path));
  }

#ifdef NO_MEASUREMENT
  if (Settings::do_linking) {
    auto const objs = globals.module_paths.to_span().subspan(MAIN_TU_IDX); // skip the extern object
    Backend::linkObjects(objs);
  }
#endif
}

#ifdef PROFILE
void LOM::reset_state() noexcept {
  globals.~decltype(globals)();
  new(&globals) decltype(globals);
}
#endif