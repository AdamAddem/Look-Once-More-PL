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

class Globals {
  inline static constinit eden::vector<fs::path> extern_objects_paths{};
  inline static constinit eden::vector<fs::path> module_paths{};
  inline static std::unordered_map<std::string_view, u16_t> module_map{};
  inline static constinit eden::owned_span<TUnion> tu_list{};

  static void init() {
    module_paths.reserve(4);
    module_paths.emplace_back(extern_path);
    module_paths.emplace_back(src_path);
    for (auto const& entry : fs::directory_iterator{src_path}) {
      if (not entry.is_directory()) continue;
      if (is_empty(entry)) continue;

      module_paths.emplace_back( entry.path() );
    }
    auto const num_tus = module_paths.size();
    module_map.reserve(num_tus);
    module_map.emplace("__C", C_TU_IDX);

    // we do this malarky instead of using a vector because TUnion's move/copy constructor cannot be implemented in any satisfactory way
    // and vector requires a movable object even if we reserve upfront
    auto const tu_array = std::start_lifetime_as_array<TUnion>(::operator new(num_tus * sizeof(TUnion), align_t{64}), num_tus);
    tu_list.reset( tu_array, num_tus );

    std::construct_at<TUnion>(tu_array, C_TU_IDX)->generic_tu.name = "__C";
    for (sz_t i{MAIN_TU_IDX}; i<num_tus; ++i) {
      auto const id = (u16_t) i;
      auto* tu = std::construct_at<TUnion>(tu_array + i, id);

      auto const& path = module_paths[i];
      auto const n = path.stem().native().size() + 1; // this is so stupid i hate this language
      auto const module_name_cstr = new char[n]; // TODO: fix purposeful memory leak
      std::strcpy(module_name_cstr, path.filename().c_str());
      auto const module_name = std::string_view{module_name_cstr, n-1};

      module_map.emplace(module_name, id);
      tu->generic_tu.name = module_name;
    }
  }

public:

  edenInlineNodiscardCXPR static sz_t numTUs() noexcept { return tu_list.size(); }
  edenInlineNodiscardCXPR static TUnion& getTU(u16_t tu_id) noexcept { return tu_list[tu_id]; }
  edenInlineNodiscardCXPR static fs::path const& getTUPath(u16_t tu_id) noexcept { return module_paths[tu_id]; }

  edenInlineNodiscardCXPR static std::span<TUnion> getUserTUs() noexcept { return tu_list.to_span().subspan(MAIN_TU_IDX); } // does not include the CModule TU
  edenInlineNodiscardCXPR static std::span<fs::path> getUserTUPaths() noexcept { return module_paths.to_span().subspan(MAIN_TU_IDX); } // does not include the CModule path

  friend void LOM::build();

  friend Module* LOM::getModule(std::string_view) noexcept;
  friend Module& LOM::getModule(u16_t module_id) noexcept;
  friend std::string_view LOM::getNameOfModule(u16_t module_id) noexcept;

  friend void compileExtern();
};

// LOL
void compileExtern() {
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
  for (auto const& path : fs::directory_iterator{extern_path}) {
    if (path.path().extension() != ".c") continue;
    Globals::extern_objects_paths.emplace_back(
      path.path().filename()).replace_extension(obj_extension);
    command.append(
      std::format("../../{} ", path.path().native())
      );
  }

  command.push_back(')');
  system(command.c_str());
}

edenNoInlineCold void
print_parsed() noexcept {
  for (auto const& tu : Globals::getUserTUs()) {
    std::println("\n--- Parser Output --- {}", tu.generic_tu.name);
    Parser::printTU(tu.parser_tu);
    std::println("\n--- Parser Output ---");
  }
}

edenNoInlineCold void
print_peeped() noexcept {
  for (auto const& tu : Globals::getUserTUs()) {
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

edenHot edenPure [[nodiscard]] Module&
LOM::getModule(u16_t module_id) noexcept {
  assert(module_id < Globals::numTUs());
  return Globals::getTU(module_id).generic_tu.module;
}

// returns nullptr if not found
edenPure [[nodiscard]] Module*
LOM::getModule(std::string_view module_name) noexcept {
  auto const iter = Globals::module_map.find(module_name);
  if (iter == Globals::module_map.end()) return nullptr;
  return &getModule(iter->second);
}

edenPure [[nodiscard]] std::string_view
LOM::getNameOfModule(u16_t module_id) noexcept {
  assert(module_id < Globals::numTUs());
  return Globals::getTU(module_id).generic_tu.name;
}

edenPure [[nodiscard]] Module& LOM::getCModule() noexcept { return getModule(C_TU_IDX); }

namespace {

[[nodiscard]] bool
parse_file(eden::vector<Lexer::Token>& tokens, Parser::TU& tu, fs::path const& path) {
  auto const file = tu.source_files.emplace_back(path);
  if (Lexer::tokenizeFile(tokens, file))         { print_errors(file); return true; }
  if (Parser::parseTokens(tu, tokens.to_span())) { print_errors(file); return true; }
  return false;
}

// populates tu and returns whether an error was encountered
[[nodiscard]] bool
parse_module(Parser::TU& tu, u16_t tu_id) {
  eden::vector<Lexer::Token> tokens{eden::flags::reserve_initial<>, 32};
  auto const& directory = Globals::getTUPath(tu_id);

  bool has_error = false;
  for (auto const& entry : fs::directory_iterator{directory}) {
    auto const& path = entry.path();
    if (not entry.is_regular_file()) throw std::runtime_error(std::format("Sorry! Submodules not supported yet. Module Path: {}", path.string()));
    if (path.extension() != ".lom")  continue;

    has_error |= parse_file(tokens, tu, path);
    tokens.clear();
  }

  return has_error;
}

// returns whether compilation should stop (due to error or outputing parser)
[[nodiscard]] bool
parse_modules() {
  bool has_error = false;

  // handle main first
  {
    auto& tu = Globals::getTU(MAIN_TU_IDX);
    static auto const path = fs::path{"src/main.lom"};
    if (not fs::exists(path)) std::println(stderr, "LookOnceMore: main.lom not found."), std::abort();

    eden::vector<Lexer::Token> tokens{eden::flags::reserve_initial<>, 32};
    has_error = parse_file(tokens, tu.parser_tu, path);
  }

  auto const num_tus = Globals::numTUs();
  for (auto i{MAIN_TU_IDX + 1}; i<num_tus; ++i) {
    auto& ptu = Globals::getTU((u16_t) i).parser_tu;
    if (parse_module(ptu, i)) has_error = true;
  }

  if (has_error) { return true; }
  if (Settings::do_output_parser) { print_parsed(); return true; }
  return false;
}

// returns whether compilation should stop (due to error or outputing peep)
[[nodiscard]] bool
peep_modules() {
  bool has_error = false;
  for (auto& tu : Globals::getUserTUs()) {
    if (not PeepIR::lowerToPeep(tu.parser_tu)) continue;

    has_error = true;
    for (auto const file : tu.generic_tu.source_files) print_errors(file);
  }

  if (has_error) return true;
  if (Settings::do_output_peep) { print_peeped(); return true; }
  return false;
}

void compile_modules() {
  auto const tu_paths = Globals::getUserTUPaths();
  auto const tus = Globals::getUserTUs();

  for (auto [tu, path] : eden::iter_over_both(tus, tu_paths)) {
    auto& peeped = tu.peepir_tu;
    [[maybe_unused]]
    auto const compiled = Backend::codegen( std::move(peeped), path );

#ifdef NO_MEASUREMENT
    if (Settings::do_output_asm)    compiled->createASMFile(path);
    if (Settings::do_output_llvmir) compiled->createIRFile(path);
    if (Settings::do_output_obj)    path = compiled->createObjectFile(path);
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
  auto const begin_time = std::chrono::high_resolution_clock::now();

  std::jthread compile_extern;
  if constexpr (not Settings::external_compiler.empty()) {
    if (Settings::do_output_obj) compile_extern = std::jthread(compileExtern);
  }

  if (not fs::exists(src_path))
    throw std::runtime_error("LookOnceMore: src directory not found!");

  Globals::init();

  if (parse_modules()) return;
  if (peep_modules())  return;

  compile_modules();
  output_benchmark(begin_time);

  if (compile_extern.joinable()) {
    compile_extern.join();
    Globals::module_paths.reserve(Globals::numTUs() + Globals::extern_objects_paths.size());
    for (auto& extern_path : Globals::extern_objects_paths)
      Globals::module_paths.emplace_back_unchecked(std::move(extern_path));
  }

#ifdef NO_MEASUREMENT
  if (Settings::do_linking) {
    auto const objs = Globals::module_paths.to_span().subspan(MAIN_TU_IDX); // skip the extern object
    Backend::linkObjects(objs);
  }
#endif


#ifdef PROFILE
  for (auto& kv : Globals::module_map) {
    if (kv.first not_eq std::string_view("__C")) delete[] kv.first.data();
  }

  auto& tu_list = Globals::tu_list;
  for (auto& tu : tu_list) tu.~TUnion();
  ::operator delete(tu_list.data(), tu_list.size() * sizeof(TUnion), align_t{64});
#endif
}