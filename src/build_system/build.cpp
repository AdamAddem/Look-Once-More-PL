#include "build.hpp"
#include "backends/codegen.hpp"
#include "build_system/print.hpp"
#include "lexing/lex.hpp"
#include "modules/table_and_module.hpp"
#include "parsing/parse.hpp"
#include "peepir/peepir.hpp"

#include <chrono>
#include <filesystem>
#include <print>
#include <thread>
using namespace LOM;
namespace fs = std::filesystem;

namespace {
constexpr auto C_MODULE_IDX = 0uz;
constexpr auto MAIN_MODULE_IDX = 1uz;
const fs::path src_path{"src"};
const fs::path extern_path{"extern"};

// using a global struct so state can be easily reset when profiling
// kinda dumb but so is this language
[[maybe_unused]]
struct GlobalStruct {
  std::unordered_map<std::string_view, u16_t> module_map;
  eden::vector<Module> module_list;
  eden::vector<fs::path> module_paths;
  eden::vector<fs::path> extern_objects_paths;
  sz_t num_modules;
  sz_t num_external_objects;

  void init_module_paths() noexcept {
    module_paths.reserve(4);
    module_paths.emplace_back(extern_path);
    module_paths.emplace_back(src_path);
    for (auto const& entry : fs::directory_iterator{src_path}) {
      if (entry.is_directory()) {
        if (is_empty(entry) or entry.path().filename().native()[0] == '.') continue; // jank, ignores directories starting with .
        module_paths.emplace_back(entry.path().stem());
      }
    }
  }

  void init_module_list() noexcept {
    module_list.reserve(module_paths.size());
    module_list.emplace_back(0); module_list.emplace_back(1);
  }

  void init_module_map() noexcept {
    module_map.reserve(module_paths.size());
    module_map.emplace("__C", C_MODULE_IDX);
  }

  GlobalStruct() noexcept {
    init_module_paths();
    init_module_list();
    init_module_map();
    num_modules = module_list.size();
    num_external_objects = extern_objects_paths.size();
  }

#ifdef PROFILE
  ~GlobalStruct() noexcept {
    for (auto& kv : module_map) {
      if (kv.first not_eq std::string_view("__C"))
        delete[] kv.first.data();
    }
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

}

// returns nullptr if not found
[[nodiscard]] Module*
LOM::getModule(std::string_view module_name) {
  auto const iter = globals.module_map.find(module_name);
  if (iter == globals.module_map.end()) return nullptr;
  return &globals.module_list[iter->second];
}

[[nodiscard]] Module&
LOM::getModule(u32_t module_id) {
  assert(module_id < globals.module_list.size());
  return globals.module_list[module_id];
}
[[nodiscard]] Module& LOM::getCModule() noexcept { return globals.module_list[C_MODULE_IDX]; }

namespace {

// populates tu and returns whether an error was encountered
[[nodiscard]] bool
lex_and_parse_module(Parser::TU& tu, u32_t module_id)  {
  eden::vector<Lexer::Token> tokens; tokens.reserve(64);
  auto const& directory = globals.module_paths[module_id];

  { // set up module, this is horrible please change
    assert(not globals.module_map.contains(directory.c_str()));
    auto const n = directory.filename().native().size() + 1; // this is so stupid i hate this language
    auto const module_name_cstr = new char[n]; // TODO: fix purposeful memory leak
    std::strcpy(module_name_cstr, directory.filename().c_str());
    auto const module_name = std::string_view{module_name, n-1};

    globals.module_map.emplace(module_name, module_id);
    tu.name = module_name;
    tu.module_id = module_id;
  }

  bool has_error = false;
  for (auto const& entry : fs::directory_iterator{directory}) {
    auto const& path = entry.path();
    if (not entry.is_regular_file()) throw std::runtime_error( std::format("LookOnceMore: Sorry! Submodules not supported yet.\nModule Path: {}", path.string() ));
    if (path.extension() != ".lom")  continue;

    auto const file = tu.source_files.emplace_back(path);
    if      (Lexer::tokenizeFile(tokens, file))  print_lexer_errors(file),  has_error = true;
    else if (Parser::parseTokens(tu, tokens))    print_parser_errors(file), has_error = true;

    tokens.clear();
  }

  return has_error;
}

// returns whether an error was encountered
[[nodiscard]] bool
parse_modules(eden::vector<Parser::TU>& out) noexcept {
  bool has_error = false;
  for (auto i{MAIN_MODULE_IDX}; i<globals.module_paths.size(); ++i) {
    auto& ptu = out.emplace_back();
    if (lex_and_parse_module(ptu, i)) has_error = true;
  }

  if (has_error) { return true; }
  if (Settings::do_output_parser) { print_parser(out, globals.module_paths); }
  return false;
}

// returns whether an error was encountered
[[nodiscard]] bool
peep_modules(eden::vector<Parser::TU>& parsed_tus, eden::vector<PeepIR::TU>& out) noexcept {
  bool has_error = false;
  for (auto& parsed_tu : parsed_tus) {
    auto& tu = out.emplace_back();
    auto const error = PeepIR::lowerToPeep(tu, std::move(parsed_tu));
    if (error) print_peep_errors(tu), has_error = true;
  }

  if (has_error) { return true; }
  if (Settings::do_output_peep) { print_peep(out, globals.module_paths); }
  return false;
}

void compile_modules(eden::vector<PeepIR::TU>& peeped_tus) noexcept {
  for (auto i{0uz}; i<peeped_tus.size(); ++i) {
    auto& module_path = globals.module_paths[i];
    auto& peeped = peeped_tus[i];

    [[maybe_unused]]
    auto const compiled = Backend::codegen( std::move(peeped), module_path );

#ifdef NO_MEASUREMENT
    if (Settings::do_output_asm)
      compiled->createASMFile(module_path);
    if (Settings::do_output_llvmir)
      compiled->createIRFile(module_path);
    if (Settings::do_output_obj)
      module_path = compiled->createObjectFile(module_path);
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

  eden::vector<Parser::TU> parsed_tus(eden::flags::reserve_initial<>, globals.num_modules);
  if (parse_modules(parsed_tus)) {
    if (compile_extern.joinable()) compile_extern.join();
    std::quick_exit(1);
  }

  eden::vector<PeepIR::TU> peeped_tus(eden::flags::reserve_initial<>, globals.num_modules);
  if (peep_modules(parsed_tus, peeped_tus)) {
    if (compile_extern.joinable()) compile_extern.join();
    std::quick_exit(1);
  }

  compile_modules(peeped_tus);
  output_benchmark(begin_time);

  if (compile_extern.joinable()) {
    compile_extern.join();
    globals.module_paths.reserve(globals.num_modules + globals.num_external_objects);
    for (auto& extern_path : globals.extern_objects_paths)
      globals.module_paths.emplace_back(std::move(extern_path));
  }

#ifdef NO_MEASUREMENT
  if (Settings::do_linking)
    Backend::linkObjects(globals.module_paths);
#endif
}

#ifdef PROFILE
void LOM::reset_state() noexcept {
  globals.~decltype(globals)();
  new(&globals) decltype(globals);
}
#endif