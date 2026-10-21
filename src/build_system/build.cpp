#include "build.hpp"
#include "backends/codegen.hpp"
#include "lexing/lex.hpp"
#include "module/table_and_module.hpp"
#include "parsing/parse.hpp"
#include "peepir/peepir.hpp"
#include "print.hpp"

#include "edenlib/owned.hpp"
#include "edenlib/multi_iter.hpp"
#include <chrono>
#include <cstdio>
#include <filesystem>
#include <print>
#include <thread>
#include <unordered_map>

using namespace LOM;
namespace fs = std::filesystem;
using nanoseconds = std::chrono::nanoseconds;
using microseconds = std::chrono::microseconds;
using milliseconds = std::chrono::milliseconds;
using seconds = std::chrono::seconds;

namespace {
InlineNodiscard auto chrono_now() noexcept { return std::chrono::high_resolution_clock::now(); }
InlineNodiscard microseconds to_us(auto time) noexcept { return std::chrono::duration_cast<microseconds>(time); }
InlineNodiscard milliseconds to_ms(auto time) noexcept { return std::chrono::duration_cast<milliseconds>(time); }
InlineNodiscard seconds to_seconds(auto time) noexcept { return std::chrono::duration_cast<seconds>(time); }

fs::path const src_path{"src"};
fs::path const extern_path{"extern"};
fs::path const main_path{"src/main.lom"};

union TUnion {
  GenericTU  m;
  Parser::TU parser_tu;
  PeepIR::TU peepir_tu;

  InlineCXPR explicit TUnion(u16_t tu_id) : parser_tu(tu_id) { m.source_files.reserve(1); }
  InlineCXPR ~TUnion() { peepir_tu.~TU(); }
};
static_assert( sizeof(Parser::TU) == sizeof(PeepIR::TU) );

namespace BuildStage {
enum Enum : sz_t {
  START,
  PARSING,
  PEEPING,
  LOWERING,
  LINKING,
  DONE,
  ERROR,
};
}

struct alignas(64) Benchmark {
  nanoseconds file_open_time{};
  nanoseconds parsing_time{};
  nanoseconds peeping_time{};
  nanoseconds backend_time{};

  InlineNodiscardCXPR nanoseconds sum() const noexcept { return file_open_time + parsing_time + peeping_time + backend_time; }
};

class Globals {
  inline static constinit eden::vector<fs::path> module_paths{};
  inline static std::unordered_map<std::string_view, u16_t> module_map{};
  inline static constinit eden::owned_span<TUnion> tu_list{};
  inline static constinit std::atomic build_stage{ std::to_underlying(BuildStage::START) }; // build stages only used for assertions

  inline static constinit eden::vector<Benchmark> benchmark_times;
  static constexpr auto init_time_bm_ptr = &Benchmark::parsing_time;
  static constexpr auto file_output_bm_ptr = &Benchmark::peeping_time;

  static void init() {
    auto const begin = chrono_now();

    module_paths.reserve(4);
    module_paths.emplace_back_unchecked(extern_path);
    module_paths.emplace_back_unchecked(src_path);
    for (auto const& entry : fs::directory_iterator{src_path}) {
      if (not entry.is_directory()) continue;
      if (is_empty(entry)) continue;
      module_paths.emplace_back( entry.path() );
    }

    auto const num_tus = module_paths.size();
    benchmark_times.resize(num_tus);
    module_map.reserve(num_tus);
    module_map.emplace("__C", C_TU_IDX);

    // we do this malarky instead of using a vector because TUnion's move/copy constructor cannot be implemented in any satisfactory way
    // and vector requires a movable object even if we reserve upfront
    auto const tu_array = std::start_lifetime_as_array<TUnion>(::operator new(num_tus * sizeof(TUnion), align_t{64}), num_tus);
    tu_list.reset(tu_array, num_tus);

    std::construct_at<TUnion>(tu_array, C_TU_IDX)->m.name = "__C";
    for (sz_t i{MAIN_TU_IDX}; i<num_tus; ++i) {
      auto const id = eden::narrow<u16_t>(i);
      auto* tu = std::construct_at<TUnion>(tu_array + i, id);

      auto const& path = module_paths[i];
      {
        auto const full_path_size = path.native().size();
        auto const dir_name_size = path.stem().native().size();
        auto const dir_name_start = full_path_size - dir_name_size;
        auto const module_name = std::string_view(path.native().data(), full_path_size).substr(dir_name_start, dir_name_size);
        module_map.emplace(module_name, id);
        tu->m.name = module_name;
      }

      if (i == MAIN_TU_IDX) {
        assert(fs::exists(main_path));
        tu->m.source_files.emplace_back(main_path);
        continue; // only main.lom considered
      }

      for (auto const& entry : fs::directory_iterator{path}) {
        auto const& entry_path = entry.path();
        if (entry.is_directory()) throw std::runtime_error(std::format("Sorry! Submodules not supported yet. Module Path: {}", path.string()));
        if (entry_path.extension() != ".lom") continue;
        tu->m.source_files.emplace_back(entry_path);
      }
    }
    auto const end = chrono_now();

    // storing here cuz its empty so why not, kinda stupid but whatever
    benchmark_times[C_TU_IDX].*init_time_bm_ptr = end - begin;
  }

  static void resetAllState() {
    module_paths.~vector(); std::construct_at(&module_paths);
    module_map.~unordered_map(); std::construct_at(&module_map);

    for (auto& tu : tu_list) tu.~TUnion();
    ::operator delete(tu_list.data(), tu_list.size() * sizeof(TUnion), align_t{64});

    build_stage.store(BuildStage::START);
    benchmark_times.~vector(); std::construct_at(&benchmark_times);
  }

public:
  static constexpr auto const& module_to_index_map = module_map;

  InlineNodiscardCXPR static BuildStage::Enum getCurrentBuildStage() noexcept {
#ifndef NDEBUG
    return (BuildStage::Enum) build_stage.load(std::memory_order_relaxed);
#else
    return {};
#endif
  }
  InlineNodiscardCXPR static bool isParsingCompleted() noexcept {
#ifndef NDEBUG
    return build_stage.load(std::memory_order_relaxed) > BuildStage::PARSING;
#else
    return true;
#endif
  }
  InlineNodiscardCXPR static bool isPeepingCompleted() noexcept {
#ifndef NDEBUG
    return build_stage.load(std::memory_order_relaxed) > BuildStage::PEEPING;
#else
    return true;
#endif
  }

  template <BuildStage::Enum NextStage> requires (NextStage != BuildStage::ERROR)
  constexpr static void progressStageTo() noexcept {
#ifndef NDEBUG
    assert(build_stage.load() != BuildStage::DONE);
    assert(NextStage != BuildStage::START);
    assert(build_stage.load() + 1 == NextStage);
    build_stage.fetch_add(1, std::memory_order_relaxed);
#endif
  }

  constexpr static void setErrorStage() noexcept {
#ifndef NDEBUG
    assert(build_stage.load() != BuildStage::DONE);
    build_stage.store(BuildStage::ERROR, std::memory_order_relaxed);
#endif
  }

  InlineNodiscardCXPR static sz_t numTUs() noexcept { return tu_list.size(); }
  InlineNodiscardCXPR static TUnion& getTU(u16_t tu_id) noexcept { assert(tu_id < numTUs()); return tu_list[tu_id]; }
  InlineNodiscardCXPR static std::span<TUnion> getUserTUs() noexcept { return tu_list.to_span().subspan(MAIN_TU_IDX); } // does not include the CModule TU

  InlineCXPR static void addParsingBenchmark(nanoseconds t, u16_t module_id) noexcept { benchmark_times[module_id].parsing_time += t; }
  InlineCXPR static void addPeepingBenchmark(nanoseconds t, u16_t module_id) noexcept { benchmark_times[module_id].peeping_time += t; }
  InlineCXPR static void addBackendBenchmark(nanoseconds t, u16_t module_id) noexcept { benchmark_times[module_id].backend_time += t; }
  InlineCXPR static void addFileOutputBenchmark(nanoseconds t) noexcept { benchmark_times[C_TU_IDX].*file_output_bm_ptr += t; }

  InlineNodiscardCXPR static Benchmark getBenchmark(u16_t module_id) noexcept { return benchmark_times[module_id]; }
  InlineNodiscardCXPR static nanoseconds getInitTime() noexcept { return benchmark_times[C_TU_IDX].*init_time_bm_ptr; }
  InlineNodiscardCXPR static nanoseconds getFileOutputTime() noexcept { return benchmark_times[C_TU_IDX].*file_output_bm_ptr; }

  friend void LOM::build();
  friend void compileExtern();
};

void compileExtern() {
  if (not Settings::do_output_obj) return;
  if (not fs::exists(extern_path) or is_empty(fs::directory_entry(extern_path))) return;


  std::string command = "(cd build/obj && ";
  command += Settings::external_compiler;
  command += " -O";
  command.push_back(Settings::getOptimizationLevel() + '0');
  command.push_back(' ');

  command.append("-c ");
  for (auto const& entry : fs::directory_iterator{extern_path}) {
    auto const& path = entry.path();
    if (path.extension() != ".c") continue;
    command.append( std::format("../../{} ", path.native()) );
  }

  command.push_back(')');
  system(command.c_str());
}

}

// returns nullptr if not found
PureFN [[nodiscard]] Module* LOM::getModule(std::string_view module_name) noexcept {
  assert(Globals::getCurrentBuildStage() == BuildStage::PARSING);
  auto const iter = Globals::module_to_index_map.find(module_name);
  if (iter == Globals::module_to_index_map.end()) return nullptr;
  return &Globals::getTU(iter->second).m.module;
}
PureFN [[nodiscard]] std::optional<u16_t> LOM::getModuleID(std::string_view module_name) noexcept {
  assert(Globals::getCurrentBuildStage() == BuildStage::PARSING);
  auto const iter = Globals::module_to_index_map.find(module_name);
  if (iter == Globals::module_to_index_map.end()) return std::nullopt;
  return iter->second;
}
PureFN HotFN [[nodiscard]] Module& LOM::getModule(u16_t module_id) noexcept {
  assert(Globals::isParsingCompleted());
  return Globals::getTU(module_id).m.module;
}
PureFN [[nodiscard]] std::string_view LOM::getNameOfModule(u16_t module_id) noexcept {
  assert(Globals::isParsingCompleted());
  return Globals::getTU(module_id).m.name;
}
PureFN [[nodiscard]] Parser::TU& LOM::getParserTU(u16_t module_id) noexcept {
  assert(Globals::isParsingCompleted());
  return Globals::getTU(module_id).parser_tu;
}
PureFN [[nodiscard]] PeepIR::TU& LOM::getPeepTU(u16_t module_id) noexcept {
  assert(Globals::isPeepingCompleted());
  return Globals::getTU(module_id).peepir_tu;
}
PureFN [[nodiscard]] u16_t LOM::getNumTUs() noexcept { return Globals::numTUs(); }

static_assert(not Settings::MULTITHREADING_SUPPORT);
void LOM::addExternDeclaration(
  std::string_view name,
  eden::swap_vector16<Symbol::Variable>&& arguments,
  TypeID const& return_typeID,
  bool is_variadic) noexcept {
  assert(Globals::getCurrentBuildStage() == BuildStage::PARSING);

  auto& cTU = Globals::getTU(C_TU_IDX).peepir_tu;
  auto& cModule = cTU.module;
  cModule.addFunction(name, std::move(arguments), return_typeID, true, is_variadic);
}

namespace {

// populates tu and returns whether an error was encountered
[[nodiscard]] bool
parseTU(Parser::TU& tu) {
  eden::vector<Lexer::Token> tokens{eden::flags::reserve_initial<32>};
  bool has_error = false;
  for (auto const file : tu.source_files) {
    if (Lexer::tokenizeFile(tokens, file)) { printFileErrors(file); has_error = true; }
    else if (Parser::parseTokens(tu, tokens.to_span())) { printFileErrors(file); has_error = true; }
    tokens.clear();
  }

  return has_error;
}

// returns true if compilation should stop (due to error or outputing parser)
[[nodiscard]] bool
parseTUs() {
  bool has_error = false;

  for (sz_t i{MAIN_TU_IDX}; i<Globals::numTUs(); ++i) {
    auto const begin_time = chrono_now();
    auto& parser_tu = Globals::getTU(eden::narrow<u16_t>(i)).parser_tu;
    has_error |= parseTU(parser_tu);
    auto const end_time = chrono_now();
    Globals::addParsingBenchmark(end_time - begin_time, parser_tu.module.getID());
  }

  return has_error;
}

// returns true if compilation should stop (due to error or outputing peep)
[[nodiscard]] bool
peepTUs() {
  bool has_error = false;
  for (auto& tu : Globals::getUserTUs()) {
    auto const begin_time = chrono_now();
    if (PeepIR::lowerToPeep(tu.parser_tu)) { has_error = true; for (auto const file : tu.m.source_files) printFileErrors(file); }
    auto const end_time = chrono_now();
    Globals::addPeepingBenchmark(end_time - begin_time, tu.m.module.getID());
  }

  return has_error;
}

void lowerTUs() {
  for (auto& tu : Globals::getUserTUs()) {
    auto const begin = chrono_now();
    auto const compiled = Backend::codegen(tu.peepir_tu);
    auto const end = chrono_now();
    Globals::addBackendBenchmark(end - begin, tu.m.module.getID());

    if constexpr(not Settings::do_profile) {
      auto const file_output_begin = chrono_now();
      if (Settings::do_output_asm)    compiled->createASMFile();
      if (Settings::do_output_llvmir) compiled->createIRFile();
      if (Settings::do_output_obj)    compiled->createObjectFile();
      auto const file_output_end = chrono_now();
      Globals::addFileOutputBenchmark(file_output_end - file_output_begin);
    }
  }
}

void outputBenchmarks(nanoseconds linking_time) noexcept {
  if constexpr (Settings::do_profile) return;

  Benchmark totals{};
  auto print_and_sum = [] (auto time_ptr) {
    nanoseconds total{};
    for (auto i{MAIN_TU_IDX}; i<Globals::numTUs(); ++i) {
      auto const ns = Globals::getBenchmark(eden::narrow<u16_t>(i)).*time_ptr;
      auto const us = to_us(ns);
      std::println("  {}\ttook\t{}, {}", getNameOfModule(i), us, ns);
      total += ns;
    }
    return total;
  };

  std::println("Parser:");  totals.parsing_time = print_and_sum(&Benchmark::parsing_time); std::println();
  std::println("Peeper:");  totals.peeping_time = print_and_sum(&Benchmark::peeping_time); std::println();
  std::println("Backend:"); totals.backend_time = print_and_sum(&Benchmark::backend_time); std::println();

  auto const init_time = Globals::getInitTime();
  auto const file_output_time = Globals::getFileOutputTime();
  auto const compile_total = totals.sum() + init_time;

  std::println("Initialization: {}, {} +", to_ms(init_time), to_us(init_time));
  std::println("Parsing total:  {}, {} +", to_ms(totals.parsing_time), to_us(totals.parsing_time));
  std::println("Peeping total:  {}, {} +", to_ms(totals.peeping_time), to_us(totals.peeping_time));
  std::println("Backend total:  {}, {} =", to_ms(totals.backend_time), to_us(totals.backend_time));
  std::println("                {}, {}\n", to_ms(compile_total), to_us(compile_total));

  std::println("Object File Output: {}, {}", to_ms(file_output_time), to_us(file_output_time));
  std::println("Linking: {}, {}", to_ms(linking_time), to_us(linking_time));

  auto const total = init_time + file_output_time + compile_total + linking_time;
  std::println("Total: {}, {}, {}", to_seconds(total), to_ms(total), to_us(total));
}

}

void LOM::build() {
  std::jthread compile_extern(compileExtern);

  if (not fs::exists(src_path))  throw std::runtime_error("src directory not found!");
  if (not fs::exists(main_path)) throw std::runtime_error("main.lom not found.");
  fs::remove_all("build/obj/");
  Globals::init();

  Globals::progressStageTo<BuildStage::PARSING>();
  {
    if (parseTUs()) [[unlikely]] return;
    if (Settings::do_output_parser) [[unlikely]] { Globals::setErrorStage(); return printParsed(); }
  }

  Globals::progressStageTo<BuildStage::PEEPING>();
  {
    if (peepTUs()) [[unlikely]] return;
    if (Settings::do_output_peep) [[unlikely]] { Globals::setErrorStage(); return printPeeped(); }
  }

  Globals::progressStageTo<BuildStage::LOWERING>();
  lowerTUs();

  Globals::progressStageTo<BuildStage::LINKING>();
  if constexpr(not Settings::do_profile) {
    if (Settings::do_linking) {
      auto const begin = chrono_now();

      auto const& main_tu = Globals::getTU(MAIN_TU_IDX);
      auto const main_fn = main_tu.m.module.getFunction("main");
      if (main_fn == nullptr) { reportError(main_tu.m.source_files[0], 0, 0, "Main function not defined."); return; }
      if (not main_fn->is_public) { reportError(main_tu.m.source_files[0], 0, 0, "Main function not public."); return; }

      if (compile_extern.joinable()) compile_extern.join();
      Backend::linkObjects();
      outputBenchmarks(chrono_now() - begin);
    }
  }

  Globals::progressStageTo<BuildStage::DONE>();

  if constexpr(Settings::do_profile)
    Globals::resetAllState();
}