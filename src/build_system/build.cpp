#include "build.hpp"
#include "backends/codegen.hpp"
#include "lexing/lex.hpp"
#include "module/table_and_module.hpp"
#include "parsing/parse.hpp"
#include "peepir/peepir.hpp"
#include "print.hpp"

#include "edenlib/multi_iter.hpp"
#include <chrono>
#include <cstdio>
#include <filesystem>
#include <print>
#include <thread>
#include <unordered_map>

using namespace LOM;
namespace fs = std::filesystem;

namespace {

fs::path const src_path{"src"};
fs::path const extern_path{"extern"};

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

class Globals {
  inline static constinit eden::vector<fs::path> extern_objects_paths{};
  inline static constinit eden::vector<fs::path> module_paths{};
  inline static std::unordered_map<std::string_view, u16_t> module_map{};
  inline static constinit eden::owned_span<TUnion> tu_list{};
  inline static constinit std::atomic build_stage{ std::to_underlying(BuildStage::START) };

  static void init() {
    module_paths.reserve(4);
    module_paths.emplace_back_unchecked(extern_path);
    module_paths.emplace_back_unchecked(src_path);
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
    tu_list.reset(tu_array, num_tus);

    std::construct_at<TUnion>(tu_array, C_TU_IDX)->m.name = "__C";
    for (sz_t i{MAIN_TU_IDX}; i<num_tus; ++i) {
      auto const id = (u16_t) i;
      auto* tu = std::construct_at<TUnion>(tu_array + i, id);

      auto const& path = module_paths[i];
      auto const n = path.stem().native().size() + 1; // this is so stupid i hate this language
      auto const module_name_cstr = new char[n]; // TODO: fix purposeful memory leak
      std::strcpy(module_name_cstr, path.filename().c_str());
      auto const module_name = std::string_view{module_name_cstr, n-1};

      module_map.emplace(module_name, id);
      tu->m.name = module_name;
    }
  }

  static void resetAllState() {
    build_stage.store(BuildStage::START);
    for (auto& kv : module_map) {
      if (kv.first not_eq std::string_view("__C")) delete[] kv.first.data();
    }

    for (auto& tu : tu_list) tu.~TUnion();
    ::operator delete(tu_list.data(), tu_list.size() * sizeof(TUnion), align_t{64});
  }

public:
  inline static constinit auto const& module_to_index_map = module_map;

  InlineNodiscardCXPR static BuildStage::Enum getCurrentBuildStage() noexcept { return (BuildStage::Enum) build_stage.load(); }
  InlineNodiscardCXPR static bool isParsingCompleted() noexcept { return build_stage.load() > BuildStage::PARSING; }
  InlineNodiscardCXPR static bool isPeepingCompleted() noexcept { return build_stage.load() > BuildStage::PEEPING; }

  template <BuildStage::Enum NextStage> requires (NextStage != BuildStage::ERROR)
  constexpr static void progressStageTo() noexcept {
    assert(build_stage.load() != BuildStage::DONE);
    assert(NextStage != BuildStage::START);
    assert(build_stage.load() + 1 == NextStage);
    build_stage.fetch_add(1);
  }

  constexpr static void setErrorStage() noexcept {
    assert(build_stage.load() != BuildStage::DONE);
    build_stage.store(BuildStage::ERROR);
  }

  InlineNodiscardCXPR static sz_t numTUs() noexcept { return tu_list.size(); }
  InlineNodiscardCXPR static TUnion& getTU(u16_t tu_id) noexcept { assert(tu_id < numTUs()); return tu_list[tu_id]; }
  InlineNodiscardCXPR static fs::path const& getTUPath(u16_t tu_id) noexcept { return module_paths[tu_id]; }

  InlineNodiscardCXPR static std::span<TUnion> getUserTUs() noexcept { return tu_list.to_span().subspan(MAIN_TU_IDX); } // does not include the CModule TU
  InlineNodiscardCXPR static std::span<fs::path> getUserTUPaths() noexcept { return module_paths.to_span().subspan(MAIN_TU_IDX); } // does not include the CModule path

  friend void LOM::build();
  friend void compileExtern();
};

void compileExtern() {
  if (not fs::exists(extern_path) or is_empty(fs::directory_entry(extern_path))) return;

  std::string command = std::format("(cd build/obj && {} ", Settings::external_compiler);
  switch (Settings::getOptimizationLevel()) {
  case 0: break;
  case 1: command.append(" -O1 "); break;
  case 2: command.append(" -O2 "); break;
  case 3: command.append(" -O3 "); break;
  default: Unreachable("Invalid optimization level.");
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
  auto const fn_typeID = cModule.addFunctionType(arguments.to_span(), return_typeID, is_variadic);
  auto const fn_id = cModule.addFunction(name, std::move(arguments), fn_typeID, true);

  auto& fn = cTU.functions.emplace_back();
  fn.is_public = true;
  fn.file_idx = 0;
  fn.id_in_module = fn_id;
  fn.name_len = name.length();
  fn.name_ptr = name.data();

  // this is rlly dumb, but PeepIR Function holds a type*
  // but this function is run during parsing so we can't materialize fn_typeID just yet
  // ordinarily we'd make cTU a parseTU and later transmute to a peepTU
  // but we'd also have to transmute the functions too, and thats so much work
  // this lets us just store the typeID where the pointer is for now
  // the thread handling compileExtern will undo this later
  fn.type = std::bit_cast<FunctionType const*>(fn_typeID); // TODO: theres gotta be a better way
}

namespace {

[[nodiscard]] bool
parseFile(eden::vector<Lexer::Token>& tokens, Parser::TU& tu, fs::path const& path) {
  auto const file = tu.source_files.emplace_back(path);
  if (Lexer::tokenizeFile(tokens, file)) { printFileErrors(file); return true; }
  if (Parser::parseTokens(tu, tokens.to_span())) { printFileErrors(file); return true; }
  return false;
}

// populates tu and returns whether an error was encountered
[[nodiscard]] bool
parseModule(Parser::TU& tu, u16_t tu_id) {
  eden::vector<Lexer::Token> tokens{eden::flags::reserve_initial<32>};
  auto const& directory = Globals::getTUPath(tu_id);

  bool has_error = false;
  for (auto const& entry : fs::directory_iterator{directory}) {
    auto const& path = entry.path();
    if (entry.is_directory()) throw std::runtime_error(std::format("Sorry! Submodules not supported yet. Module Path: {}", path.string()));
    if (path.extension() != ".lom") continue;

    has_error |= parseFile(tokens, tu, path);
    tokens.clear();
  }

  return has_error;
}

// returns true if compilation should stop (due to error or outputing parser)
[[nodiscard]] bool
parseModules() {
  bool has_error = false;

  // handle main first
  {
    auto& tu = Globals::getTU(MAIN_TU_IDX);
    static auto const path = fs::path{"src/main.lom"};
    if (not fs::exists(path)) throw std::runtime_error("LookOnceMore: main.lom not found.");

    eden::vector<Lexer::Token> tokens{eden::flags::reserve_initial<32>};
    has_error = parseFile(tokens, tu.parser_tu, path);
  }

  auto const num_tus = Globals::numTUs();
  for (auto i{MAIN_TU_IDX + 1}; i<num_tus; ++i) {
    auto& ptu = Globals::getTU((u16_t) i).parser_tu;
    has_error |= parseModule(ptu, i);
  }

  return has_error;
}

// returns true if compilation should stop (due to error or outputing peep)
[[nodiscard]] bool
peepModules() {
  bool has_error = false;
  for (auto& tu : Globals::getUserTUs()) {
    if (not PeepIR::lowerToPeep(tu.parser_tu)) continue;

    has_error = true;
    for (auto const file : tu.m.source_files) printFileErrors(file);
  }

  return has_error;
}

void lowerModules() {

  auto const tus = Globals::getUserTUs();
  auto const tu_paths = Globals::getUserTUPaths();

  for (auto [tu, path] : eden::iter_over(tus, tu_paths)) {
    auto& peeped = tu.peepir_tu;
    [[maybe_unused]]
    auto const compiled = Backend::codegen( peeped, path );

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
#endif
}

}

void LOM::build() {
  auto const begin_time = std::chrono::high_resolution_clock::now();

  std::jthread compile_extern;
  if (Settings::do_output_obj) compile_extern = std::jthread(compileExtern);

  if (not fs::exists(src_path))
    throw std::runtime_error("LookOnceMore: src directory not found!");
  Globals::init();

  Globals::progressStageTo<BuildStage::PARSING>();
  if (parseModules()) [[unlikely]] return;
  if (Settings::do_output_parser) [[unlikely]] { Globals::setErrorStage(); return printParsed(); }

  // adjust the c module stuff mentioned in addExternDeclaration
  {
    auto& cTU = Globals::getTU(C_TU_IDX).peepir_tu;
    auto const& cModule = Globals::getTU(C_TU_IDX).peepir_tu.module;
    for (auto& f : cTU.functions)
      f.type = &cModule.getFunctionType( std::bit_cast<TypeID>(f.type) );
  }

  Globals::progressStageTo<BuildStage::PEEPING>();
  if (peepModules()) [[unlikely]] return;
  if (Settings::do_output_peep) [[unlikely]] { Globals::setErrorStage(); return printPeeped(); }

  if (compile_extern.joinable()) {
    compile_extern.join();
    Globals::module_paths.move_append_range( Globals::extern_objects_paths.to_span() );
  }

  Globals::progressStageTo<BuildStage::LOWERING>();
  lowerModules();
  output_benchmark(begin_time);

  Globals::progressStageTo<BuildStage::LINKING>();
#ifdef NO_MEASUREMENT
  if (Settings::do_linking) {
    auto const objs = Globals::module_paths.to_span().subspan(MAIN_TU_IDX); // skip the extern object
    Backend::linkObjects(objs);
  }
#endif
  Globals::progressStageTo<BuildStage::DONE>();

#ifdef PROFILE
  Globals::resetAllState();
#endif
}