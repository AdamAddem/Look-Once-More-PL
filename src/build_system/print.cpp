#include <vector>
#include <filesystem>
#include <print>
#include "parsing/parse.hpp"
#include "peepir/peepir.hpp"
namespace fs = std::filesystem;

namespace LOM {

eden_noinline_cold void
print_parser(eden::vector<Parser::TU> const& tus, eden::vector<fs::path> const& paths) {
  assert(tus.size() == paths.size()); assert(Settings::do_output_parser);
  for (auto i{0uz}; i<tus.size(); ++i) {
    std::println("\n--- Parser Output --- {}", paths[i].native());
    Parser::printTU(tus[i]);
    std::println("\n--- Parser Output ---");
  }
}

eden_noinline_cold void
print_peep(eden::vector<PeepIR::TU> const& tus, eden::vector<fs::path> const& paths) {
  assert(tus.size() == paths.size()); assert(Settings::do_output_peep);
  for (auto i{0uz}; i<tus.size(); ++i) {
    std::println("\n--- Peep Output --- {}", paths[i].native());
    PeepIR::printPeep(tus[i]);
    std::println("\n--- Peep Output ---");
  }
}

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