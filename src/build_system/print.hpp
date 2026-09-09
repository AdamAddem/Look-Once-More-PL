#pragma once
#include <vector>
#include <filesystem>
#include <print>
#include "file.hpp"
#include "parsing/parse.hpp"
#include "peepir/peepir.hpp"


namespace LOM {

edenNoInlineCold void print_parser(eden::vector<Parser::TU> const& tus, eden::vector<std::filesystem::path> const& paths);
edenNoInlineCold void print_peep(eden::vector<PeepIR::TU> const& tus, eden::vector<std::filesystem::path> const& paths);

edenNoInlineCold void print_lexer_errors(File file);
edenNoInlineCold void print_parser_errors(File file);
edenNoInlineCold void print_peep_errors(PeepIR::TU const& peep_tu);

}