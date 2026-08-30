#pragma once
#include <vector>
#include <filesystem>
#include <print>
#include "file.hpp"
#include "parsing/parse.hpp"
#include "peepir/peepir.hpp"


namespace LOM {

void print_parser(eden::vector<Parser::TU> const& tus, eden::vector<std::filesystem::path> const& paths);
void print_peep(eden::vector<PeepIR::TU> const& tus, eden::vector<std::filesystem::path> const& paths);

void print_lexer_errors(File file);
void print_parser_errors(File file);
void print_peep_errors(PeepIR::TU const& peep_tu);

}