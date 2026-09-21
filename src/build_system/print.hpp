#pragma once
#include <vector>
#include <filesystem>
#include <print>
#include <span>
#include "file.hpp"
#include "parsing/parse.hpp"
#include "peepir/peepir.hpp"


namespace LOM {

edenNoInlineCold void print_parser(std::span<Parser::TU const> tus, std::span<std::filesystem::path const> paths);
edenNoInlineCold void print_peep(std::span<PeepIR::TU const> tus, std::span<std::filesystem::path const> paths);

edenNoInlineCold void print_lexer_errors(File file);
edenNoInlineCold void print_parser_errors(File file);
edenNoInlineCold void print_peep_errors(PeepIR::TU const& peep_tu);

}