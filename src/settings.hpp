#pragma once
#include "edenlib/typedefs.hpp"
#include <string>

namespace LOM::Settings {

extern bool const& do_output_lexer;
extern bool const& do_output_parser;
extern bool const& do_output_peep;
extern bool const& do_output_validation;
extern bool const& do_output_llvmir;
extern bool const& do_output_asm;
extern bool const& do_output_obj;
extern bool const& do_linking;
extern bool const& do_build;

std::string const& getExecutableName() noexcept;
u8_t getOptimizationLevel() noexcept;

void setArgs(unsigned argc, const char* argv[]);

static constexpr auto MAX_FUNCTION_PARAMETERS = 8;
static constexpr auto MAX_TYPELIST_MEMBERS = 8;
static constexpr auto MAX_STRUCT_MEMBER_VARIABLES = 255;
}