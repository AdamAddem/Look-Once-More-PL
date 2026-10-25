#pragma once
#include "edenlib/typedefs.hpp"
#include <string_view>

namespace LOM::Settings {

extern bool const& do_output_ast;
extern bool const& do_output_peepir;
extern bool const& do_output_validation;
extern bool const& do_output_llvmir;
extern bool const& do_output_asm;
extern bool const& do_linking;
extern bool const& do_run;

std::string_view getExecutableName() noexcept;
std::string_view getExternFlags() noexcept;
u8_t getOptimizationLevel() noexcept;
void setArgs(int, const char**);

static constexpr auto SUBMODULE_SUPPORT = false;
static constexpr auto MULTITHREADING_SUPPORT = false;

// lol
#ifdef __clang__
inline constexpr std::string_view external_compiler{"clang"};
#elifdef __GNUC__
inline constexpr std::string_view external_compiler{"gcc"};
#else
inline constexpr std::string_view external_compiler;
#endif

static constexpr bool do_profile = false;


}