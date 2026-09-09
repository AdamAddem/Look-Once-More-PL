#pragma once
#include "edenlib/typedefs.hpp"
#include <string>

namespace LOM::Settings {

extern bool const& do_output_parser;
extern bool const& do_output_peep;
extern bool const& do_output_validation;
extern bool const& do_output_llvmir;
extern bool const& do_output_asm;
extern bool const& do_output_obj;
extern bool const& do_linking;
extern bool const& do_build;

std::string_view getExecutableName() noexcept;
std::string_view getExternFlags() noexcept;
u8_t getOptimizationLevel() noexcept;
void setArgs(unsigned argc, const char* argv[]);

inline constexpr auto MAX_FUNCTION_PARAMETERS = 8;
inline constexpr auto MAX_TYPELIST_MEMBERS = 8;
inline constexpr auto MAX_STRUCT_MEMBER_VARIABLES = 256;
inline constexpr auto SUBMODULE_SUPPORT = false;
inline constexpr auto MULTITHREADING_SUPPORT = false;
inline constexpr auto PARAMETERS_READONLY = true;
inline constexpr auto ONLY_FUNCTIONS_CALLABLE = true;
inline constexpr auto ONLY_PRIMITIVES_ARITHMETIC = true;

// lol
#ifdef __clang__
inline constexpr std::string_view external_compiler{"clang"};
#elif defined(__GNUC__)
inline constexpr std::string_view external_compiler{"gcc"};
#else
inline constexpr std::string_view external_compiler;
#endif

// #define PROFILE
// #define STAGE_BENCHMARKS

#ifndef PROFILE
#ifndef STAGE_BENCHMARKS
#define NO_MEASUREMENT
#endif
#endif

}