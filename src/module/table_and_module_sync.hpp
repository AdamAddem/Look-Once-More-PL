#pragma once

// This exists to solve the cyclic dependency between symbol_table.hpp and types.hpp.
// symbol_table.hpp static_asserts that these properties hold true.
namespace LOM {
static constexpr sz_t SYMBOL_TABLE_SIZE = 48;
static constexpr sz_t SYMBOL_TABLE_ALIGNMENT = 8;
static constexpr sz_t MODULE_SIZE = 128;
static constexpr sz_t MODULE_ALIGNMENT = 8;
}