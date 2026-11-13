#pragma once

// This exists to solve the cyclic dependency between table_and_module.hpp and types.hpp.
// table_and_module.hpp static_asserts that these properties hold true.
namespace LOM {
static constexpr auto SYMBOL_TABLE_SIZE = 48;
static constexpr auto SYMBOL_TABLE_ALIGNMENT = 8;
static constexpr auto MODULE_SIZE = 112;
static constexpr auto MODULE_ALIGNMENT = 8;
}