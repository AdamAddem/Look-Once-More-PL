#pragma once

// This exists to solve the cyclic dependency between symbol_table.hpp and types.hpp.
// symbol_table.hpp static_asserts that these properties hold true.
namespace LOM {
inline constexpr auto SYMBOL_TABLE_SIZE = 48uz;
inline constexpr auto SYMBOL_TABLE_ALIGNMENT = 8uz;
inline constexpr auto MODULE_SIZE = 56uz;
inline constexpr auto MODULE_ALIGNMENT = 8uz;
}