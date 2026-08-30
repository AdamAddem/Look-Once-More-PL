#pragma once
#include "settings.hpp"
#include <string_view>

namespace LOM {
class Module;
class Type;

void build();

#ifdef PROFILE
void reset_state() noexcept;
#endif

// returns nullptr if non-existent
[[nodiscard]] Module* getModule(std::string_view module_name) noexcept;
[[nodiscard]] Module& getModule(u32_t module_id) noexcept;
[[nodiscard]] Module& getCModule() noexcept;

}