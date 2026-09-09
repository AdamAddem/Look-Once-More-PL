#pragma once
#include "edenlib/macros.hpp"
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
edenPure [[nodiscard]] Module* getModule(std::string_view module_name) noexcept;
edenHot edenPure [[nodiscard]] Module& getModule(u32_t module_id) noexcept;
edenPure [[nodiscard]] Module& getCModule() noexcept;

}