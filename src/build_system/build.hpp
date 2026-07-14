#pragma once
#include <string_view>
#include <settings.hpp>

namespace LOM {
class Module;

void build();

#ifdef PROFILE
void reset_state() noexcept;
#endif

inline Module* dunderc_module;

[[nodiscard]] Module* getModule(std::string_view);

}