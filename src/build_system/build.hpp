#pragma once
#include "edenlib/macros.hpp"
#include "settings.hpp"
#include <string_view>

namespace LOM {
class Module;

void build();

// returns nullptr if non-existent
edenPure [[nodiscard]] Module* getModule(std::string_view module_name) noexcept;

edenPure edenHot [[nodiscard]] Module& getModule(u16_t module_id) noexcept;
edenPure [[nodiscard]] std::string_view getNameOfModule(u16_t module_id) noexcept;
edenPure [[nodiscard]] Module& getCModule() noexcept;

}