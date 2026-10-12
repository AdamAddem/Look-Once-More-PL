#pragma once
#include "edenlib/shortened_macros.hpp"
#include "settings.hpp"
#include <string_view>

namespace LOM {
class Module;

void build();

// returns nullptr if non-existent
PureFN [[nodiscard]] Module* getModule(std::string_view module_name) noexcept;

PureFN HotFN [[nodiscard]] Module& getModule(u16_t module_id) noexcept;
PureFN [[nodiscard]] std::string_view getNameOfModule(u16_t module_id) noexcept;
PureFN [[nodiscard]] Module& getCModule() noexcept;

}