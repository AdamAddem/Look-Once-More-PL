#pragma once
#include <string_view>

namespace LOM {
class Module;

void reset_compilation_state();
void build();

[[nodiscard]] Module*
getModule(std::string_view);

}