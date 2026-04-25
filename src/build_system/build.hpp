#pragma once
#include <string_view>

namespace LOM {
class Module;

void build();

[[nodiscard]] Module*
getModule(std::string_view);

}