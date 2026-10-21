#pragma once
#include "settings.hpp"

#include "edenlib/shortened_macros.hpp"
#include "edenlib/vectors/swap_vector.hpp"
#include <string_view>
#include <optional>

namespace LOM {

namespace Parser { struct TU; }
namespace PeepIR { struct TU; }
namespace Symbol { struct Variable; }
class Module;
struct TypeID;

static constexpr auto C_TU_IDX = 0uz;
static constexpr auto MAIN_TU_IDX = 1uz;
static constexpr auto USER_TU_START_IDX = 1uz;

void build();

// call during parsing
// returns nullptr if non-existent
PureFN [[nodiscard]] Module* getModule(std::string_view module_name) noexcept;
PureFN [[nodiscard]] std::optional<u16_t> getModuleID(std::string_view module_name) noexcept;

// call when parsing is completed
PureFN HotFN [[nodiscard]] Module& getModule(u16_t module_id) noexcept;
PureFN [[nodiscard]] std::string_view getNameOfModule(u16_t module_id) noexcept;

// only call these after their respective phases
PureFN [[nodiscard]] Parser::TU& getParserTU(u16_t module_id) noexcept;
PureFN [[nodiscard]] PeepIR::TU& getPeepTU(u16_t module_id) noexcept;

PureFN [[nodiscard]] u16_t getNumTUs() noexcept;

// call during parsing
void addExternDeclaration(
  std::string_view name,
  eden::swap_vector16<Symbol::Variable>&& arguments,
  TypeID const& return_typeID,
  bool is_variadic) noexcept;

}