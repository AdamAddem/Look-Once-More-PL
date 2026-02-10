#pragma once

namespace lom_debug {
    enum class halt_flags {NO_HALT, LEXING, FIRSTPARSE, SECONDPARSE, VALIDATION};

    //set respective phase to see the output of that phase printed to console
#ifdef NDEBUG
    inline constexpr auto stage_to_halt{halt_flags::NO_HALT};
#else
    inline constexpr auto stage_to_halt{halt_flags::SECONDPARSE};
#endif

}
