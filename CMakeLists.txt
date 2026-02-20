cmake_minimum_required(VERSION 3.31)
project(LookOnceMore)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Werror -Wpedantic")

include_directories(${PROJECT_SOURCE_DIR}/src)

add_executable(LookOnceMore
        main.cpp
        src/debug_flags.hpp

        src/lexing/lex.cpp
        src/lexing/lex.hpp
        src/lexing/tokenhandler.cpp
        src/lexing/tokentype.hpp

        src/parsing/firstparse.cpp
        src/parsing/firstparse.hpp
        src/parsing/secondparse.cpp
        src/parsing/secondparse.hpp

        src/grammar/statements.cpp
        src/grammar/statements.hpp
        src/grammar/expressions.cpp
        src/grammar/expressions.hpp
        src/grammar/types.cpp
        src/grammar/types.hpp

        src/validation/symbol_table.cpp
        src/validation/symbol_table.hpp
        src/validation/ast_validation.cpp
        src/validation/ast_validation.hpp

)

add_executable(tokentype_genrator src/lexing/tokentype_generator.cpp)
