cmake_minimum_required(VERSION 3.31)
project(LookOnceMore)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Werror -Wpedantic")

include_directories(${PROJECT_SOURCE_DIR}/src)

add_executable(LookOnceMore
        main.cpp
        src/lexing/lex.cpp
        src/lexing/tokenhandler.cpp

        src/parsing/firstparse.cpp
        src/parsing/secondparse.cpp

        src/grammar/statements.cpp
        src/grammar/expressions.cpp
        src/grammar/types.cpp

        src/validation/symbol_table.cpp
        src/validation/ast_validation.cpp
        src/debug_flags.hpp
)

