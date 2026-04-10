cmake_minimum_required(VERSION 3.31)
project(LookOnceMore)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Werror -Wpedantic")

find_package(LLVM 20.1.8 REQUIRED CONFIG)

include_directories(${PROJECT_SOURCE_DIR}/src)
add_executable(LookOnceMore
        main.cpp
        src/error.hpp
        src/error.cpp
        src/settings.hpp
        src/settings.cpp

        src/lexing/lex.hpp
        src/lexing/lex.cpp
        src/lexing/tokentype.hpp
        src/lexing/tokenview.cpp

        src/parsing/parse.hpp
        src/parsing/parse.cpp

        src/ast/ast.hpp
        src/ast/ast.cpp

        src/semantic_analysis/types.hpp
        src/semantic_analysis/types.cpp
        src/semantic_analysis/symbol_table.hpp
        src/semantic_analysis/symbol_table.cpp

        src/peep_mir/peep_mir.hpp
        src/peep_mir/peep_mir.cpp

        src/backends/codegen.hpp
        src/backends/codegen.cpp
        src/backends/llvm/tollvm.hpp
        src/backends/llvm/tollvm.cpp
)


target_include_directories(LookOnceMore PRIVATE ${LLVM_INCLUDE_DIRS})
llvm_map_components_to_libnames(LLVM_LIBS ${LLVM_TARGETS_TO_BUILD} core support irreader codegen mc mcparser option)
target_link_libraries(LookOnceMore PRIVATE ${LLVM_LIBS})

