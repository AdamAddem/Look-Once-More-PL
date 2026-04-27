cmake_minimum_required(VERSION 3.31)
project(LookOnceMore)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Werror -Wpedantic -Wnrvo -Wno-deprecated-declarations")

find_package(LLVM 21.1.8 REQUIRED CONFIG)

include_directories(${PROJECT_SOURCE_DIR}/src)
add_executable(lom
        main.cpp
        src/error.hpp
        src/error.cpp
        src/settings.hpp
        src/settings.cpp

        src/lexing/lex.hpp
        src/lexing/lex.cpp
        src/lexing/tokenview.cpp

        src/parsing/parse.hpp
        src/parsing/parse.cpp
        src/parsing/ast.hpp
        src/parsing/ast.cpp

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
        src/build_system/build.cpp
        src/build_system/build.hpp
)


target_include_directories(lom PRIVATE ${LLVM_INCLUDE_DIRS})
llvm_map_components_to_libnames(LLVM_LIBS ${LLVM_TARGETS_TO_BUILD} core support irreader codegen mc mcparser option)
target_link_libraries(lom PRIVATE ${LLVM_LIBS})

