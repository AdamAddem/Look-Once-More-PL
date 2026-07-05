cmake_minimum_required(VERSION 3.31)
project(LookOnceMore)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Werror -Wpedantic -Wnrvo -Wno-maybe-uninitialized")

find_package(LLVM 22.1.4 REQUIRED CONFIG)

include_directories(${PROJECT_SOURCE_DIR}/src)
add_executable(lom
        main.cpp
        src/error.hpp
        src/error.cpp
        src/settings.hpp
        src/settings.cpp
        src/file.hpp

        src/lexing/lex.hpp
        src/lexing/lex.cpp
        src/lexing/tokentype.hpp
        src/lexing/tokenview.cpp

        src/parsing/parse.hpp
        src/parsing/parse.cpp
        src/parsing/ast.hpp
        src/parsing/ast.cpp

        src/semantic_analysis/types.hpp
        src/semantic_analysis/types.cpp
        src/semantic_analysis/symbol_table.hpp

        src/peepir/peepir.hpp
        src/peepir/peepir.cpp

        src/backends/codegen.hpp
        src/backends/codegen.cpp
        src/backends/llvm/tollvm.hpp
        src/backends/llvm/tollvm.cpp

        src/build_system/build.cpp
        src/build_system/build.hpp

        src/semantic_analysis/table_and_module_sync.hpp
)


target_include_directories(lom PRIVATE ${LLVM_INCLUDE_DIRS})
target_link_libraries(lom PRIVATE LLVM)

