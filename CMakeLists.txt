cmake_minimum_required(VERSION 3.31)
project(LookOnceMore)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Werror -Wpedantic -O0")

find_package(LLVM 20.1.8 REQUIRED CONFIG)



include_directories(${PROJECT_SOURCE_DIR}/src)
add_executable(LookOnceMore
        main.cpp
        src/error.cpp
        src/error.hpp
        src/arguments.cpp
        src/arguments.hpp

        src/lexing/lex.cpp
        src/lexing/lex.hpp
        src/lexing/tokenhandler.cpp
        src/lexing/tokentype.hpp

        src/parsing/parse.cpp
        src/parsing/parse.hpp

        src/ast/statements.cpp
        src/ast/statements.hpp
        src/ast/expressions.cpp
        src/ast/expressions.hpp
        src/ast/types.cpp
        src/ast/types.hpp

        src/validation/symbol_table.cpp
        src/validation/symbol_table.hpp
        src/validation/ast_validation.cpp
        src/validation/ast_validation.hpp

        src/backends/llvm/tollvm.cpp
        src/backends/llvm/tollvm.hpp

        src/utilities/variant_overload.hpp
)

target_include_directories(LookOnceMore PRIVATE ${LLVM_INCLUDE_DIRS})
llvm_map_components_to_libnames(LLVM_LIBS ${LLVM_TARGETS_TO_BUILD} core support irreader codegen mc mcparser option)
target_link_libraries(LookOnceMore PRIVATE ${LLVM_LIBS})

