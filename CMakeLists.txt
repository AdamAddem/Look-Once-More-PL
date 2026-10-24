cmake_minimum_required(VERSION 3.31)
project(LookOnceMore LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(THREADS_PREFER_PTHREAD_FLAG TRUE)
find_package(Threads REQUIRED)

find_package(LLVM 22.1.4 REQUIRED CONFIG)

add_executable(lom
  src/main.cpp
  src/error.cpp
  src/settings.cpp
  src/print.cpp
  src/lexing/lex.cpp
  src/parsing/parse.cpp
  src/module/types.cpp
  src/peepir/peepir.cpp
  src/backends/codegen.cpp
  src/backends/llvm/tollvm.cpp
  src/build_system/build.cpp
)

target_include_directories(lom PRIVATE
  "${CMAKE_CURRENT_SOURCE_DIR}/src"
)
target_include_directories(lom SYSTEM PRIVATE
  ${LLVM_INCLUDE_DIRS}
)

separate_arguments(LLVM_DEFINITIONS_LIST
  NATIVE_COMMAND "${LLVM_DEFINITIONS}"
)
target_compile_definitions(lom PRIVATE
  ${LLVM_DEFINITIONS_LIST}
)

target_link_libraries(lom PRIVATE
  LLVM
  Threads::Threads
)

if(CMAKE_CXX_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  target_compile_options(lom PRIVATE
    "$<$<CONFIG:Release>:-O3;-march=native;-flto>"
  )

  target_link_options(lom PRIVATE
    "$<$<CONFIG:Release>:-O3;-march=native;-flto>"
  )

  target_compile_options(lom PRIVATE
    -Wall
    -Wextra
    -Wpedantic
    -Werror
  )
endif()
