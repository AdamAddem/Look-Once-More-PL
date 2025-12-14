cmake_minimum_required(VERSION 3.31)
project(LookOnceMore)

set(CMAKE_CXX_STANDARD 23)

include_directories(${PROJECT_SOURCE_DIR}/src)

add_executable(LookOnceMore
        main.cpp
        src/lex.cpp
        src/parse.cpp
)
