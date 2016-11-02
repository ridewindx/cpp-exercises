cmake_minimum_required(VERSION 3.0.0)
project(ce)  # Cpp-Exercises

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")

include_directories(.)

aux_source_directory(. SRC)
aux_source_directory(tests SRC_TESTS)

add_executable(ce ${SRC} ${SRC_TESTS})
