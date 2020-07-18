cmake_minimum_required(VERSION 3.10)

set(CMAKE_C_COMPILER gcc)

set(CMAKE_VERBOSE_MAKEFILE on)
set(CMAKE_C_STANDARD 99)

set(WARNING_FLAGS "-Wall -W -pedantic -Wextra -Wshadow -Wpointer-arith -Wconversion")
set(OPTIMIZATION_FLAGS "-Os")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${WARNING_FLAGS} ${OPTIMIZATION_FLAGS} ${DEBUG_FLAGS}")

project(Simulation VERSION 1.0)

include_directories("../../src/sim")
include_directories("/usr/include/simavr")

add_executable(Simulation ../../src/sim/main.c)
