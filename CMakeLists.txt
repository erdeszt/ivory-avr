cmake_minimum_required(VERSION 3.10)

set(CMAKE_C_COMPILER avr-gcc)

set(CMAKE_VERBOSE_MAKEFILE on)
set(CMAKE_C_STANDARD 99)
set(WARNING_FLAGS "-Wall -W -pedantic -Wextra -Wshadow -Wpointer-arith -Wconversion")
# set(AVR_FLAGS "-nostdinc -mmcu=atmega328p -DF_CPU=16000000UL")
set(AVR_FLAGS "-mmcu=atmega328p -DF_CPU=16000000UL")
set(OPTIMIZATION_FLAGS "-Os")
set(DEBUG_FLAGS "-save-temps --verbose-asm")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${WARNING_FLAGS} ${AVR_FLAGS} ${OPTIOMIZATION_FLAGS}")

project(IvoryAVR VERSION 1.0)

include_directories("ivory-out")

add_executable(Firmware ivory-out/firmware.c)
