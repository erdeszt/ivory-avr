cmake_minimum_required(VERSION 3.10)

set(CMAKE_C_COMPILER gcc)

set(CMAKE_VERBOSE_MAKEFILE on)
set(CMAKE_C_STANDARD 99)

set(SRC "../../src/sim")
set(SIMAVR_SRC "../../../simavr/simavr/sim")
set(SIMAVR_CORES "../../../simavr/simavr/cores")

set(WARNING_FLAGS "-Wall -W -pedantic -Wextra -Wshadow -Wpointer-arith -Wconversion")
set(OPTIMIZATION_FLAGS "-Os")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${WARNING_FLAGS} ${OPTIMIZATION_FLAGS} ${DEBUG_FLAGS}")

set(CONFIG_MEGA328 1)

project(Simulation VERSION 1.0)

include_directories("../../src/sim")

include_directories("../../../simavr/simavr/sim")
include_directories("../../../simavr/simavr/cores")
include_directories("/usr/include/simavr")

# link_directories("../../../simavr/simavr/obj-x86_64-linux-gnu")

# find_library(SIM_AVR
#     NAMES sim_elf
#     PATHS "../../../simavr/simavr/obj-x86_64-linux-gnu"
# )

# add_library(SIM_AVR STATIC)

add_executable(Simulation
    "${SRC}/main.c"
    "${SIMAVR_CORES}/sim_mega328.c"
    "${SIMAVR_CORES}/sim_megax8.c"
    "${SIMAVR_CORES}/sim_megax4.c"
    "${SIMAVR_CORES}/sim_megax.c"
    "${SIMAVR_CORES}/sim_megaxm1.c"
    "${SIMAVR_CORES}/sim_mega128.c"
    "${SIMAVR_CORES}/sim_mega128.c"
    "${SIMAVR_CORES}/sim_mega1280.c"
    "${SIMAVR_CORES}/sim_mega1281.c"
    "${SIMAVR_CORES}/sim_mega1284.c"
    "${SIMAVR_CORES}/sim_mega128rfa1.c"
    "${SIMAVR_CORES}/sim_mega128rfr2.c"
    "${SIMAVR_CORES}/sim_mega16.c"
    "${SIMAVR_CORES}/sim_mega164.c"
    "${SIMAVR_CORES}/sim_mega168.c"
    "${SIMAVR_CORES}/sim_mega16m1.c"
    "${SIMAVR_CORES}/sim_mega2560.c"
    "${SIMAVR_CORES}/sim_mega32.c"
    "${SIMAVR_CORES}/sim_mega324.c"
    "${SIMAVR_CORES}/sim_mega324a.c"
    "${SIMAVR_CORES}/sim_mega48.c"
    "${SIMAVR_CORES}/sim_mega644.c"
    "${SIMAVR_CORES}/sim_mega8.c"
    "${SIMAVR_CORES}/sim_mega88.c"
    "${SIMAVR_CORES}/sim_tinyx4.c"
    "${SIMAVR_CORES}/sim_tinyx5.c"
    "${SIMAVR_CORES}/sim_tiny13.c"
    "${SIMAVR_CORES}/sim_tiny2313.c"
    "${SIMAVR_CORES}/sim_tiny24.c"
    "${SIMAVR_CORES}/sim_tiny25.c"
    "${SIMAVR_CORES}/sim_tiny44.c"
    "${SIMAVR_CORES}/sim_tiny45.c"
    "${SIMAVR_CORES}/sim_tiny84.c"
    "${SIMAVR_CORES}/sim_tiny85.c"
    "${SIMAVR_SRC}/avr_acomp.c"
    "${SIMAVR_SRC}/avr_adc.c"
    "${SIMAVR_SRC}/avr_eeprom.c"
    "${SIMAVR_SRC}/avr_extint.c"
    "${SIMAVR_SRC}/avr_flash.c"
    "${SIMAVR_SRC}/avr_ioport.c"
    "${SIMAVR_SRC}/avr_lin.c"
    "${SIMAVR_SRC}/avr_spi.c"
    "${SIMAVR_SRC}/avr_timer.c"
    "${SIMAVR_SRC}/avr_twi.c"
    "${SIMAVR_SRC}/avr_uart.c"
    "${SIMAVR_SRC}/avr_watchdog.c"
    "${SIMAVR_SRC}/sim_avr.c"
    "${SIMAVR_SRC}/sim_core.c"
    "${SIMAVR_SRC}/sim_cmds.c"
    "${SIMAVR_SRC}/sim_cycle_timers.c"
    "${SIMAVR_SRC}/sim_elf.c"
    "${SIMAVR_SRC}/sim_gdb.c"
    "${SIMAVR_SRC}/sim_hex.c"
    "${SIMAVR_SRC}/sim_interrupts.c"
    "${SIMAVR_SRC}/sim_io.c"
    "${SIMAVR_SRC}/sim_irq.c"
    "${SIMAVR_SRC}/sim_utils.c"
    "${SIMAVR_SRC}/sim_vcd_file.c"
)

target_link_libraries(Simulation elf)
