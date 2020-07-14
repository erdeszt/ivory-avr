#!/bin/bash

pushd avr-build
avr-gcc -o ./C -mmcu=atmega328p -DF_CPU=16000000UL -fno-exceptions -fno-asynchronous-unwind-tables -Os -save-temps --verbose-asm -Wall ../c/$1.c
popd
./hex.sh C