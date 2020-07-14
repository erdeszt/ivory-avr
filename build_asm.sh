#!/bin/sh

avr-as -o avr-build/Asm asm/$1.asm
./hex.sh Asm