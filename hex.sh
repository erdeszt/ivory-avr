#!/bin/sh

set -x

TARGET=${1:-Firmware}

avr-objcopy -O ihex -R .eeprom  avr-build/${TARGET} avr-build/${TARGET}.hex