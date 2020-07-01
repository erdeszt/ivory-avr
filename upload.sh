#!/bin/sh

set -x

SPEED=""
PORT=${2:-ttyS3}
TARGET=${3:-Firmware}

if [ -z "$1" ]; then
  echo "Provide a model: uno|nano"
  exit 1
else
  if [ "$1" = "uno" ]; then
    SPEED="115200"
  elif [ "$1" = "nano" ]; then
    SPEED="57600"
  else
    echo "Invalid model, choose from: uno|nano"
    exit 2
  fi
fi

avrdude -Cavrdude.conf -v -patmega328p -carduino -P/dev/ttyS3 -b${SPEED} -D -Uflash:w:avr-build/${TARGET}.hex:i