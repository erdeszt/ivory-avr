#!/bin/sh

set -x

SPEED=""
TARGET=${2:-Firmware}
PORT=${3:-ttyS3}

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

avrdude -Cavrdude.conf -v -patmega328p -carduino -P/dev/${PORT} -b${SPEED} -D -Uflash:w:avr-build/${TARGET}.hex:i
