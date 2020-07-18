# ivory-avr

## Dependencies

 * simavr, libsimavr-dev
 * gcc-avr, binutils-avr
 * avrdude
 * gcc
 * cmake
 * stack

## Build

### Main firmware
```
> stack build
> stack exec ivory-avr
> build/main
```

### Assembly files
```
> build/asm
```

### C files
```
> build/c
```

### Simulation
```
> build/sim
```

## Upload
```
> build/upload nano|uno [hex file=Firmware] [port=/dev/ttyS3]
```
