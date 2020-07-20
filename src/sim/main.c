#include <stdlib.h>
#include <stdio.h>
#include <libgen.h>
#include <pthread.h>

#include <sim_avr.h>
#include <avr_ioport.h>
#include <avr_twi.h>
#include <sim_elf.h>
#include <sim_gdb.h>
#include <sim_vcd_file.h>

avr_t *avr = NULL;
avr_vcd_t vcd_file;

int main(void)
{
    elf_firmware_t f;
    const char *file_name = "build/out/Firmware";

    elf_read_firmware(file_name, &f);

    printf("firmware: %s f=%d mmcu=%s\n", file_name, (int)f.frequency, f.mmcu);


    printf("YO\n");

    return 0;
}