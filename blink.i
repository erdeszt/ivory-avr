# 1 "src/c/blink.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "src/c/blink.c"
# 1 "/usr/lib/avr/include/avr/io.h" 1 3
# 99 "/usr/lib/avr/include/avr/io.h" 3
# 1 "/usr/lib/avr/include/avr/sfr_defs.h" 1 3
# 126 "/usr/lib/avr/include/avr/sfr_defs.h" 3
# 1 "/usr/lib/avr/include/inttypes.h" 1 3
# 37 "/usr/lib/avr/include/inttypes.h" 3
# 1 "/usr/lib/gcc/avr/5.4.0/include/stdint.h" 1 3 4
# 9 "/usr/lib/gcc/avr/5.4.0/include/stdint.h" 3 4
# 1 "/usr/lib/avr/include/stdint.h" 1 3 4
# 125 "/usr/lib/avr/include/stdint.h" 3 4

# 125 "/usr/lib/avr/include/stdint.h" 3 4
typedef signed int int8_t __attribute__((__mode__(__QI__)));
typedef unsigned int uint8_t __attribute__((__mode__(__QI__)));
typedef signed int int16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int uint16_t __attribute__ ((__mode__ (__HI__)));
typedef signed int int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int uint32_t __attribute__ ((__mode__ (__SI__)));

typedef signed int int64_t __attribute__((__mode__(__DI__)));
typedef unsigned int uint64_t __attribute__((__mode__(__DI__)));
# 146 "/usr/lib/avr/include/stdint.h" 3 4
typedef int16_t intptr_t;




typedef uint16_t uintptr_t;
# 163 "/usr/lib/avr/include/stdint.h" 3 4
typedef int8_t int_least8_t;




typedef uint8_t uint_least8_t;




typedef int16_t int_least16_t;




typedef uint16_t uint_least16_t;




typedef int32_t int_least32_t;




typedef uint32_t uint_least32_t;







typedef int64_t int_least64_t;






typedef uint64_t uint_least64_t;
# 217 "/usr/lib/avr/include/stdint.h" 3 4
typedef int8_t int_fast8_t;




typedef uint8_t uint_fast8_t;




typedef int16_t int_fast16_t;




typedef uint16_t uint_fast16_t;




typedef int32_t int_fast32_t;




typedef uint32_t uint_fast32_t;







typedef int64_t int_fast64_t;






typedef uint64_t uint_fast64_t;
# 277 "/usr/lib/avr/include/stdint.h" 3 4
typedef int64_t intmax_t;




typedef uint64_t uintmax_t;
# 10 "/usr/lib/gcc/avr/5.4.0/include/stdint.h" 2 3 4
# 38 "/usr/lib/avr/include/inttypes.h" 2 3
# 77 "/usr/lib/avr/include/inttypes.h" 3
typedef int32_t int_farptr_t;



typedef uint32_t uint_farptr_t;
# 127 "/usr/lib/avr/include/avr/sfr_defs.h" 2 3
# 100 "/usr/lib/avr/include/avr/io.h" 2 3
# 272 "/usr/lib/avr/include/avr/io.h" 3
# 1 "/usr/lib/avr/include/avr/iom328p.h" 1 3
# 273 "/usr/lib/avr/include/avr/io.h" 2 3
# 627 "/usr/lib/avr/include/avr/io.h" 3
# 1 "/usr/lib/avr/include/avr/portpins.h" 1 3
# 628 "/usr/lib/avr/include/avr/io.h" 2 3

# 1 "/usr/lib/avr/include/avr/common.h" 1 3
# 630 "/usr/lib/avr/include/avr/io.h" 2 3

# 1 "/usr/lib/avr/include/avr/version.h" 1 3
# 632 "/usr/lib/avr/include/avr/io.h" 2 3






# 1 "/usr/lib/avr/include/avr/fuse.h" 1 3
# 239 "/usr/lib/avr/include/avr/fuse.h" 3
typedef struct
{
    unsigned char low;
    unsigned char high;
    unsigned char extended;
} __fuse_t;
# 639 "/usr/lib/avr/include/avr/io.h" 2 3


# 1 "/usr/lib/avr/include/avr/lock.h" 1 3
# 642 "/usr/lib/avr/include/avr/io.h" 2 3
# 2 "src/c/blink.c" 2



# 4 "src/c/blink.c"
void delay_init_8(void)
{
    
# 6 "src/c/blink.c" 3
   (*(volatile uint8_t *)((0x25) + 0x20)) 
# 6 "src/c/blink.c"
          |= 
# 6 "src/c/blink.c" 3
             0
# 6 "src/c/blink.c"
                 ;
}

void delay_init(void)
{
    
# 11 "src/c/blink.c" 3
   (*(volatile uint8_t *)(0x80)) 
# 11 "src/c/blink.c"
          = 0;
    
# 12 "src/c/blink.c" 3
   (*(volatile uint8_t *)(0x81)) 
# 12 "src/c/blink.c"
          = (1 << 
# 12 "src/c/blink.c" 3
                  0
# 12 "src/c/blink.c"
                      );
}

void delay_X_8(uint8_t interval)
{
    static const uint8_t one_ms_ticks = 100;

    for (uint8_t idx = 0; idx < interval; ++idx)
    {
        
# 21 "src/c/blink.c" 3
       (*(volatile uint8_t *)((0x26) + 0x20)) 
# 21 "src/c/blink.c"
             = 0;

        while (
# 23 "src/c/blink.c" 3
              (*(volatile uint8_t *)((0x26) + 0x20)) 
# 23 "src/c/blink.c"
                    < one_ms_ticks);
    }
}

void delay_X(uint16_t interval)
{
    static const uint16_t one_ms_ticks = 16000;

    for (uint16_t idx = 0; idx < interval; ++idx)
    {

        uint16_t current_tick = 0;
        
# 35 "src/c/blink.c" 3
       (*(volatile uint16_t *)(0x84)) 
# 35 "src/c/blink.c"
             = 0;

        while (current_tick < one_ms_ticks)
        {
          current_tick = 
# 39 "src/c/blink.c" 3
                        (*(volatile uint16_t *)(0x84))
# 39 "src/c/blink.c"
                             ;
        }
    }
}

int main_8(void);
int main_16(void);

int main(void)
{
    main_16();

    return 0;
}

int main_8(void)
{
    
# 56 "src/c/blink.c" 3
   (*(volatile uint8_t *)((0x04) + 0x20)) 
# 56 "src/c/blink.c"
        = 1;

    delay_init_8();

    while (1)
    {
        
# 62 "src/c/blink.c" 3
       (*(volatile uint8_t *)((0x05) + 0x20)) 
# 62 "src/c/blink.c"
             = 1;
        delay_X_8(200);
        
# 64 "src/c/blink.c" 3
       (*(volatile uint8_t *)((0x05) + 0x20)) 
# 64 "src/c/blink.c"
             = 0;
        delay_X_8(200);
    }

    return 0;
}

int main_16(void)
{
    
# 73 "src/c/blink.c" 3
   (*(volatile uint8_t *)((0x04) + 0x20)) 
# 73 "src/c/blink.c"
        = 2;

    delay_init();

    while (1)
    {
        
# 79 "src/c/blink.c" 3
       (*(volatile uint8_t *)((0x05) + 0x20)) 
# 79 "src/c/blink.c"
             = 2;
        delay_X(200);
        
# 81 "src/c/blink.c" 3
       (*(volatile uint8_t *)((0x05) + 0x20)) 
# 81 "src/c/blink.c"
             = 0;
        delay_X(200);
    }

    return 0;
}
