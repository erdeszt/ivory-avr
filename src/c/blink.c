#include <avr/io.h>
#include <stdint.h>

void delay_init_8(void)
{
    TCCR0B |= CS00;
}

void delay_init(void)
{
    TCCR1A = 0;
    TCCR1B = (1 << CS10);
}

void delay_X_8(uint8_t interval)
{
    static const uint8_t one_ms_ticks = 100;

    for (uint8_t idx = 0; idx < interval; ++idx)
    {
        TCNT0 = 0;

        while (TCNT0 < one_ms_ticks);
    }
}

void delay_X(uint16_t interval)
{
    static const uint16_t one_ms_ticks = 16000;

    for (uint16_t idx = 0; idx < interval; ++idx)
    {

        uint16_t current_tick = 0;
        TCNT1 = 0;

        while (current_tick < one_ms_ticks)
        {
          current_tick = TCNT1;
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
    DDRB = 1;

    delay_init_8();

    while (1)
    {
        PORTB = 1;
        delay_X_8(200);
        PORTB = 0;
        delay_X_8(200);
    }

    return 0;
}

int main_16(void)
{
    DDRB = 2;

    delay_init();

    while (1)
    {
        PORTB = 2;
        delay_X(200);
        PORTB = 0;
        delay_X(200);
    }

    return 0;
}
