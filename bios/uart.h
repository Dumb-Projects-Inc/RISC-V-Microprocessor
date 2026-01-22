#ifndef UART_H
#define UART_H
#include "libr.h"
typedef struct
{
    volatile unsigned int DATA;   // Offset 0x00: Data Register
    volatile unsigned int STATUS; // Offset 0x04: Status Register

} UART_t;

volatile UART_t *get_uart(unsigned int addr)
{
    return (volatile UART_t *)addr;
}
// Example
// volatile UART_t *uart0 = get_uart(0x10000000);

void uart_write_char(volatile UART_t *uart, unsigned char c)
{
    // wait until TX is ready
    while ((uart->STATUS & 0x1) == 0)
        ;
    uart->DATA = (unsigned int)c;
}

void uart_write_string(volatile UART_t *uart, const char *str)
{
    while (*str)
    {
        uart_write_char(uart, (unsigned int)*str++);
    }
}

void uart_write_int(volatile UART_t *uart, unsigned int value)
{
    char bytes[9];
    itoa(value, bytes);
    uart_write_string(uart, bytes);
}

void uart_read_char(volatile UART_t *uart, unsigned char *out)
{
    // wait until data is available
    while ((uart->STATUS & 0x2) == 0)
        ;
    *out = (unsigned char)(uart->DATA & 0xFF);
}

void uart_read_line(volatile UART_t *uart, unsigned char *buffer, unsigned int max_length)
{
    unsigned int index = 0;
    unsigned char c;
    while (index < max_length - 1)
    {
        uart_read_char(uart, &c);
        if (c == '\n' || c == '\r')
        {
            break;
        }
        buffer[index++] = c;
    }
    buffer[index] = '\0';
}

#endif // UART_H