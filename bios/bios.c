__attribute__((section(".text.start"))) __attribute__((naked)) void entry()
{
    // We love that no stack pointer is initialized by default
    __asm__ volatile("li sp, 0x00011000"); // Set stack pointer to top of memory
    __asm__ volatile("jal ra, main");
}

#include "uart.h"
#include "libr.h"
#define UART_ADDR 0x00001000

void trap_handler();

int main()
{
    volatile UART_t *uart = get_uart(UART_ADDR);
    uart_write_string(uart, "Hello, UART!\n");

    // setup trap handler for ECALL
    //__asm__ volatile("csrw mtvec, %0" : : "r"(trap_handler));

    // more cool bios stuff can go here

    // load and jump to OS entry point

    return 0;
}

void trap_handler()
{
    volatile UART_t *uart = get_uart(UART_ADDR);
    uart_write_string(uart, "Trap occurred!\n");
    return;
}