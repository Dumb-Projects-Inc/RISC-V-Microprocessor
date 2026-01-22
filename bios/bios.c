__attribute__((section(".text.start"))) __attribute__((naked)) void entry()
{
    // We love that no stack pointer is initialized by default
    __asm__ volatile("li sp, 0x0001FFF0"); // Set stack pointer to top of memory
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
    __asm__ volatile("csrw mtvec, %0" : : "r"(trap_handler));

    // Do a breakpoint just to show
    bp();

    // load and jump to OS entry point
    __asm__ volatile("mv a0, %0" : : "r"(0x00010004)); // load address where program will be loaded
    __asm__ volatile("li a7, 1");                      // syscall 1 is load_program
    __asm__ volatile("ecall");                         // trigger trap to load program
    return 0;
}

typedef void (*trap_function_t)(void *a0);

void dump_regs(void *a0);
void load_program(void *a0);
void breakpoint(void *a0);

trap_function_t sys_table[32] = {
    dump_regs,    // 0
    load_program, // 1
    breakpoint,   // 2
    0};

void trap_handler()
{
    // Capture registers immediately before any C logic runs
    // Use the 'register' keyword with asm binding to force specific registers
    register unsigned long syscall_num_reg asm("a7");
    register void *a0_reg asm("a0");

    // Copy to local variables to ensure they aren't clobbered later
    unsigned long syscall_num = syscall_num_reg;
    void *a0 = a0_reg;

    volatile UART_t *uart = get_uart(UART_ADDR);
    // uart_write_string(uart, "Trap occurred!\n");

    if (syscall_num < 3)
    {
        sys_table[syscall_num](a0);
    }
    else
    {
        uart_write_string(uart, "Unknown syscall!\n");
    }

    // Increment MEPC to return to the instruction after ecall
    volatile unsigned long mepc;
    __asm__ volatile("csrr %0, mepc" : "=r"(mepc));
    mepc += 4;
    __asm__ volatile("csrw mepc, %0" : : "r"(mepc));
    __asm__ volatile("mret");
}

void dump_regs(void *a0)
{
    volatile UART_t *uart = get_uart(UART_ADDR);
    // save all registers to uart
    long buf[31];
    __asm__ volatile("mv %0, x1" : "=r"(buf[0]));
    __asm__ volatile("mv %0, x2" : "=r"(buf[1]));
    __asm__ volatile("mv %0, x3" : "=r"(buf[2]));
    __asm__ volatile("mv %0, x4" : "=r"(buf[3]));
    __asm__ volatile("mv %0, x5" : "=r"(buf[4]));
    __asm__ volatile("mv %0, x6" : "=r"(buf[5]));
    __asm__ volatile("mv %0, x7" : "=r"(buf[6]));
    __asm__ volatile("mv %0, x8" : "=r"(buf[7]));
    __asm__ volatile("mv %0, x9" : "=r"(buf[8]));
    __asm__ volatile("mv %0, x10" : "=r"(buf[9]));
    __asm__ volatile("mv %0, x11" : "=r"(buf[10]));
    __asm__ volatile("mv %0, x12" : "=r"(buf[11]));
    __asm__ volatile("mv %0, x13" : "=r"(buf[12]));
    __asm__ volatile("mv %0, x14" : "=r"(buf[13]));
    __asm__ volatile("mv %0, x15" : "=r"(buf[14]));
    __asm__ volatile("mv %0, x16" : "=r"(buf[15]));
    __asm__ volatile("mv %0, x17" : "=r"(buf[16]));
    __asm__ volatile("mv %0, x18" : "=r"(buf[17]));
    __asm__ volatile("mv %0, x19" : "=r"(buf[18]));
    __asm__ volatile("mv %0, x20" : "=r"(buf[19]));
    __asm__ volatile("mv %0, x21" : "=r"(buf[20]));
    __asm__ volatile("mv %0, x22" : "=r"(buf[21]));
    __asm__ volatile("mv %0, x23" : "=r"(buf[22]));
    __asm__ volatile("mv %0, x24" : "=r"(buf[23]));
    __asm__ volatile("mv %0, x25" : "=r"(buf[24]));
    __asm__ volatile("mv %0, x26" : "=r"(buf[25]));
    __asm__ volatile("mv %0, x27" : "=r"(buf[26]));
    __asm__ volatile("mv %0, x28" : "=r"(buf[27]));
    __asm__ volatile("mv %0, x29" : "=r"(buf[28]));
    __asm__ volatile("mv %0, x30" : "=r"(buf[29]));
    __asm__ volatile("mv %0, x31" : "=r"(buf[30]));

    uart_write_string(uart, "Register dump:\n");
    for (int i = 0; i < 31; i++)
    {
        uart_write_string(uart, "x");
        uart_write_int(uart, i + 1);
        uart_write_string(uart, ": 0x");
        uart_write_int(uart, (unsigned int)buf[i]);
        uart_write_string(uart, "\n");
    }
}

void load_program(void *a0)
{
    volatile UART_t *uart = get_uart(UART_ADDR);
    uart_write_string(uart, "Load program function called!\n");
    uart_write_int(uart, (unsigned int)a0);
    uart_write_string(uart, "\n");
    // get data from uart and load into memory starting from address in a0
    volatile unsigned int load_addr = (unsigned int)a0;
    unsigned int ff_cnt = 0;

    while (1)
    {
        unsigned char c;
        uart_read_char(uart, &c);
        if (c == 0xff) // EOT
        {
            ff_cnt++;
            if (ff_cnt == 4)
                break;
        }
        else
        {
            ff_cnt = 0;
        }
        *((unsigned char *)load_addr) = c;
        load_addr++;
    }
    // Set MEPC to a0 and return
    uart_write_string(uart, "Program loaded. Jumping to entry point...\n");
    __asm__ volatile("csrw mepc, %0" : : "r"((unsigned int)a0));
    __asm__ volatile("mret");
}

void breakpoint(void *a0)
{
    volatile UART_t *uart = get_uart(UART_ADDR);
    uart_write_string(uart, "Breakpoint hit!\n");
    unsigned char buf;
    while (1) // wait for 'c' to continue
    {
        uart_read_char(uart, &buf);
        if (buf == 'c')
        {
            break;
        }
        if (buf == 'd')
        {
            dump_regs(0);
        }
    }
}