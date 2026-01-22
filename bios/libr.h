#ifndef LIBR_H
#define LIBR_H

// ###############################
// LIBR is a very basic standard library to easily perform common tasks
// without relying on external libraries.
// ###############################

static inline int read_cycle(void)
{
    int value;
    __asm__ volatile("csrr %0, 0xC00" : "=r"(value));
    return value;
}

static inline void bp()
{
    void *a7_prev;
    __asm__ volatile("mv %0, a7" : "=r"(a7_prev));
    __asm__ volatile("li a7, 2"); // syscall 2 is breakpoint
    __asm__ volatile("ecall");
    __asm__ volatile("mv a7, %0" : : "r"(a7_prev));
}

unsigned int squared(unsigned int x)
{
    unsigned int result = 0;
    for (unsigned int i = 0; i < x; i++)
    {
        result += x;
    }
    return result;
}

unsigned int modulus(unsigned int a, unsigned int b)
{
    if (b == 0)
    {
        return -1; // division by zero
    }
    unsigned int temp = a;
    while (temp >= b)
    {
        temp -= b;
    }

    return temp;
}

void itoa(unsigned int value, char *str)
{
    // convert integer to hex string, always 8 chars + null terminator
    const char hex_chars[] = "0123456789ABCDEF";
    for (int i = 7; i >= 0; i--)
    {
        str[i] = hex_chars[modulus(value, 16)];
        value >>= 4;
    }
    str[8] = '\0';
}

#endif // LIBR_H