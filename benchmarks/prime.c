
// USER INPUT: CSR_TIMING and UART_ADDR
#define CSR_EN TRUE
#define UART_ADDR 0x10000000

#define NUM_PRIMES 100

#define BOOL int
#define TRUE 1
#define FALSE 0

#define INT_MAX 4294967295U

static inline int read_cycle(void)
{
    int value;
    __asm__ volatile("csrr %0, 0xC00" : "=r"(value));
    return value;
}

int main()
{
    volatile int start_time = 0;
#ifdef CSR_EN
    start_time = read_cycle();
#endif

    unsigned int primes[NUM_PRIMES];
    unsigned int count = 0;
    unsigned int num = 2;

    while (count < NUM_PRIMES)
    {
        BOOL is_prime = TRUE;
        // calculate i^2 without multiplication
        unsigned int i = 2;
        unsigned int i_squared = 0;
        for (; i_squared <= num; i++)
        {
            // calculate i*i without multiplication
            for (unsigned int j = 0; j < i; j++)
            {
                i_squared += i;
            }
            // calculate num % i without division
            unsigned int temp = num;
            while (temp >= i)
            {
                temp -= i;
                if (temp == 0)
                {
                    is_prime = FALSE;
                    break;
                }
            }
            if (is_prime)
            {
                primes[count++] = num;
            }
            num++;
        }
    }

    volatile int end_time = 0;
#ifdef CSR_EN
    end_time = read_cycle();
#endif

    return 0;
}