MEMORY
{
    rom (rx) : ORIGIN = 0x00000000, LENGTH = 4K
    ram (rw) : ORIGIN = 0x00010000, LENGTH = 16K
}

SECTIONS
{
    .text : {
        *(.text)
        *(.text.*)
    } > rom

    .rodata : {
        *(.rodata)
        *(.rodata.*)
    } > rom

    .data : {
        *(.data)
        *(.data.*)
    } > ram

    .bss : {
        *(.bss)
        *(COMMON)
    } > ram
}
