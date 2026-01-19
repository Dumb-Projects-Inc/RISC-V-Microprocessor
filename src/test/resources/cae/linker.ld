MEMORY
{
    ROM (rx) : ORIGIN = 0x00000000, LENGTH = 4K
    RAM (rwx) : ORIGIN = 0x00010000, LENGTH = 64K
}

PHDRS
{
    text PT_LOAD;
    data PT_LOAD;
}

SECTIONS
{
    . = 0x00000000;
    
    .text : {
        KEEP(*(.text.start))
        *(.text*)
        *(.rodata*)
    } > ROM :text

    .data : {
        . = ALIGN(4);
        *(.data*)
    } > RAM :data

    .bss : {
        . = ALIGN(4);
        *(.bss*)
        *(COMMON)
    } > RAM :data
}
