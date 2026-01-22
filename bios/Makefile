# Toolchain
# Shamelessly stealing from Schoeberl to support mac
UNAME := $(shell uname)
ifeq ($(UNAME),Darwin)
# assuming tools are installed with Mac Homebrew
export CROSS=riscv64-elf-
else
export CROSS=riscv64-unknown-elf-
endif
#end of shameless stealing

CC = $(CROSS)gcc
OBJCOPY = $(CROSS)objcopy

# Compiler flags for RV32I bare metal
CFLAGS = -mabi=ilp32 -O0 
CFLAGS += -ffreestanding -nostdlib -e entry -T link.ld
CFLAGS += -march=rv32ia_zicsr


TARGET = bios
SRC = bios.c
HEADERS = uart.h libr.h

.PHONY: all clean test-hw

all: $(TARGET).bin

$(TARGET).elf: $(SRC) $(HEADERS)
	$(CC) $(CFLAGS) $(DEFINES) $< -o $@ 

$(TARGET).bin: $(TARGET).elf
	$(OBJCOPY) -O binary $< $@
	
clean:
	rm -f *.elf *.o *.bin
