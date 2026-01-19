# Makefile for CAE tests

CROSS = riscv32-unknown-linux-gnu-
CC = $(CROSS)gcc
OBJCOPY = $(CROSS)objcopy
OBJDUMP = $(CROSS)objdump

# We use -march=rv32i because the microprocessor is a base RV32I implementation.
# -mabi=ilp32 is the standard 32-bit ABI.
# -ffreestanding and -nostdlib are used for bare-metal compilation.
# -Wl,-N is used to disable page alignment of sections and avoid PHDR errors.
# --build-id=none removes the .note.gnu.build-id section.
# -no-pie is essential for bare-metal as linux-gnu toolchains often default to PIE.
# -fno-stack-protector is needed to avoid dependencies on stack protection libraries.
CFLAGS = -mabi=ilp32 -march=rv32i -ffreestanding -nostdlib -O0 -Wl,-N -Wl,--build-id=none -no-pie -fno-stack-protector
LDFLAGS = -T linker.ld

ASM_DIR = asm
BIN_DIR = bin

SRCS_S = $(wildcard $(ASM_DIR)/*.s)
SRCS_C = $(wildcard $(ASM_DIR)/*.c)

BINS = $(patsubst $(ASM_DIR)/%.s, $(BIN_DIR)/%.bin, $(SRCS_S)) \
       $(patsubst $(ASM_DIR)/%.c, $(BIN_DIR)/%.bin, $(SRCS_C))

.PHONY: all clean

all: $(BINS)

$(BIN_DIR)/%.bin: $(BIN_DIR)/%.elf
	$(OBJCOPY) -O binary $< $@

$(BIN_DIR)/%.elf: $(ASM_DIR)/%.s linker.ld
	@mkdir -p $(BIN_DIR)
	$(CC) $(CFLAGS) $(LDFLAGS) $< -o $@

$(BIN_DIR)/%.elf: $(ASM_DIR)/%.c linker.ld
	@mkdir -p $(BIN_DIR)
	$(CC) $(CFLAGS) $(LDFLAGS) $< -o $@

clean:
	rm -f $(BIN_DIR)/*.elf $(BIN_DIR)/*.bin
