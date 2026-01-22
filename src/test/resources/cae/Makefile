# Toolchain
RV64 ?= N
ifeq ($(RV64), Y)
	PREFIX = riscv64-unknown-linux-gnu-
else
	PREFIX = riscv32-unknown-linux-gnu-
endif
CC = $(PREFIX)gcc
OBJCOPY = $(PREFIX)objcopy
OBJDUMP = $(PREFIX)objdump

# Flags
CFLAGS = -march=rv32i -mabi=ilp32 -nostdlib -O0 -fno-stack-protector -fno-pic -T linker.ld
ASMFLAGS = -march=rv32i -mabi=ilp32 -nostdlib -fno-pic -T linker.ld

# Directories
ASM_DIR = asm
BIN_DIR = bin

# Files
ASM_SRCS = $(wildcard $(ASM_DIR)/*.s)
C_SRCS = $(wildcard $(ASM_DIR)/*.c)
ASM_BINS = $(patsubst $(ASM_DIR)/%.s, $(BIN_DIR)/%.bin, $(ASM_SRCS))
C_BINS = $(patsubst $(ASM_DIR)/%.c, $(BIN_DIR)/%.bin, $(C_SRCS))

.PHONY: all clean disasm-%

all: $(BIN_DIR) $(ASM_BINS) $(C_BINS)

disasm-%: $(BIN_DIR)/%.bin
	$(OBJDUMP) -D -b binary -m riscv:rv32 -M no-aliases $<

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

$(BIN_DIR)/%.bin: $(BIN_DIR)/%.elf
	$(OBJCOPY) -O binary --only-section=.text $< $@

$(BIN_DIR)/%.elf: $(ASM_DIR)/%.s
	$(CC) $(ASMFLAGS) $< -o $@

$(BIN_DIR)/%.elf: $(ASM_DIR)/%.c
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -rf $(BIN_DIR)
