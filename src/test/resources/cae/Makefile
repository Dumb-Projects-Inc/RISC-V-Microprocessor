CC      ?= riscv64-unknown-elf-gcc
OBJCOPY ?= riscv64-unknown-elf-objcopy
OBJDUMP ?= riscv64-unknown-elf-objdump

ASM_DIR = asm
BIN_DIR = bin

FLAGS   = -march=rv32i -mabi=ilp32 -nostdlib -fno-pic -T linker.ld
CFLAGS  = $(FLAGS) -O0 -fno-stack-protector

SRCS_ASM = $(wildcard $(ASM_DIR)/*.s)
SRCS_C   = $(wildcard $(ASM_DIR)/*.c)
BINS     = $(SRCS_ASM:$(ASM_DIR)/%.s=$(BIN_DIR)/%.bin) \
           $(SRCS_C:$(ASM_DIR)/%.c=$(BIN_DIR)/%.bin)

.PHONY: all clean disasm-%

all: $(BIN_DIR) $(BINS)

clean:
	rm -rf $(BIN_DIR)

$(BIN_DIR):
	mkdir -p $@

# Build Rules
$(BIN_DIR)/%.bin: $(BIN_DIR)/%.elf
	$(OBJCOPY) -O binary --only-section=.text $< $@

$(BIN_DIR)/%.elf: $(ASM_DIR)/%.s
	$(CC) $(FLAGS) $< -o $@

$(BIN_DIR)/%.elf: $(ASM_DIR)/%.c
	$(CC) $(CFLAGS) $< -o $@

# Debug Helper
disasm-%: $(BIN_DIR)/%.bin
	$(OBJDUMP) -D -b binary -m riscv:rv32 -M no-aliases $<
