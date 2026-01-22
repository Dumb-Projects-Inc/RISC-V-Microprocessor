# RISC-V-Microprocessor
Product for DTU course 02114 Design of a RISC-V microprocessor

## Getting started

### Running
Project uses sbt, helpful commands:
```bash
sbt run # Runs the chisel elaboration for the processor
sbt test # Run all tests
sbt testOnly riscv.E2ESpec # Test the processor against ISA simulator
```

# Final running
```bash
make
cd bios
uv run loader.py
```