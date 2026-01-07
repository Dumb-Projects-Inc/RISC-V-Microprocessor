# RISC-V-Microprocessor
Product for DTU course 02114 Design of a RISC-V microprocessor

## Getting started

### Clone with submodules
```bash
git clone --recursive https://github.com/Dumb-Projects-Inc/RISC-V-Microprocessor.git
```
Or if already cloned
```bash
git submodule update --init --recursive
```

### Running
Project uses sbt, helpful commands:
```bash
sbt run # Runs the chisel elaboration for the processor
sbt test # Run all tests
sbt testOnly riscv.E2ESpec # Test the processor against ISA simulator
```