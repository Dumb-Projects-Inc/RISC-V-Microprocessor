package riscv

import chisel3._

class RV32ITop extends Module {
  val io = IO(new Bundle {})

  val cpu = Module(new RV32I())

}

class RV32I extends Module {
  val io = IO(new Bundle {})

  val pc = RegInit(0.U(32.W))

  val regFile = Module(new RegisterFile())
  regFile.io.readReg1.valid := false.B
  regFile.io.readReg2.valid := false.B
  regFile.io.writeReg.valid := false.B
  regFile.io.writeData := DontCare

  val rd1 = WireDefault(0.U(32.W))
  val rd2 = WireDefault(0.U(32.W))
  regFile.io.readData1.ready := true.B
  regFile.io.readData2.ready := true.B
  rd1 := regFile.io.readData1.bits
  rd2 := regFile.io.readData2.bits

  // IF
  val instr = Wire(Valid(UInt(32.W))) // TODO: fetch instruction from memory
  instr.valid := true.B
  // Hardcoded to addi x1, x0, 0x123
  instr.bits := 0x12300093.U
  val IF_ID = Pipe(instr)
  // ID
  val ID_EQ = Pipe(IF_ID)
  ID_EQ.valid := IF_ID.valid
  when(IF_ID.valid) {
    val decoder = Module(new Decoder)
    decoder.io.instr := IF_ID.bits
    // TODO: give the register file the registers to read
    regFile.io.readReg1.bits := decoder.io.rs1
    regFile.io.readReg2.bits := decoder.io.rs2
  }

  // EX
  val EX_MEM = Pipe(ID_EQ)
  EX_MEM.valid := ID_EQ.valid

  decoder.io.imm := DontCare // TODO: connect immediate generator
  val _a1 = decoder.io.aluInput1 // TODO: use ALU input 1
  val _a2 = decoder.io.aluInput2 // TODO: use ALU input 2
  val _ao = decoder.io.aluOp // TODO: use ALU operation

  // MEM
  val MEM_WB = Pipe(EX_MEM)
  MEM_WB.valid := EX_MEM.valid

  // WB

  regFile.io.writeReg.bits := decoder.io.rd // assume decoder just returns x0 if no write
  regFile.io.writeReg.valid := true.B
  regFile.io.writeData := 42.U // TODO: get data from ALU or memory

}
