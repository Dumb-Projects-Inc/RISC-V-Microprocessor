package riscv

import chisel3._
import chisel3.util._

class RV32ITop extends Module {
  val io = IO(new Bundle {})

  val cpu = Module(new RV32I())

}

class RV32I extends Module {
  val io = IO(new Bundle {})

  val pc = RegInit(0.U(32.W))

  val regFile = Module(new RegisterFile())
  regFile.io.writeData := DontCare

  val rd1 = WireDefault(0.U(32.W))
  val rd2 = WireDefault(0.U(32.W))
  rd1 := regFile.io.readData1
  rd2 := regFile.io.readData2

  // Caches for instruction and data memory
  val ICache = Module(
    new Cache(numLines = 64, lineSize = 64)
  ) // 4KB instruction cache

  val DCache = Module(
    new Cache(numLines = 64, lineSize = 64)
  ) // 4KB data cache

  // IF
  ICache.io.addr := pc
  ICache.io.writeEnable := false.B
  ICache.io.writeData := DontCare

  // Hardcoded to addi x1, x0, 0x123
  // val instr := 0x12300093.U
  val instr = ICache.io.readData
  when(!ICache.io.hit) {
    instr.valid := false.B
    // TODO: fetch from memory
    ICache.io.writeEnable := true.B
    ICache.io.writeData := 0x12300093.U
    instr.bits := 0x12300093.U
    instr.valid := true.B
  }
  when(instr.valid) {
    pc := pc + 4.U
  }
  val IF_ID = Pipe(instr)
  // ID
  val ID_EQ = Pipe(IF_ID)
  ID_EQ.valid := IF_ID.valid
  val decoder = Module(new Decoder)
  when(IF_ID.valid) {
    decoder.io.instr := IF_ID.bits
    // TODO: give the register file the registers to read
    regFile.io.readReg1 := decoder.io.rs1
    regFile.io.readReg2 := decoder.io.rs2
  }

  // EX
  val EX_MEM = Pipe(ID_EQ)
  EX_MEM.valid := ID_EQ.valid

  decoder.io.imm := DontCare // TODO: connect immediate generator
  val _a2 =
    decoder.io.aluInput2 // TODO: use ALU i  val _a1 = decoder.io.aluInput1 // TODO: use ALU input 1nput 2
  val _ao = decoder.io.aluOp // TODO: use ALU operation

  // MEM
  val MEM_WB = Pipe(EX_MEM)
  MEM_WB.valid := EX_MEM.valid

  // WB

  regFile.io.writeReg.bits := decoder.io.rd // assume decoder just returns x0 if no write
  regFile.io.writeReg.valid := true.B
  regFile.io.writeData := 42.U // TODO: get data from ALU or memory

}
