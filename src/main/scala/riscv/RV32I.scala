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

}
