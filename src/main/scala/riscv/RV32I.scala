package riscv

import chisel3._

class RV32ITop extends Module {
  val io = IO(new Bundle {})

  val cpu = Module(new RV32I())

}

class RV32I extends Module {
  val io = IO(new Bundle {})

  val IF_stage = RegInit(0.U(32.W))
  val ID_stage = RegInit(0.U(32.W))
  val EX_stage = RegInit(0.U(32.W))
  val MEM_stage = RegInit(0.U(32.W))
  val WB_stage = RegInit(0.U(32.W))

  val pc = RegInit(0.U(32.W))

  val regFile = Module(new RegisterFile())
  regFile.io.readReg1.valid := false.B
  regFile.io.readReg2.valid := false.B
  regFile.io.writeReg.valid := false.B
  regFile.io.writeData := DontCare

  val rd1 = WireDefault(0.U(32.W))
  val rd2 = WireDefault(0.U(32.W))

  // IF

  // ID

  // EX

  // MEM

  // WB

}
