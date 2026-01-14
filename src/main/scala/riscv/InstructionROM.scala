package riscv

import chisel3._
import com.carlosedp.riscvassembler.RISCVAssembler

class InstructionROM(program: String) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(32.W))
    val instruction = Output(UInt(32.W))
    val valid = Output(Bool())
  })
  val romWords = RISCVAssembler
    .fromString(program)
    .split("\\R")
    .map(_.trim)
    .filter(_.nonEmpty)
    .map(h => BigInt(h, 16).U(32.W))
    .toSeq

  val ROM = VecInit(romWords) // Simple ROM implementation for BIOS

  val valid = RegNext(true.B, false.B)
  io.valid := valid
  io.instruction := RegNext(ROM(io.addr(31, 2)), 0.U) // word aligned
}
