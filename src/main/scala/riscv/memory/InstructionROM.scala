package riscv.memory

import chisel3._
import chisel3.util._
import com.carlosedp.riscvassembler.RISCVAssembler

/** Simple ROM implementation for instruction memory (BIOS)
  * @param program
  *   RV32I assembly program as a string, each instruction in a new line
  */
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

  // TODO: This is probably a LUT in FPGA, we could consider BRAM, but if using BRAM this might be useless as we can just preload the cache.
  val ROM = VecInit(romWords) // Simple ROM implementation for BIOS

  val valid = RegNext(true.B, false.B)
  io.valid := valid
  val Max_bit = log2Ceil(romWords.length)
  io.instruction := RegNext(ROM(io.addr(Max_bit + 1, 2)), 0.U) // word aligned
}
