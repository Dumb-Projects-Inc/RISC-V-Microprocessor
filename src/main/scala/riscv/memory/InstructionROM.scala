package riscv.memory

import chisel3._
import chisel3.util._

/** Simple ROM implementation for instruction memory (BIOS)
  * @param program
  *   RV32I bytes in hex format, one word per line
  */
class InstructionROM(program: Seq[UInt]) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(32.W))
    val instruction = Output(UInt(32.W))
    val valid = Output(Bool())

    val dataAddr = Input(UInt(32.W))
    val data = Output(UInt(32.W))
  })

  val ROM = VecInit(program) // Simple ROM implementation for BIOS

  val valid = RegNext(true.B, false.B)
  io.valid := valid
  val Max_bit = log2Ceil(program.length)
  io.instruction := RegNext(ROM(io.addr(Max_bit + 1, 2)), 0.U) // word aligned
  io.data := RegNext(ROM(io.dataAddr(Max_bit + 1, 2)), 0.U)
}
