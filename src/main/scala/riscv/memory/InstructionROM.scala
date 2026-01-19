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
  })

  val ROM = VecInit(program) // Simple ROM implementation for BIOS

  val valid = RegNext(true.B, false.B)
  io.valid := valid
  val index = io.addr(31, 2)
  io.instruction := RegNext(
    Mux(index < program.length.U, ROM(index), 0x13.U),
    0x13.U
  ) // word aligned
}
