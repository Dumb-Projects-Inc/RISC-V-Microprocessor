package riscv.branchPred

import chisel3._
import chisel3.util._

class BHT(entries: Int) extends Module {
  val io = IO(new Bundle {
    val currentPc = Input(UInt(32.W))
    val pred = Output(Bool())

    val update = Input(Bool())
    val taken = Input(Bool())
    val updatePc = Input(UInt(32.W))
  })
}
