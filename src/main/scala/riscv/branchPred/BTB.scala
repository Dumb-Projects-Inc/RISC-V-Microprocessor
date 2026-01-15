package riscv.branchPred

import chisel3._
import chisel3.util._

class BTB(entries: Int) extends Module {
  val io = IO(new Bundle {
    val currentPc = Input(UInt(32.W))
    val hit = Output(Bool())
    val targetPc = Output(UInt(32.W))

    val update = (new Bundle {
      val valid = Input(Bool())
      val pc = Input(UInt(32.W))
      val targetPc = Input(UInt(32.W))
    })
  })
}
