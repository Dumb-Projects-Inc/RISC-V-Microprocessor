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

  val indexBits = log2Ceil(entries)
  val table = RegInit(VecInit(Seq.fill(entries)(1.U(2.W))))

  def getIndex(pc: UInt): UInt = pc(1 + indexBits, 2)

  io.pred := table(getIndex(io.currentPc))

  // Update
  when(io.update) {
    val idx = getIndex(io.updatePc)
    when(io.taken) {
      when(table(idx) =/= 3.U) {
        table(idx) := table(idx) + 1.U
      }.otherwise {
        table(idx) := 3.U
      }
    }.elsewhen(!io.taken) {
      when(table(idx) =/= 0.U) {
        table(idx) := table(idx) - 1.U
      }.otherwise {
        table(idx) := 0.U
      }
    }

  }

}
