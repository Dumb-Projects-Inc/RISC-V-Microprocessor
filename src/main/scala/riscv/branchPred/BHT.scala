package riscv.branchPred

import chisel3._
import chisel3.util._

object Prediction extends ChiselEnum {
  val notTaken, weakNotTaken, weakTaken, taken = Value
}

class BHT(entries: Int) extends Module {
  val io = IO(new Bundle {
    val currentPc = Input(UInt(32.W))
    val pred = Output(Bool())

    val update = Input(Bool())
    val taken = Input(Bool())
    val updatePc = Input(UInt(32.W))
  })

  val indexBits = log2Ceil(entries)

  // 0: not taken
  // 1: weak not taken - Default
  // 2: weak taken
  // 3: taken

  val table = RegInit(VecInit(Seq.fill(entries)(Prediction.weakNotTaken)))

  def getIndex(pc: UInt): UInt = pc(1 + indexBits, 2)

  io.pred := table(getIndex(io.currentPc)).asUInt > 1.U

  // Update
  when(io.update) {
    val idx = getIndex(io.updatePc)
    when(io.taken) {
      when(table(idx) =/= Prediction.taken) {
        table(idx) := Prediction(table(idx).asUInt + 1.U)
      }.otherwise {
        table(idx) := Prediction.taken
      }
    }.elsewhen(!io.taken) {
      when(table(idx) =/= Prediction.notTaken) {
        table(idx) := Prediction(table(idx).asUInt - 1.U)
      }.otherwise {
        table(idx) := Prediction.notTaken
      }
    }

  }

}
