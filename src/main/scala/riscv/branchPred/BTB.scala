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

  val ways = 2
  val sets = entries / ways
  val indexBits = log2Ceil(sets)
  val tagBits = 32 - 2 - indexBits

  val validTable = RegInit(
    VecInit(Seq.fill(sets)(VecInit(Seq.fill(ways)(false.B))))
  )
  val tagTable = Reg(Vec(sets, Vec(ways, UInt(tagBits.W))))
  val targetPcTable = Reg(Vec(sets, Vec(ways, UInt(32.W))))
  val lru = RegInit(VecInit(Seq.fill(sets)(0.U(1.W))))

  def getIndex(pc: UInt): UInt = pc(1 + indexBits, 2)
  def getTag(pc: UInt): UInt = pc(31, 2 + indexBits)

  val hit1 = (validTable(getIndex(io.currentPc))(0) && (tagTable(
    getIndex(io.currentPc)
  )(0) === getTag(io.currentPc)))
  val hit2 = (validTable(getIndex(io.currentPc))(1) && (tagTable(
    getIndex(io.currentPc)
  )(1) === getTag(io.currentPc)))

  io.hit := hit1 || hit2

  when(hit1 || hit2) {
    when(hit1) {
      io.targetPc := targetPcTable(getIndex(io.currentPc))(0)
    }.otherwise {
      io.targetPc := targetPcTable(getIndex(io.currentPc))(1)
    }
  }

  // Update segment
  when(io.update.valid) {
    val updateIndex = getIndex(io.update.pc)
    val updateTag = getTag(io.update.pc)
    when(hit1) {
      targetPcTable(updateIndex)(0) := io.update.targetPc
      lru(updateIndex) := 1.U
    }.elsewhen(hit2) {
      targetPcTable(updateIndex)(1) := io.update.targetPc
      lru(updateIndex) := 0.U
    }.otherwise {
      when(lru(updateIndex) === 1.U) {
        validTable(updateIndex)(0) := true.B
        tagTable(updateIndex)(0) := updateTag
        targetPcTable(updateIndex)(0) := io.update.targetPc
        lru(updateIndex) := 0.U
      }.elsewhen(lru(updateIndex) === 0.U) {
        validTable(updateIndex)(1) := true.B
        tagTable(updateIndex)(1) := updateTag
        targetPcTable(updateIndex)(1) := io.update.targetPc
        lru(updateIndex) := 1.U
      }.otherwise {
        validTable(updateIndex)(0) := true.B
        tagTable(updateIndex)(0) := updateTag
        targetPcTable(updateIndex)(0) := io.update.targetPc
        lru(updateIndex) := 1.U
      }
    }

  }

}
