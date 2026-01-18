package riscv.branchPred

import chisel3._
import chisel3.util._

class BTBEntry(tagBits: Int) extends Bundle {
  val valid = Bool()
  val tag = UInt(tagBits.W)
  val target = UInt(32.W)
}

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

  val way1 = SyncReadMem(sets, new BTBEntry(tagBits))
  val way2 = SyncReadMem(sets, new BTBEntry(tagBits))

  val lru = RegInit(VecInit(Seq.fill(sets)(0.U(1.W))))

  def getIndex(pc: UInt): UInt = pc(1 + indexBits, 2)
  def getTag(pc: UInt): UInt = pc(31, 2 + indexBits)

  val idxReq = getIndex(io.currentPc)
  val tagReq = getTag(io.currentPc)

  val tagReqR = RegNext(tagReq)

  val entry1 = way1.read(idxReq, true.B)
  val entry2 = way2.read(idxReq, true.B)

  val hit1 = entry1.valid && (entry1.tag === tagReqR)
  val hit2 = entry2.valid && (entry2.tag === tagReqR)
  io.hit := hit1 || hit2
  io.targetPc := 0.U

  when(hit1 || hit2) {
    when(hit1) {
      io.targetPc := entry1.target
    }.otherwise {
      io.targetPc := entry2.target
    }
  }

  // Update segment
  when(io.update.valid) {
    val updateIndex = getIndex(io.update.pc)
    val updateTag = getTag(io.update.pc)

    val newEntry = Wire(new BTBEntry(tagBits))
    newEntry.valid := true.B
    newEntry.tag := updateTag
    newEntry.target := io.update.targetPc

    when(lru(updateIndex) === 0.U) {
      way1.write(updateIndex, newEntry)
      lru(updateIndex) := 1.U
    }.otherwise {
      way2.write(updateIndex, newEntry)
      lru(updateIndex) := 0.U
    }
  }

}
