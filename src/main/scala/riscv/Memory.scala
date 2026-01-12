package riscv

import chisel3._
import chisel3.util._

class Memory(depthWords: Int) extends Module {
  val io = IO(new Bundle {
    val bus = Bus.RespondPort()
  })

  io.bus.init()

  val mem = SyncReadMem(depthWords, UInt(32.W))
  val pendingRead = RegInit(false.B)
  val readIdx = io.bus.addr(31, 2)
  val startRead = io.bus.read && !pendingRead
  val readData = mem.read(readIdx, startRead)

  when(io.bus.write && !pendingRead) {
    mem.write(readIdx, io.bus.wrData)
  }

  io.bus.stall := pendingRead
  io.bus.rdValid := RegNext(startRead, false.B)
  io.bus.rdData := readData

  when(startRead) {
    pendingRead := true.B
  }.elsewhen(pendingRead) {
    pendingRead := false.B
  }
}
