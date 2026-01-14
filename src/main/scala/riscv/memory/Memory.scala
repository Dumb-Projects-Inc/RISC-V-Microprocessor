package riscv.memory

import chisel3._
import chisel3.util._
import os.read
import lib.Bus

class Memory(depthWords: Int) extends Module {
  val io = IO(new Bundle {
    val bus = Bus.RespondPort()
  })

  io.bus.init()

  val mem = SyncReadMem(depthWords, UInt(32.W))

  val sIdle :: sRead :: Nil = Enum(2)
  val state = RegInit(sIdle)

  val readIdx = io.bus.addr(31, 2)
  val startRead = io.bus.read && (state === sIdle)
  val readData = mem.read(readIdx, startRead)

  when((state === sIdle) && io.bus.write) {
    mem.write(readIdx, io.bus.wrData)
  }

  io.bus.stall := (state === sRead)
  io.bus.rdValid := false.B
  io.bus.rdData := readData

  switch(state) {
    is(sIdle) {
      when(startRead) {
        state := sRead
      }
    }
    is(sRead) {
      io.bus.stall := false.B
      io.bus.rdValid := true.B
      state := sIdle
    }
  }
}
