package riscv.memory

import chisel3._
import chisel3.util._
import lib.Bus

// Static Memory mapping
// Following the https://riscv.org/blog/design-approaches-and-architectures-of-risc-v-socs/ embedded memory map
// 0x0001_0000 - 0x0070_0000  : Program Memory (RAM)
// 0x0000_1000 - 0x0000_FFFF : Peripherals 60kb
// 0x0000_0000 - 0x0000_0FFF : ROM 4kb

object MemoryRegions extends ChiselEnum {
  val ROM, Peripherals, ProgramMemory = Value
}

object MemoryMap {
  // val RomStart = 0x00000000
  val RomEnd = 0x00000fff
  // val PeripheralsStart = 0x00001000
  val PeripheralsEnd = 0x0000ffff
  // val ProgramMemoryStart = 0x00010000
  // val ProgramMemoryEnd = 0x00700000 // External memory decides how much is available
}

/** CacheController using [[Cache]] and memory regions This cachecontroller
  * implements the (Modified) Harvard architecture by separating instruction and
  * data caches. Cache coherency is only ensured for I-cache if fence.i is
  * called (needs to be implemented).
  */
class CacheController() extends Module {
  val io = IO(new Bundle {
    val instrPort = Flipped(new riscv.instrPort())
    val ROMIn = Input(UInt(32.W))
    val dataPort = Flipped(new riscv.dataPort())
    // val busPort = Bus.RequestPort()
    // val flush = Input(Bool()) // fence.i instruction
  })

  io.instrPort.instr := DontCare
  io.dataPort.dataRead := DontCare
  // io.busPort.init()

  // Check region of instruction fetch
  def getRegion(addr: UInt): MemoryRegions.Type =
    MuxCase(
      MemoryRegions.ProgramMemory,
      Seq(
        (addr <= MemoryMap.RomEnd.U) -> MemoryRegions.ROM,
        (addr <= MemoryMap.PeripheralsEnd.U) -> MemoryRegions.Peripherals
      )
    )

  val instrRegion = getRegion(io.instrPort.addr)
  val dataRegion = getRegion(io.dataPort.addr)
  io.instrPort.stall := false.B
  io.dataPort.stall := false.B

  val IDat = SyncReadMem(4096, UInt(32.W)) // 16 KB Instruction Data Memory

  when(io.instrPort.enable) {
    // Always request instruction from bus
    // io.busPort.readRequest(io.instrPort.addr)

    when(instrRegion === MemoryRegions.ROM) {
      // This should not stall
      io.instrPort.instr := io.ROMIn

    }.elsewhen(instrRegion === MemoryRegions.Peripherals) {
      // instructions from peripherals not supported
      // TODO: Exceptions
      io.instrPort.instr := 0.U
      io.instrPort.stall := true.B // stall indefinitely for now
    }.otherwise {
      // Instruction fetch from Program Memory
      io.instrPort.instr := IDat.read(
        io.instrPort.addr(11, 2),
        io.instrPort.enable
      )
    }
  }

  val DDat = SyncReadMem(16384, UInt(32.W)) // 64 KB Data Memory
  when(io.dataPort.enable) {
    // io.busPort.readRequest(io.dataPort.addr)

    when(dataRegion === MemoryRegions.ROM) {
      // Data access to ROM (should be read-only)
      io.instrPort.instr := io.ROMIn
      io.dataPort.stall := false.B
    }.elsewhen(dataRegion === MemoryRegions.Peripherals) {
      // Data access to Peripherals
    }.otherwise {
      // Data access to Program Memory
      io.dataPort.dataRead := DDat.read(
        io.dataPort.addr(11, 2),
        io.dataPort.enable
      )
      when(io.dataPort.writeEn) {
        DDat.write(
          io.dataPort.addr(11, 2),
          io.dataPort.dataWrite
        )
      }
    }
  }

}
