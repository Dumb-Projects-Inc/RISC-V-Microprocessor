package riscv

import chisel3._
import chisel3.util._

class CacheLine(val blockWords: Int, val numLines: Int) extends Bundle {
  val valid = Bool()
  val tag = UInt((32 - log2Ceil(numLines) - log2Ceil(blockWords) - 2).W)
  val data = Vec(blockWords, UInt(32.W))
}

class Cache(numLines: Int, blockwords: Int) extends Module {
  assert(
    isPow2(numLines),
    "Number of cache lines must be a power of 2"
  )
  assert(
    log2Ceil(blockwords) + log2Ceil(numLines) <= 32,
    "Cache size exceeds addressable memory space"
  )
  val io = IO(new Bundle {
    val addr = Input(UInt(32.W))
    val writeData = Input(UInt((blockwords * 32).W))
    val writeEnable = Input(Bool())
    val readData = Output(UInt(32.W))
    val hit = Output(Bool())
  })

  val cache = SyncReadMem(numLines, new CacheLine(blockwords, numLines))
  // Offsets: Tag     | Index                | Block Offset      | Byte Offset (always 2 bit)
  //          32-rest |      log2(numLines)  |  log2(blockwords) | 2
  // 32 - 6 - 4 - 2 = 20 for 64 lines and 16 words per block
  val indexBits = log2Ceil(numLines)
  val blockOffBits = log2Ceil(blockwords)

  val indexLo = 2 + blockOffBits
  val indexHi = indexLo + indexBits - 1

  val tagLo = indexHi + 1

  val index = io.addr(indexHi, indexLo)
  val tag = io.addr(31, tagLo)

// read
  val read = RegNext(
    cache.read(index),
    0.U.asTypeOf(new CacheLine(blockwords, numLines))
  )
  val tagR = RegNext(tag)

// offset must also be delayed to match SyncReadMem latency
  val offR =
    if (blockOffBits == 0) 0.U
    else RegNext(io.addr(1 + blockOffBits, 2))

  io.readData := read.data(offR)
  io.hit := read.valid && (read.tag === tagR)

// write
  when(io.writeEnable) {
    val newLine = Wire(new CacheLine(blockwords, numLines))
    newLine.valid := true.B
    newLine.tag := tag
    newLine.data := io.writeData.asTypeOf(Vec(blockwords, UInt(32.W)))
    cache.write(index, newLine)
  }
}

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

class CacheController(ROMProgram: String) extends Module {
  val io = IO(new Bundle {
    val instrPort = Flipped(new instrPort())
    val dataPort = Flipped(new dataPort())
    // val flush = Input(Bool()) // fence.i instruction
  })

  io.instrPort.instr := DontCare
  io.dataPort.dataRead := DontCare

  val romWords = ROMProgram
    .split("\\R")
    .map(_.trim)
    .filter(_.nonEmpty)
    .map(h => BigInt(h, 16).U(32.W))
    .toSeq

  val ROM = VecInit(romWords) // Simple ROM implementation for BIOS

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
    when(instrRegion === MemoryRegions.ROM) {
      io.instrPort.instr := RegNext(
        ROM(io.instrPort.addr(31, 2))
      ) // word aligned

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
    when(dataRegion === MemoryRegions.ROM) {
      // Data access to ROM (should be read-only)
      io.dataPort.dataRead := RegNext(ROM(io.dataPort.addr(31, 2)))
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
