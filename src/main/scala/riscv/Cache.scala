package riscv

import chisel3._
import chisel3.util._

class CacheLine(val lineSize: Int) extends Bundle {
  val valid = Bool()
  val tag = UInt((32 - log2Ceil(lineSize)).W)
}

class Cache(numLines: Int, lineSize: Int) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(32.W))
    val writeData = Input(UInt((lineSize * 8).W))
    val writeEnable = Input(Bool())
    val readData = Output(Valid(UInt(32.W)))
    val hit = Output(Bool())
  })

  val lines = RegInit(
    VecInit(Seq.fill(numLines)(0.U.asTypeOf(new CacheLine(lineSize))))
  )
  val cache = SyncReadMem(numLines, UInt((lineSize * 8).W))

  val index =
    io.addr(log2Ceil(lineSize) + log2Ceil(numLines) - 1, log2Ceil(lineSize))
  val tag = io.addr(31, log2Ceil(lineSize) + log2Ceil(numLines))
  val offset = io.addr(log2Ceil(lineSize) - 1, 0)

  val line = lines(index)
  val isHit = line.valid && (line.tag === tag)

  io.hit := isHit
  val lineData = cache.read(index)

  val lastIndex = RegNext(index)
  val isHitReg = RegNext(isHit, false.B)
  io.readData.valid := RegNext(
    (isHit && (index === lastIndex)) || (isHit && !isHitReg),
    false.B
  ) // we hit and same index means we already read data
  io.readData.bits := (lineData << (offset << 3))(
    lineSize * 8 - 1,
    lineSize * 8 - 32
  )

  when(io.writeEnable) {
    // write to cache
    lines(index).valid := true.B
    lines(index).tag := tag
    cache.write(index, io.writeData)
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

  val L1ICache = Module(new Cache(numLines = 256, lineSize = 4))
  L1ICache.io.writeEnable := false.B
  L1ICache.io.writeData := DontCare
  L1ICache.io.addr := DontCare

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
      L1ICache.io.addr := io.instrPort.addr
      L1ICache.io.writeEnable := false.B // instruction fetch is read-only
      io.instrPort.instr := L1ICache.io.readData.bits
      io.instrPort.stall := !L1ICache.io.readData.valid && !L1ICache.io.hit
    }
  }

  val L1DCache = Module(new Cache(numLines = 64, lineSize = 16))
  L1DCache.io.writeEnable := false.B
  L1DCache.io.writeData := DontCare
  L1DCache.io.addr := DontCare
  when(io.dataPort.enable) {
    when(dataRegion === MemoryRegions.ROM) {
      // Data access to ROM (should be read-only)
      io.dataPort.dataRead := RegNext(ROM(io.dataPort.addr(31, 2)))
      io.dataPort.stall := false.B
    }.elsewhen(dataRegion === MemoryRegions.Peripherals) {
      // Data access to Peripherals
    }.otherwise {
      // Data access to Program Memory
      L1DCache.io.addr := io.dataPort.addr
      L1DCache.io.writeEnable := io.dataPort.writeEn
      L1DCache.io.writeData := io.dataPort.dataWrite
      io.dataPort.dataRead := L1DCache.io.readData.bits
      io.dataPort.stall := !L1DCache.io.readData.valid && !L1DCache.io.hit

    }
  }

}
