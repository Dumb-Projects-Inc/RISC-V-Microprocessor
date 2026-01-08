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
//
//
//
// 0x00000000 - 0x0FFFFFFF : Peripherals

object MemoryMap {
  val Peripherals = 0x00000000.U(32.W)
  val ProgramMemory = 0x10000000.U(32.W)
  val BIOS = 0xfffff000.U(32.W) // 512 bytes for BIOS
}

class CacheController extends Module {
  val io = IO(new Bundle {
    // Define cache controller IO here
  })

  val L1InstructionCache = Module(
    new Cache(numLines = 4, lineSize = 1024)
  ) // long lines, short cache, 1024 bytes = 256 instructions per line, still 4Kb
  val L1DataCache = Module(
    new Cache(numLines = 64, lineSize = 64)
  ) // 4KB data cache

  L1InstructionCache.io.addr := DontCare
  L1InstructionCache.io.writeEnable := false.B
  L1InstructionCache.io.writeData := DontCare

  L1DataCache.io.addr := DontCare
  L1DataCache.io.writeEnable := false.B
  L1DataCache.io.writeData := DontCare

}
