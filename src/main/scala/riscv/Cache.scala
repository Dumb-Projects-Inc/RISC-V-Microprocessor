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

object MemoryMap {
  val RomStart = 0x00000000
  val RomEnd = 0x00000fff
  val PeripheralsStart = 0x00001000
  val PeripheralsEnd = 0x0000ffff
  val ProgramMemoryStart = 0x00010000
  val ProgramMemoryEnd = 0x00700000
}

class CacheRequest extends Bundle {
  val addr = UInt(32.W)
  val writeData = UInt(1024.W)
  val dataLength = UInt(3.W)
  val writeEnable = Bool()
}

class CacheController extends Module {
  val io = IO(new Bundle {
    // Define cache controller IO here
    val instrAddr = Input(UInt(32.W))
    val request = Input(new CacheRequest())
    val stall = Output(Bool())
    val flush = Input(Bool()) // fence.i instruction
  })

  val L1ICache = Module(
    new Cache(numLines = 4, lineSize = 1024)
  ) // long lines, short cache, 1024 bytes = 256 instructions per line, still 4Kb
  val L1DCache = Module(
    new Cache(numLines = 4, lineSize = 1024)
  ) // 4KB data cache

  L1ICache.io.addr := io.instrAddr
  L1ICache.io.writeEnable := false.B
  L1ICache.io.writeData := DontCare

  L1DCache.io.addr := io.request.addr
  L1DCache.io.writeEnable := io.request.writeEnable
  L1DCache.io.writeData := io.request.writeData

  val stall = !(L1ICache.io.hit && L1DCache.io.hit)
  io.stall := stall

}
