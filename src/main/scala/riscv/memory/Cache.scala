package riscv.memory

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
