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
