package riscv

import chisel3._
import chisel3.util._

class CSR extends Module {
  val io = IO(new Bundle {
    val currentPc = Input(UInt(32.W))
    val trapValid = Input(Bool())
    val trapCause = Input(UInt(32.W))
    val retValid = Input(Bool())

    val redirectValid = Output(Bool())
    val redirectPc = Output(UInt(32.W))
  })

  val tvec = RegInit(
    "h00000000".U(32.W)
  ) // TODO: what is the correct address for the trap vector?
  val epc = RegInit(0.U(32.W))
  val cause = RegInit(0.U(32.W))

  io.redirectValid := false.B
  io.redirectPc := 0.U

  when(io.trapValid) {
    epc := io.currentPc
    cause := io.trapCause
    io.redirectValid := true.B
    io.redirectPc := tvec
  }.elsewhen(io.retValid) {
    io.redirectValid := true.B
    io.redirectPc := epc
  }
}
