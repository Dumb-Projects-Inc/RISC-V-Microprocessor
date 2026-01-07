package riscv

import chisel3._
import chisel3.util._

class ALU extends Module {
  val io = IO(new Bundle {
    val a = Input(SInt(32.W))
    val b = Input(SInt(32.W))
    val op = Input(ALUOp())
    val result = Output(SInt(32.W)) // Maybe ready/valid later if needed
  })
  io.result := DontCare
  switch(io.op) {
    is(ALUOp.Add) {
      io.result := io.a + io.b
    }
    is(ALUOp.Sub) {
      io.result := io.a - io.b
    }
  }
}
