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
    is(ALUOp.Sll) { // shift left logical
      io.result := io.a.asSInt << io.b(4, 0)
    }
    is(ALUOp.Slt) { // set less than
      io.result := Mux(io.a.asSInt < io.b.asSInt, 1.S, 0.S)
    }
    is(ALUOp.Sltu) { // set less than unsigned
      io.result := Mux(io.a.asUInt < io.b.asUInt, 1.S, 0.S)
    }
    is(ALUOp.Xor) { // bitwise xor
      io.result := io.a ^ io.b
    }
    is(ALUOp.Srl) { // shift right logical
      io.result := (io.a.asUInt >> io.b(4, 0)).asSInt
    }
    is(ALUOp.Sra) { // shift right arithmetic
      io.result := io.a.asSInt >> io.b(4, 0)
    }
    is(ALUOp.Or) { // bitwise or
      io.result := io.a | io.b
    }
    is(ALUOp.And) { // bitwise and
      io.result := io.a & io.b
    }
    is(ALUOp.Noop) {
      io.result := io.b
    }
  }
}
