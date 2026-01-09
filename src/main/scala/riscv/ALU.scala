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
      io.result := Mux(io.a.asSInt < io.b.asSInt, 1.U, 0.U)
    }
    is(ALUOp.Sltu) { // set less than unsigned
      io.result := Mux(io.a.asSInt < io.b.asSInt, 1.U, 0.U)
    }
    is(ALUOp.Xor) { // bitwise xor
      io.result := io.a ^ io.b
    }
    is(ALUOp.Srl) { // shift right logical
      io.result := io.a.asSInt >> io.b(4, 0)
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
    // is(ALUOp.Mul) { // multiply
    //   io.result := (io.a.asSInt * io.b.asSInt).asSInt
    // }
    // is(ALUOp.Mulh) { // multiply upper half -> R[rd] = (R[rs1] * R[rs2])[(2×XLEN-1):XLEN]
    //   io.result := ((io.a.asSInt * io.b.asSInt) >> 32).asSInt
    // }
    // is(ALUOp.Mulhu) { // multiply upper half unsigned -> R[rd] = (R[rs1] * R[rs2])[(2×XLEN-1):XLEN]
    //   io.result := ((io.a.asSInt * io.b.asSInt) >> 32).asSInt
    // }
    // is(ALUOp.Mulhsu) { // multiply upper half signed × unsigned -> R[rd] = (R[rs1] * R[rs2])[(2×XLEN-1):XLEN]
    //   io.result := ((io.a.asSInt * io.b.asSInt) >> 32).asSInt
    // }
    // is(ALUOp.Div) { // signed division
    //   io.result := (io.a.asSInt / io.b.asSInt).asSInt
    // }
    // is(ALUOp.Divu) { // signed division unsigned
    //   io.result := (io.a.asSInt / io.b.asSInt).asSInt
    // }
    // is(ALUOp.Rem) { // remainder
    //   io.result := (io.a.asSInt % io.b.asSInt).asSInt
    // }
    // is(ALUOp.Rem) { // remainder unsigned
    //   io.result := (io.a.asSInt % io.b.asSInt).asSInt
    // }
  }
}
