package riscv

import chisel3._
import chisel3.util._

object Instruction {
  def ADDI = BitPat("b????????????_?????_000_?????_0010011")
  def ADD = BitPat("b0000000_?????_?????_000_?????_0110011")
  def LD = BitPat("b????????????_?????_011_?????_0000011")
  def SD = BitPat("b???????_?????_?????_011_?????_0100011")
  def JAL = BitPat("b????????????????????_?????_1101111")

}

object ALUInput1 extends ChiselEnum {
  val Rs1, Pc = Value
}

object ALUInput2 extends ChiselEnum {
  val Rs2, Imm = Value
}

object ALUOp extends ChiselEnum {
  val Add, Sub, Sll, Slt, Sltu, Xor, Srl, Sra, Or, And = Value
}

object WriteSource extends ChiselEnum {
  val ALU, Memory = Value
}

object BranchType extends ChiselEnum {
  val BEQ, BNE, NO, J, BLT, BGE, BLTU, BGEU = Value
}

object Format extends ChiselEnum {
  val R, I, S, B, U, J = Value
}

object ControlSignals {
  class EX extends Bundle {
    val aluOp     = ALUOp()
    val aluInput1 = ALUInput1()
    val aluInput2 = ALUInput2()
  }
  class MEM extends Bundle {
    // TODO: 
  }
  class WB extends Bundle {
    val writeEnable = Bool()
    val writeSource = WriteSource() 
  }
}



// TODO: Consider using https://www.chisel-lang.org/docs/explanations/decoder to minimize logic
class Decoder extends Module {
  val io = IO(new Bundle {
    val instr = Input(UInt(32.W))
    val rd = Output(UInt(5.W))
    val rs1 = Output(UInt(5.W))
    val rs2 = Output(UInt(5.W))
    val aluOp = Output(ALUOp())
    val aluInput1 = Output(ALUInput1())
    val aluInput2 = Output(ALUInput2())
    val writeSource = Output(WriteSource())
    val writeEnable = Output(Bool())
    val imm = Output(SInt(32.W))
  })

  // val opcode = io.instr(6, 0)
  // val funct3 = io.instr(14, 12)
  // val funct7 = io.instr(31, 25)

  io.rd := io.instr(11, 7)
  io.rs1 := io.instr(19, 15)
  io.rs2 := io.instr(24, 20)

  io.aluOp := DontCare
  io.aluInput1 := DontCare
  io.aluInput2 := DontCare
  io.writeSource := DontCare
  io.writeEnable := DontCare

  val format = Wire(Format())
  format := DontCare
  val immGen = Module(new ImmGen)
  immGen.io.instr := io.instr
  immGen.io.format := format
  io.imm := immGen.io.out

  when(io.instr === Instruction.ADDI) {
    io.aluOp := ALUOp.Add
    io.aluInput1 := ALUInput1.Rs1
    io.aluInput2 := ALUInput2.Imm
    io.writeSource := WriteSource.ALU
    io.writeEnable := true.B
    format := Format.I
  }.elsewhen(io.instr === Instruction.ADD) {
    io.aluOp := ALUOp.Add
    io.aluInput1 := ALUInput1.Rs1
    io.aluInput2 := ALUInput2.Rs2
    io.writeSource := WriteSource.ALU
    io.writeEnable := true.B
    format := Format.R
  }
}

class ImmGen extends Module {
  val io = IO(new Bundle {
    val instr = Input(Bits(32.W))
    val format = Input(Format())
    val out = Output(SInt(32.W))
  })

  io.out := DontCare
  switch(io.format) {
    is(Format.I) {
      io.out := io.instr(31, 20).asSInt
    }
    is(Format.S) {
      io.out := (io.instr(31, 25) ## io.instr(11, 7)).asSInt
    }
    is(Format.B) {
      io.out := (io.instr(31) ## io.instr(7) ## io.instr(30, 25) ## io.instr(
        11,
        8
      ) ## 0.U(1.W)).asSInt
    }
    is(Format.U) {
      io.out := (io.instr(31, 12) ## 0.U(12.W)).asSInt
    }
    is(Format.J) {
      io.out := (io.instr(31) ## io.instr(19, 12) ## io
        .instr(20) ## io.instr(30, 21) ## 0.U(1.W)).asSInt
    }
    // TODO: Z-Format / System instructions
  }

}
