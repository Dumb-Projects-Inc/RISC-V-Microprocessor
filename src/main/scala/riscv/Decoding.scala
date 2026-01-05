package riscv

import chisel3._
import chisel3.util.MuxLookup


object ALUSrc extends ChiselEnum {
  val Imm, Reg = Value
}

object ALUOp extends ChiselEnum {
  val Add, Sub = Value
}

object Format extends ChiselEnum {
  val R,I,S,B,U,J = Value
}

object Opcodes extends ChiselEnum {
  val LUI     = "b0110111".U
  val AUIPC   = "b0010111".U
  val JAL     = "b1101111".U
  val JALR    = "b1100111".U
  val BRANCH  = "b1100011".U
  val LOAD    = "b0000011".U
  val STORE   = "b0100011".U
  val IMM     = "b0010011".U
  val REG     = "b0110011".U
}



// TODO: possible optimization, consider using https://www.chisel-lang.org/docs/explanations/decoder to minimize logic
class Decoder extends Module {
  val io = IO(new Bundle{
    val instr = Input(Bits(32.W))
  })

  val opcode  = io.instr(6,0)
  val funct3  = io.instr(14,12)
  val funct7 = io.instr(31,25)

  val rd      = io.instr(11,7)
  val rs1     = io.instr(19,15)
  val rs2     = io.instr(24,20)

}

class ImmGen extends Module {
  val io = IO(new Bundle{
    val instr = Input(Bits(32.W))
    val format = Input(Format())
    val out = Output(SInt(32.W))
  })

  io.out := MuxLookup(io.format, 0.S)(Seq(
    Format.I -> io.instr(31,20).asSInt,
    Format.S -> (io.instr(31,25) ## io.instr(11,7)).asSInt,
    Format.B -> (io.instr(31) ## io.instr(7) ## io.instr(30,25) ## io.instr(11,8) ## 0.U(1.W)).asSInt,
    Format.U -> (io.instr(31,12) ## 0.U(12.W)).asSInt,
    Format.J -> (io.instr(31) ## io.instr(19,12) ## io.instr(20) ## io.instr(30,21) ## 0.U(1.W)).asSInt,
    ))

}
