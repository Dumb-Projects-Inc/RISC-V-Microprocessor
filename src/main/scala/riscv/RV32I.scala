package riscv

import chisel3._
import chisel3.util._
import java.util.Base64.Decoder

class RV32ITop extends Module {
  val io = IO(new Bundle {})

  val cpu = Module(new RV32I())
}

class RV32I extends Module {

  val pc = RegInit(0.U(32.W))

  val regFile = Module(new RegisterFile())
  regFile.io.writeData := DontCare

  val rd1 = regFile.io.readData1
  val rd2 = regFile.io.readData2

  // IF
  val instr = 0x12300093.U(32.W) // Dummy instruction: addi x1, x0, 0x123
  pc := pc + 4.U
  // ID
  val decoder = Module(new Decoder())
  decoder.io.instr := instr

  regFile.io.readReg1 := decoder.io.rs1
  regFile.io.readReg2 := decoder.io.rs2

  // EX
  val alu = Module(new ALU())
  val aluInput1 =
    Mux(decoder.io.aluInput1 === ALUInput1.Rs1, rd1.asSInt, pc.asSInt)
  val aluInput2 =
    Mux(decoder.io.aluInput2 === ALUInput2.Rs2, rd2.asSInt, decoder.io.imm)
  alu.io.a := aluInput1
  alu.io.b := aluInput2
  alu.io.op := decoder.io.aluOp
  val aluResult = alu.io.result.asUInt
  // MEM

  // WB

}
