package riscv

import chisel3._
import chisel3.util._

class Pipeline {
  class IF_ID extends Bundle {
    val valid = Bool()
    val instr = UInt(32.W)
    val pc    = UInt(32.W)
  }
  class ID_EX extends Bundle {
    val valid   = Bool()
    val rs1     = UInt(5.W)
    val rs2     = UInt(5.W)
    val rd      = UInt(5.W)
    val pc      = UInt(32.W)
    val imm     = SInt(32.W)
    val control = new Bundle {
      val ex    = new ControlSignals.EX
      val mem   = new ControlSignals.MEM
      val wb    = new ControlSignals.WB
    }
  }
  class EX_MEM extends Bundle {
    val valid     = Bool()
    val pc        = UInt(32.W)
    val rd        = UInt(5.W)
    val aluResult = UInt(32.W)
    val imm       = SInt(32.W)
    val memAddr   = UInt(32.W)
    val control   = new Bundle {
      val mem     = new ControlSignals.MEM
      val wb      = new ControlSignals.WB
    }
  }
  class MEM_WB extends Bundle {
    val valid     = Bool()
    val memOut    = UInt(32.W)
    val pc        = UInt(32.W)
    val aluResult = UInt(32.W)
    val imm       = SInt(32.W)
    val control   = new Bundle {
      val wb      = new ControlSignals.WB
    }
  }
}

class RV32ITop extends Module {
  val io = IO(new Bundle {})

  val cpu = Module(new RV32I())
}

class RV32I extends Module {

  val pc = RegInit(0.U(32.W))

  val regFile = Module(new RegisterFile())
  regFile.io.writeData := DontCare

  val rd1Val = regFile.io.readData1
  val rd2Val = regFile.io.readData2

  // IF
  val instr = 0x12300093.U(32.W) // Dummy instruction: addi x1, x0, 0x123
  pc := pc + 4.U

  // ID
  val decoder = Module(new Decoder())
  decoder.io.instr := RegNext(instr)

  regFile.io.readReg1 := decoder.io.rs1
  regFile.io.readReg2 := decoder.io.rs2

  // EX
  val alu = Module(new ALU())
  val aluInput1 =
    Mux(decoder.io.aluInput1 === ALUInput1.Rs1, rd1Val.asSInt, pc.asSInt)
  val aluInput2 =
    Mux(decoder.io.aluInput2 === ALUInput2.Rs2, rd2Val.asSInt, decoder.io.imm)
  alu.io.a := aluInput1
  alu.io.b := aluInput2
  alu.io.op := decoder.io.aluOp
  val aluResult = alu.io.result.asUInt
  // MEM
  
  val temporaryMemoryOutput = 0x69.U

  // WB

  regFile.io.writeData := DontCare

  switch(decoder.io.writeSource) {
    is(WriteSource.ALU) {
      regFile.io.writeData := aluResult
    }
    is(WriteSource.Memory) {
      regFile.io.writeData := temporaryMemoryOutput
    }
  }

}
