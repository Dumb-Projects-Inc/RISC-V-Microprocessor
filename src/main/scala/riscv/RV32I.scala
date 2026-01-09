package riscv

import chisel3._
import chisel3.util._

object Pipeline {
  class IF_ID extends Bundle {
    val valid = Bool()
    val instr = UInt(32.W)
    val pc = UInt(32.W)
  }
  class ID_EX extends Bundle {
    val valid = Bool()
    // val rs1 = UInt(5.W) // Not implemented in bundle, since register file is already delayed by one cycle
    // val rs2 = UInt(5.W)
    val rd = UInt(5.W)
    val pc = UInt(32.W)
    val imm = SInt(32.W)
    val control = new Bundle {
      val ex = new ControlSignals.EX
      val mem = new ControlSignals.MEM
      val wb = new ControlSignals.WB
    }
  }
  class EX_MEM extends Bundle {
    val valid = Bool()
    val pc = UInt(32.W)
    val rd = UInt(5.W)
    val aluResult = UInt(32.W)
    val imm = SInt(32.W)
    val memAddr = UInt(32.W)
    val control = new Bundle {
      val mem = new ControlSignals.MEM
      val wb = new ControlSignals.WB
    }
  }
  class MEM_WB extends Bundle {
    val valid = Bool()
    val pc = UInt(32.W)
    val rd = UInt(5.W)
    val imm = SInt(32.W)
    val aluResult = UInt(32.W)
    val memOut = UInt(32.W)
    val control = new Bundle {
      val wb = new ControlSignals.WB
    }
  }
}

class RV32ITop extends Module {
  val io = IO(new Bundle {})
  val cpu = Module(new RV32I())
}

class RV32IDebug extends Bundle {
  val pc = UInt(32.W)
  val registers = Vec(32, UInt(32.W))
  val ALUOut = UInt(32.W)
}

class RV32I(debug: Boolean = false) extends Module {
  // TODO: Implement two interfaces for instruction cache and data cache
  val pc = RegInit(0.U(32.W))

  val regFile = Module(new RegisterFile(debug))

  // IF
  // Dummy instruction: addi x1, x0, 0x123
  // Instruction should come from IO
  val instr = 0x12300093.U(32.W)
  pc := pc + 4.U

  val IF_ID = RegInit({
    val bundle = Wire(new Pipeline.IF_ID())
    bundle := DontCare
    bundle.valid := false.B
    bundle
  })

  if (debug) {
    dontTouch(IF_ID)
  }

  IF_ID.valid := true.B // Valid is initialized to false, set to true on next clockcycle. Valid signal cascades to next cycle
  IF_ID.instr := instr
  IF_ID.pc := pc

  // ID
  val decoder = Module(new Decoder())
  decoder.io.instr := IF_ID.instr

  regFile.io.readReg1 := decoder.io.rs1
  regFile.io.readReg2 := decoder.io.rs2

  val ID_EX = RegInit({
    val bundle = Wire(new Pipeline.ID_EX())
    bundle := DontCare
    bundle.valid := false.B
    bundle
  })

  if (debug) {
    dontTouch(ID_EX)
  }

  ID_EX.pc := IF_ID.pc
  ID_EX.valid := IF_ID.valid

  ID_EX.imm := decoder.io.imm
  ID_EX.rd := decoder.io.rd
  ID_EX.control.ex.aluInput1 := decoder.io.aluInput1
  ID_EX.control.ex.aluInput2 := decoder.io.aluInput2
  ID_EX.control.ex.aluOp := decoder.io.aluOp
  ID_EX.control.mem := DontCare // TODO: Not implemented
  ID_EX.control.wb.writeEnable := decoder.io.writeEnable
  ID_EX.control.wb.writeSource := decoder.io.writeSource

  // EX
  val alu = Module(new ALU())
  val aluInput1 =
    Mux(
      ID_EX.control.ex.aluInput1 === ALUInput1.Rs1,
      regFile.io.reg1Data.asSInt, // We dont read from pipeline register here, since reg reads are naturally behind by one clk
      ID_EX.pc.asSInt
    )
  val aluInput2 =
    Mux(
      ID_EX.control.ex.aluInput2 === ALUInput2.Rs2,
      regFile.io.reg2Data.asSInt, // Above comment also applies here
      ID_EX.imm
    )
  alu.io.a := aluInput1
  alu.io.b := aluInput2
  alu.io.op := ID_EX.control.ex.aluOp
  val aluResult = alu.io.result.asUInt

  // TODO: Branching logic
  // val branchLogic = Module(new BranchLogic())
  // branchLogic.io.data1 := rd1Val.asSInt
  // branchLogic.io.data2 := rd2Val.asSInt
  // branchLogic.io.branchJump := decoder.io.branchType
  // val pcSelect = branchLogic.io.pcSelect

  val EX_MEM = RegInit({
    val bundle = Wire(new Pipeline.EX_MEM())
    bundle := DontCare
    bundle.valid := false.B
    bundle
  })

  if (debug) {
    dontTouch(EX_MEM)
  }

  EX_MEM.valid := ID_EX.valid
  EX_MEM.pc := ID_EX.pc
  EX_MEM.rd := ID_EX.rd
  EX_MEM.imm := ID_EX.imm

  EX_MEM.aluResult := aluResult
  EX_MEM.memAddr := DontCare // TODO

  EX_MEM.control.mem := ID_EX.control.mem
  EX_MEM.control.wb := ID_EX.control.wb

  // MEM

  val temporaryMemoryOutput = 0x67.U

  val MEM_WB = RegInit({
    val bundle = Wire(new Pipeline.MEM_WB())
    bundle := DontCare
    bundle.valid := false.B
    bundle
  })

  if (debug) {
    dontTouch(MEM_WB)
  }

  MEM_WB.valid := EX_MEM.valid
  MEM_WB.pc := EX_MEM.pc
  MEM_WB.rd := EX_MEM.rd
  MEM_WB.imm := EX_MEM.imm
  MEM_WB.aluResult := EX_MEM.aluResult

  MEM_WB.memOut := temporaryMemoryOutput

  MEM_WB.control.wb := EX_MEM.control.wb

  // WB
  regFile.io.writeData := DontCare
  regFile.io.wrEn := MEM_WB.control.wb.writeEnable && MEM_WB.valid
  regFile.io.writeReg := MEM_WB.rd

  switch(MEM_WB.control.wb.writeSource) {
    is(WriteSource.ALU) {
      regFile.io.writeData := MEM_WB.aluResult
    }
    is(WriteSource.Memory) {
      regFile.io.writeData := MEM_WB.memOut
    }
  }

  val dbg = if (debug) Some(IO(Output(new RV32IDebug()))) else None
  if (debug) {
    dbg.get.pc := pc
    dbg.get.ALUOut := aluResult
    dbg.get.registers := regFile.dbg.get
  }

}
