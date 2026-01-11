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
    val rs1 = UInt(5.W)
    val rs2 = UInt(5.W)
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
    val memData = UInt(32.W)
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
    // val memOut = UInt(32.W) // Also not included because of one cycle read delay
    val control = new Bundle {
      val wb = new ControlSignals.WB
    }
  }
}

class instrPort extends Bundle {
  val addr = Output(UInt(32.W))
  val instr = Input(UInt(32.W))
  val enable = Output(Bool())
  val stall = Input(Bool())
}

class dataPort extends Bundle {
  val addr = Output(UInt(32.W))
  val dataRead = Input(UInt(32.W))
  val dataWrite = Output(UInt(32.W))
  val writeEn = Output(Bool())
  val enable = Output(Bool())
  val stall = Input(Bool())
}

class Debug extends Bundle {
  val pc = UInt(32.W)
  val regs = Vec(32, UInt(32.W))
}

class Pipeline(debug: Boolean = false, debugPrint: Boolean = false)
    extends Module {
  val io = IO(new Bundle {
    val instrPort = new instrPort()
    val dataPort = new dataPort()
  })

  val dbg = if (debug) Some(IO(Output(new Debug()))) else None

  val hazardUnit = Module(new HazardUnit())
  val branchLogic = Module(new BranchLogic())
  val forwardingUnit = Module(new ForwardingUnit())
  hazardUnit.io.exRedirect := branchLogic.io.takeBranch
  val alu = Module(new ALU())
  val aluResult = alu.io.result.asUInt
  val pc = RegInit(0.U(32.W))

  when(hazardUnit.io.out.stallPC) {
    pc := pc
  }.elsewhen(branchLogic.io.takeBranch) {
    pc := aluResult
  }.otherwise {
    pc := pc + 4.U
  }
  io.instrPort.addr := pc
  io.instrPort.enable := true.B

  val regFile = Module(new RegisterFile(debug))

  if (debug) {
    dbg.get.regs := regFile.dbg.get
    dbg.get.pc := pc
  }
  if (debugPrint) {
    printf("PC: %x\n", pc)
    for (i <- 0 until 32) {
      printf("x%d: %x ", i.U, regFile.dbg.get(i))
      if ((i + 1) % 8 == 0) printf("\n")
    }
  }

  // IF

  val IF_ID = RegInit({
    val bundle = Wire(new Pipeline.IF_ID())
    bundle := DontCare
    bundle.valid := false.B
    bundle
  })

  if (debug) {
    dontTouch(IF_ID)
  }

  when(hazardUnit.io.out.flushIFID) {
    IF_ID.valid := false.B
    IF_ID.instr := 0.U
    IF_ID.pc := 0.U
  }.otherwise {
    IF_ID.valid := true.B
    IF_ID.instr := io.instrPort.instr
    IF_ID.pc := RegNext(pc)
  }

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

  hazardUnit.io.id.rs1 := decoder.io.rs1
  hazardUnit.io.id.rs2 := decoder.io.rs2
  hazardUnit.io.id.usesRs1 := decoder.io.uses.rs1
  hazardUnit.io.id.usesRs2 := decoder.io.uses.rs2

  ID_EX.pc := IF_ID.pc
  ID_EX.valid := IF_ID.valid
  ID_EX.rs1 := decoder.io.rs1
  ID_EX.rs2 := decoder.io.rs2

  when(hazardUnit.io.out.flushIDEX) {
    ID_EX.valid := false.B
    ID_EX.control.mem.memOp := MemOp.Noop
    ID_EX.control.wb.writeEnable := false.B
  }

  ID_EX.imm := decoder.io.imm
  ID_EX.rd := decoder.io.rd

  ID_EX.control.ex := decoder.io.control.ex
  ID_EX.control.mem := decoder.io.control.mem
  ID_EX.control.wb := decoder.io.control.wb

  // EX
  val EX_MEM = RegInit({
    val bundle = Wire(new Pipeline.EX_MEM())
    bundle := DontCare
    bundle.valid := false.B
    bundle
  })

  val MEM_WB = RegInit({
    val bundle = Wire(new Pipeline.MEM_WB())
    bundle := DontCare
    bundle.valid := false.B
    bundle
  })

  forwardingUnit.io.dec.rs1 := ID_EX.rs1
  forwardingUnit.io.dec.rs2 := ID_EX.rs2

  forwardingUnit.io.dec.uses.aluRs1 := ID_EX.control.ex.aluInput1 === ALUInput1.Rs1
  forwardingUnit.io.dec.uses.aluRs2 := ID_EX.control.ex.aluInput2 === ALUInput2.Rs2
  val isBranch =
    ID_EX.control.ex.branchType =/= BranchType.NO && ID_EX.control.ex.branchType =/= BranchType.J
  forwardingUnit.io.dec.uses.bjRs1 := isBranch
  forwardingUnit.io.dec.uses.bjRs2 := isBranch
  forwardingUnit.io.dec.uses.storeDataRs2 := ID_EX.control.mem.memOp === MemOp.Store
  forwardingUnit.io.dec.uses.idRs1 := false.B
  forwardingUnit.io.dec.uses.idRs2 := false.B
  forwardingUnit.io.exe.rd := EX_MEM.rd
  forwardingUnit.io.exe.regWrite := EX_MEM.control.wb.writeEnable
  forwardingUnit.io.exe.isLoad := EX_MEM.control.mem.memOp === MemOp.Load
  forwardingUnit.io.mem.rd := MEM_WB.rd
  forwardingUnit.io.mem.regWrite := MEM_WB.control.wb.writeEnable
  forwardingUnit.io.mem.isLoad := false.B
  forwardingUnit.io.wb := DontCare
  forwardingUnit.io.wb.regWrite := false.B

  val forwardedR1 =
    MuxLookup(forwardingUnit.io.sel.data1ALUSel, regFile.io.reg1Data.asSInt)(
      Seq(
        ForwardingUnit.Sel.exe -> EX_MEM.aluResult.asSInt,
        ForwardingUnit.Sel.mem -> regFile.io.writeData.asSInt
      )
    )

  val aluInput1 =
    MuxLookup(ID_EX.control.ex.aluInput1, 0.S)(
      Seq(
        ALUInput1.Rs1 -> forwardedR1, // We dont read from pipeline register here, since reg reads are naturally behind by one clk
        ALUInput1.Pc -> ID_EX.pc.asSInt
      )
    )

  val forwardedR2 =
    MuxLookup(forwardingUnit.io.sel.data2ALUSel, regFile.io.reg2Data.asSInt)(
      Seq(
        ForwardingUnit.Sel.exe -> EX_MEM.aluResult.asSInt,
        ForwardingUnit.Sel.mem -> regFile.io.writeData.asSInt
      )
    )

  val aluInput2 =
    MuxLookup(ID_EX.control.ex.aluInput2, 0.S)(
      Seq(
        ALUInput2.Rs2 -> forwardedR2,
        ALUInput2.Imm -> ID_EX.imm
      )
    )

  alu.io.a := aluInput1
  alu.io.b := aluInput2
  alu.io.op := ID_EX.control.ex.aluOp

  hazardUnit.io.ex.rd := ID_EX.rd
  hazardUnit.io.ex.isLoad := ID_EX.control.mem.memOp === MemOp.Load
  branchLogic.io.data1 := MuxLookup(
    forwardingUnit.io.sel.data1BJSel,
    regFile.io.reg1Data.asSInt
  )(
    Seq(
      ForwardingUnit.Sel.exe -> EX_MEM.aluResult.asSInt,
      ForwardingUnit.Sel.mem -> regFile.io.writeData.asSInt // Forward the data being written back
    )
  )
  branchLogic.io.data2 := MuxLookup(
    forwardingUnit.io.sel.data2BJSel,
    regFile.io.reg2Data.asSInt
  )(
    Seq(
      ForwardingUnit.Sel.exe -> EX_MEM.aluResult.asSInt,
      ForwardingUnit.Sel.mem -> regFile.io.writeData.asSInt // Forward the data being written back
    )
  )
  branchLogic.io.branchType := ID_EX.control.ex.branchType

  if (debug) {
    dontTouch(EX_MEM)
  }

  EX_MEM.valid := ID_EX.valid
  EX_MEM.pc := ID_EX.pc
  EX_MEM.rd := ID_EX.rd
  EX_MEM.imm := ID_EX.imm

  EX_MEM.aluResult := aluResult

  val storeData =
    MuxLookup(forwardingUnit.io.sel.dataMemSel, regFile.io.reg2Data)(
      Seq(
        ForwardingUnit.Sel.exe -> EX_MEM.aluResult,
        ForwardingUnit.Sel.mem -> regFile.io.writeData
      )
    )

  EX_MEM.memData := storeData

  EX_MEM.control.mem := ID_EX.control.mem
  EX_MEM.control.wb := ID_EX.control.wb

  // MEM

  if (debug) {
    dontTouch(MEM_WB)
  }

  io.dataPort.addr := EX_MEM.aluResult
  io.dataPort.dataWrite := EX_MEM.memData
  io.dataPort.enable := EX_MEM.control.mem.memOp =/= MemOp.Noop
  io.dataPort.writeEn := (EX_MEM.control.mem.memOp === MemOp.Store) && EX_MEM.valid

  MEM_WB.valid := EX_MEM.valid
  MEM_WB.pc := EX_MEM.pc
  MEM_WB.rd := EX_MEM.rd
  MEM_WB.imm := EX_MEM.imm
  MEM_WB.aluResult := EX_MEM.aluResult

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
      regFile.io.writeData := io.dataPort.dataRead
    }
    is(WriteSource.Pc) {
      regFile.io.writeData := MEM_WB.pc + 4.U
    }
  }
}
