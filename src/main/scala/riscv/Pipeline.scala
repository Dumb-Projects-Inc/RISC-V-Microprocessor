package riscv

import chisel3._
import chisel3.util._

import riscv.memory.MemoryMap

object ControlSignals {
  class EX extends Bundle {
    val aluOp = ALUOp()
    val aluInput1 = ALUInput1()
    val aluInput2 = ALUInput2()
    val branchType = BranchType()
    val rs1 = UInt(5.W)
    val rs2 = UInt(5.W)
    val imm = SInt(32.W)
  }
  class MEM extends Bundle {
    val memOp = MemOp()
    val memSize = MemSize()
    val rs2Data = UInt(32.W)
    val branch = Bool()
  }
  class WB extends Bundle {
    val writeSource = WriteSource()
    val writeEnable = Bool()
    val aluResult = UInt(32.W)
    val rd = UInt(5.W)
    val pc = UInt(32.W)
  }
}

object ResetControl {
  def EX() = {
    val bundle = Wire(new ControlSignals.EX())
    bundle := DontCare
    bundle.branchType := BranchType.NO
    bundle
  }
  def MEM() = {
    val bundle = Wire(new ControlSignals.MEM())
    bundle := DontCare
    bundle.memOp := MemOp.Noop
    bundle.branch := false.B
    bundle
  }
  def WB() = {
    val bundle = Wire(new ControlSignals.WB)
    bundle := DontCare
    bundle.writeEnable := false.B
    bundle.rd := 0.U
    bundle
  }
}

object Pipeline {
  class ID_EX extends Bundle {
    val ex = new ControlSignals.EX
    val mem = new ControlSignals.MEM
    val wb = new ControlSignals.WB
  }
  class EX_MEM extends Bundle {
    val mem = new ControlSignals.MEM
    val wb = new ControlSignals.WB
  }
  class MEM_WB extends Bundle {
    val wb = new ControlSignals.WB
  }
}

object ResetPipeline {
  def ID_EX() = {
    val bundle = Wire(new Pipeline.ID_EX())
    bundle.ex := ResetControl.EX()
    bundle.mem := ResetControl.MEM()
    bundle.wb := ResetControl.WB()
    bundle
  }
  def EX_MEM(): Pipeline.EX_MEM = {
    val bundle = Wire(new Pipeline.EX_MEM())
    bundle.mem := ResetControl.MEM()
    bundle.wb := ResetControl.WB()
    bundle
  }

  def MEM_WB(): Pipeline.MEM_WB = {
    val bundle = Wire(new Pipeline.MEM_WB())
    bundle.wb := ResetControl.WB()
    bundle
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
  val memSize = Output(MemSize())
  val memOp = Output(MemOp())
  val enable = Output(Bool())
  val stall = Input(Bool())
}

class Debug extends Bundle {
  val pc = UInt(32.W)
  val writebackPc = UInt(32.W)
  val regs = Vec(32, UInt(32.W))
}

class Pipeline(
    debug: Boolean = false,
    debugPrint: Boolean = false,
    pcInit: Long = 0
) extends Module {
  val io = IO(new Bundle {
    val instrPort = new instrPort()
    val dataPort = new dataPort()
  })

  // Forward declarations
  val opResult = Wire(UInt(32.W))
  val flush = WireDefault(false.B)
  val stall = WireDefault(false.B)

  if (debug) {
    dontTouch(stall)
    dontTouch(flush)
  }

  val ID_EX_reg = RegInit(ResetPipeline.ID_EX())

  val EX_MEM_reg = RegInit(ResetPipeline.EX_MEM())

  val MEM_WB_reg = RegInit(ResetPipeline.MEM_WB())

  // Stage: Fetch
  val pc = RegInit(pcInit.U(32.W))
  val nextPc = WireDefault(pc + 4.U)
  pc := nextPc

  when(stall) {
    nextPc := pc
  }

  val first = RegNext(false.B, true.B)
  when(first) {
    nextPc := pc
    flush := true.B
  }

  io.instrPort.addr := nextPc
  io.instrPort.enable := true.B

  // Stage: Decode, prepare register fetch
  val registers = Module(new RegisterFile(debug))

  val dbg = if (debug) Some(IO(Output(new Debug()))) else None
  if (debug) {
    dbg.get.regs := registers.dbg.get
    dbg.get.pc := pc
    dbg.get.writebackPc := MEM_WB_reg.wb.pc
  }

  val decoder = Module(new Decoder)
  decoder.io.instr := Mux(flush, Instruction.NOP, io.instrPort.instr)

  registers.io.readReg1 := decoder.io.ex.rs1
  registers.io.readReg2 := decoder.io.ex.rs2

  ID_EX_reg := decoder.io
  ID_EX_reg.wb.pc := pc
  when(stall) {
    ID_EX_reg := ID_EX_reg
  }
  when(flush) {
    ID_EX_reg := ResetPipeline.ID_EX()
  }

  // STAGE: Execute, decide branch

  val hazardExMemRs1 =
    (EX_MEM_reg.wb.rd === ID_EX_reg.ex.rs1) && EX_MEM_reg.wb.writeEnable && (EX_MEM_reg.wb.rd =/= 0.U)
  val hazardExMemRs2 =
    (EX_MEM_reg.wb.rd === ID_EX_reg.ex.rs2) && EX_MEM_reg.wb.writeEnable && (EX_MEM_reg.wb.rd =/= 0.U)

  val hazardExWbRs1 =
    (MEM_WB_reg.wb.rd === ID_EX_reg.ex.rs1) && MEM_WB_reg.wb.writeEnable && (MEM_WB_reg.wb.rd =/= 0.U)
  val hazardExWbRs2 =
    (MEM_WB_reg.wb.rd === ID_EX_reg.ex.rs2) && MEM_WB_reg.wb.writeEnable && (MEM_WB_reg.wb.rd =/= 0.U)

  val rs1 = Mux(
    hazardExMemRs1,
    EX_MEM_reg.wb.aluResult,
    Mux(hazardExWbRs1, opResult, registers.io.reg1Data)
  )
  val rs2 = Mux(
    hazardExMemRs2,
    EX_MEM_reg.wb.aluResult,
    Mux(hazardExWbRs2, opResult, registers.io.reg2Data)
  )

  if (debug) {
    dontTouch(hazardExMemRs1)
    dontTouch(hazardExMemRs2)
    dontTouch(hazardExWbRs1)
    dontTouch(hazardExWbRs2)
    dontTouch(rs1)
    dontTouch(rs2)
  }

  val alu = Module(new ALU())
  alu.io.op := ID_EX_reg.ex.aluOp
  alu.io.a := Mux(
    ID_EX_reg.ex.aluInput1 === ALUInput1.Rs1,
    rs1,
    ID_EX_reg.wb.pc
  ).asSInt
  alu.io.b := Mux(
    ID_EX_reg.ex.aluInput2 === ALUInput2.Rs2,
    rs2.asSInt,
    ID_EX_reg.ex.imm
  )

  val branch = Module(new BranchLogic())
  branch.io.data1 := rs1.asSInt
  branch.io.data2 := rs2.asSInt
  branch.io.branchType := ID_EX_reg.ex.branchType

  EX_MEM_reg := ID_EX_reg
  EX_MEM_reg.wb.aluResult := alu.io.result.asUInt
  EX_MEM_reg.mem.rs2Data := rs2
  EX_MEM_reg.mem.branch := branch.io.takeBranch

  val isLoad =
    (EX_MEM_reg.mem.memOp === MemOp.Load || EX_MEM_reg.mem.memOp === MemOp.LoadUnsigned)
  val loadUseHazard =
    isLoad &&
      ((EX_MEM_reg.wb.rd === ID_EX_reg.ex.rs1) || (EX_MEM_reg.wb.rd === ID_EX_reg.ex.rs2)) &&
      (EX_MEM_reg.wb.rd =/= 0.U)

  if (debug) {
    dontTouch(loadUseHazard)
  }

  when(loadUseHazard) {
    stall := true.B
  }

  when(flush || stall) {
    EX_MEM_reg := ResetPipeline.EX_MEM()
  }

  // We dont support unaligned mem access
  when(EX_MEM_reg.mem.memOp =/= MemOp.Noop) {
    when(EX_MEM_reg.mem.memSize === MemSize.Word) {
      assert(
        EX_MEM_reg.wb.aluResult(1, 0) === 0.U,
        "Unaligned WORD access | pc=0x%x addr=0x%x memOp=%d memSize=%d\n",
        EX_MEM_reg.wb.pc,
        EX_MEM_reg.wb.aluResult,
        EX_MEM_reg.mem.memOp.asUInt,
        EX_MEM_reg.mem.memSize.asUInt
      )
    }
    when(EX_MEM_reg.mem.memSize === MemSize.HalfWord) {
      assert(
        EX_MEM_reg.wb.aluResult(0) === 0.U,
        "Unaligned HALFWORD access | pc=0x%x addr=0x%x memOp=%d memSize=%d\n",
        EX_MEM_reg.wb.pc,
        EX_MEM_reg.wb.aluResult,
        EX_MEM_reg.mem.memOp.asUInt,
        EX_MEM_reg.mem.memSize.asUInt
      )
    }
  }

  io.dataPort.addr := EX_MEM_reg.wb.aluResult
  io.dataPort.memOp := EX_MEM_reg.mem.memOp
  io.dataPort.enable := (EX_MEM_reg.mem.memOp =/= MemOp.Noop)
  io.dataPort.dataWrite := EX_MEM_reg.mem.rs2Data
  io.dataPort.memSize := EX_MEM_reg.mem.memSize

  when(EX_MEM_reg.mem.branch) {
    flush := true.B
    nextPc := EX_MEM_reg.wb.aluResult
  }

  MEM_WB_reg := EX_MEM_reg

  opResult := MuxLookup(MEM_WB_reg.wb.writeSource, MEM_WB_reg.wb.aluResult)(
    Seq(
      WriteSource.Memory -> io.dataPort.dataRead,
      WriteSource.Pc -> (MEM_WB_reg.wb.pc + 4.U)
    )
  )

  registers.io.writeData := opResult
  registers.io.wrEn := MEM_WB_reg.wb.writeEnable
  registers.io.writeReg := MEM_WB_reg.wb.rd

}
