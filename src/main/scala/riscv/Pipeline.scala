package riscv

import chisel3._
import chisel3.util._

object ControlSignals {
  class EX extends Bundle {
    val memOp = MemOp()
    val memSize = MemSize()
  }
  class WB extends Bundle {
    val aluInput1Source = ALUInput1()
    val aluInput2Source = ALUInput2()
    val pc = UInt(32.W)
    val imm = SInt(32.W)
    val rs1Data = UInt(32.W)
    val rs2Data = UInt(32.W)
    val aluOp = ALUOp()
    val writeSource = WriteSource()
    val writeEnable = Bool()
    val branchType = BranchType()
    val rd = UInt(5.W)
    val rs1 = UInt(5.W)
    val rs2 = UInt(5.W)
  }
}

object Pipeline {
  class ID_EX extends Bundle {
    val ex = new ControlSignals.EX
    val wb = new ControlSignals.WB
  }
  class EX_WB extends Bundle {
    val wb = new ControlSignals.WB
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

  // Forward declarations
  val aluResult = Wire(UInt(32.W))
  val flush = WireDefault(false.B)

  // STAGE: Fetch instruction
  val pc = RegInit(0.U(32.W))

  val nextPc = WireDefault(pc + 4.U)
  pc := nextPc

  val first = RegNext(false.B, true.B)
  when(first) {
    nextPc := pc
    flush := true.B
  }

  io.instrPort.addr := nextPc
  io.instrPort.enable := true.B

  // STAGE: Decode control signals and fetch registers
  val ID_EX_REG = RegInit({
    val bundle = Wire(new Pipeline.ID_EX())
    bundle := DontCare
    bundle.ex.memOp := MemOp.Noop
    bundle.wb.writeEnable := false.B
    bundle.wb.branchType := BranchType.NO
    bundle.wb.rs2 := 0.U
    bundle.wb.rd := 0.U
    bundle
  })
  if (debug) {
    dontTouch(ID_EX_REG)
  }

  val decoder = Module(new Decoder)
  decoder.io.instr := Mux(flush, Instruction.NOP, io.instrPort.instr)

  if (debug) {
    dontTouch(io.instrPort.instr)
  }

  val registers = Module(new RegisterFile(debug))

  val dbg = if (debug) Some(IO(Output(new Debug()))) else None
  if (debug) {
    dbg.get.regs := registers.dbg.get
    dbg.get.pc := pc
  }

  registers.io.readReg1 := decoder.io.wb.rs1
  registers.io.readReg2 := decoder.io.wb.rs2

  ID_EX_REG.ex := decoder.io.ex
  ID_EX_REG.wb := decoder.io.wb
  ID_EX_REG.wb.pc := pc

  when(flush) {
    ID_EX_REG.ex.memOp := MemOp.Noop
    ID_EX_REG.wb.writeEnable := false.B
    ID_EX_REG.wb.branchType := BranchType.NO
    ID_EX_REG.wb.rs2 := 0.U
    ID_EX_REG.wb.rd := 0.U
  }

  // STAGE: Retrieve memory and prepare ALU inputs
  val EX_WB_REG = RegInit({
    val bundle = Wire(new Pipeline.EX_WB())
    bundle := DontCare
    bundle.wb.writeEnable := false.B
    bundle.wb.branchType := BranchType.NO
    bundle.wb.rd := 0.U
    bundle
  })

  if (debug) {
    dontTouch(EX_WB_REG)
  }

  val memoryAddress = registers.io.reg1Data.asSInt + ID_EX_REG.wb.imm
  io.dataPort.addr := memoryAddress.asUInt
  io.dataPort.enable := ID_EX_REG.ex.memOp =/= MemOp.Noop
  io.dataPort.writeEn := ID_EX_REG.ex.memOp === MemOp.Store
  io.dataPort.dataWrite := registers.io.reg2Data
  io.dataPort.dataWrite := Mux(
    ID_EX_REG.wb.rs2 === EX_WB_REG.wb.rd,
    aluResult,
    registers.io.reg2Data
  )

  EX_WB_REG.wb := ID_EX_REG.wb
  // EX_WB_REG.wb.aluInput1 := Mux(
  //   ID_EX_REG.ex.aluInput1Source === ALUInput1.Rs1,
  //   registers.io.reg1Data.asSInt,
  //   ID_EX_REG.wb.pc.asSInt
  // )
  EX_WB_REG.wb.rs1Data := registers.io.reg1Data
  // EX_WB_REG.wb.aluInput2 := Mux(
  //   ID_EX_REG.ex.aluInput2Source === ALUInput2.Rs2,
  //   registers.io.reg2Data.asSInt,
  //   ID_EX_REG.ex.imm
  // )
  EX_WB_REG.wb.rs2Data := registers.io.reg2Data

  when(flush) {
    EX_WB_REG.wb.writeEnable := false.B
    EX_WB_REG.wb.branchType := BranchType.NO
    EX_WB_REG.wb.rd := 0.U
  }

  // STAGE: ALU and WB

  val alu = Module(new ALU())
  val aluInput1 = Mux(
    EX_WB_REG.wb.aluInput1Source === ALUInput1.Rs1,
    EX_WB_REG.wb.rs1Data,
    EX_WB_REG.wb.pc
  ).asSInt
  val aluInput2 = Mux(
    EX_WB_REG.wb.aluInput2Source === ALUInput2.Rs2,
    EX_WB_REG.wb.rs2Data.asSInt,
    EX_WB_REG.wb.imm
  )
  alu.io.a := aluInput1
  alu.io.b := aluInput2
  alu.io.op := EX_WB_REG.wb.aluOp
  aluResult := alu.io.result.asUInt

  val branch = Module(new BranchLogic())
  branch.io.data1 := EX_WB_REG.wb.rs1Data.asSInt
  branch.io.data2 := EX_WB_REG.wb.rs2Data.asSInt
  branch.io.branchType := EX_WB_REG.wb.branchType

  when(branch.io.takeBranch) {
    nextPc := aluResult
    flush := true.B
  }

  val opResult = MuxLookup(EX_WB_REG.wb.writeSource, aluResult)(
    Seq(
      WriteSource.Memory -> io.dataPort.dataRead,
      WriteSource.Pc -> (EX_WB_REG.wb.pc + 4.U)
    )
  )
  registers.io.writeData := opResult
  registers.io.wrEn := EX_WB_REG.wb.writeEnable
  registers.io.writeReg := EX_WB_REG.wb.rd

}
