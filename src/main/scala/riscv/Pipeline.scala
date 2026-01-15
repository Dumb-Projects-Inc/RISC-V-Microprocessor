package riscv

import chisel3._
import chisel3.util._
import riscv.Pipeline.EX_WB

object ControlSignals {
  class EX extends Bundle {
    val memOp = MemOp()
    val memSize = MemSize()
    val imm = SInt(32.W)
  }
  class WB extends Bundle {
    val pc = UInt(32.W)
    val aluInput1Source = ALUInput1()
    val aluInput2Source = ALUInput2()
    val aluInput1 = SInt(32.W)
    val aluInput2 = SInt(32.W)
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
  val opResult = Wire(UInt(32.W))
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

  // STAGE: fetch regs
  val registers = Module(new RegisterFile(debug))

  val dbg = if (debug) Some(IO(Output(new Debug()))) else None
  if (debug) {
    dbg.get.regs := registers.dbg.get
    dbg.get.pc := pc
  }

  registers.io.readReg1 := io.instrPort.instr(19, 15)
  registers.io.readReg2 := io.instrPort.instr(24, 20)

  val instrReg = RegInit(Instruction.NOP)
  instrReg := io.instrPort.instr
  when(flush) {
    instrReg := Instruction.NOP
  }

  // STAGE: Retrieve memory and prepare ALU inputs
  val decoder = Module(new Decoder)

  decoder.io.instr := instrReg

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

  val forwardMem =
    EX_WB_REG.wb.writeEnable === true.B &&
      EX_WB_REG.wb.rd =/= 0.U

  val forwardMemData = forwardMem && (decoder.io.wb.rs2 === EX_WB_REG.wb.rd)
  val forwardMemAddr = forwardMem && (decoder.io.wb.rs1 === EX_WB_REG.wb.rd)

  io.dataPort.addr := Mux(
    forwardMemAddr,
    opResult.asSInt + decoder.io.ex.imm,
    registers.io.reg1Data.asSInt + decoder.io.ex.imm
  ).asUInt

  io.dataPort.dataWrite := Mux(
    forwardMemData,
    opResult,
    registers.io.reg2Data
  )

  io.dataPort.enable := (decoder.io.ex.memOp =/= MemOp.Noop) && !flush
  io.dataPort.writeEn := decoder.io.ex.memOp === MemOp.Store

  EX_WB_REG.wb := decoder.io.wb

  EX_WB_REG.wb.rs1Data := registers.io.reg1Data
  EX_WB_REG.wb.rs2Data := registers.io.reg2Data

  EX_WB_REG.wb.aluInput1 := Mux(
    decoder.io.wb.aluInput1Source === ALUInput1.Rs1,
    registers.io.reg1Data,
    RegNext(pc)
  ).asSInt

  EX_WB_REG.wb.aluInput2 := Mux(
    decoder.io.wb.aluInput2Source === ALUInput2.Rs2,
    registers.io.reg2Data.asSInt,
    decoder.io.ex.imm
  )

  when(flush) {
    EX_WB_REG.wb.writeEnable := false.B
    EX_WB_REG.wb.branchType := BranchType.NO
    EX_WB_REG.wb.rd := 0.U
  }

  // STAGE: ALU and WB
  val alu = Module(new ALU())
  val aluInput1 = EX_WB_REG.wb.aluInput1
  val aluInput2 = EX_WB_REG.wb.aluInput2

  opResult := MuxLookup(EX_WB_REG.wb.writeSource, aluResult)(
    Seq(
      WriteSource.Memory -> io.dataPort.dataRead,
      WriteSource.Pc -> (RegNext(RegNext(pc)) + 4.U)
    )
  )

  val forward1 =
    (EX_WB_REG.wb.rs1 === RegNext(EX_WB_REG.wb.rd)) &&
      RegNext(EX_WB_REG.wb.writeEnable) &&
      RegNext(EX_WB_REG.wb.rd =/= 0.U)

  val forwardAlu1 =
    (EX_WB_REG.wb.aluInput1Source === ALUInput1.Rs1) && forward1

  val forward2 =
    (EX_WB_REG.wb.rs2 === RegNext(EX_WB_REG.wb.rd)) &&
      RegNext(EX_WB_REG.wb.writeEnable) &&
      RegNext(EX_WB_REG.wb.rd =/= 0.U)

  val forwardAlu2 =
    (EX_WB_REG.wb.aluInput2Source === ALUInput2.Rs2) && forward2

  alu.io.a := Mux(forwardAlu1, RegNext(opResult).asSInt, aluInput1)
  alu.io.b := Mux(forwardAlu2, RegNext(opResult).asSInt, aluInput2)
  alu.io.op := EX_WB_REG.wb.aluOp
  aluResult := alu.io.result.asUInt

  val branch = Module(new BranchLogic())
  branch.io.data1 := Mux(
    forward1,
    RegNext(opResult).asSInt,
    EX_WB_REG.wb.rs1Data.asSInt
  )
  branch.io.data2 := Mux(
    forward2,
    RegNext(opResult).asSInt,
    EX_WB_REG.wb.rs2Data.asSInt
  )
  branch.io.branchType := EX_WB_REG.wb.branchType

  when(branch.io.takeBranch) {
    nextPc := aluResult
    flush := true.B
  }

  registers.io.writeData := opResult
  registers.io.wrEn := EX_WB_REG.wb.writeEnable
  registers.io.writeReg := EX_WB_REG.wb.rd

}
