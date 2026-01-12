package riscv

import chisel3._
import chisel3.util._

object ControlSignals {
  class ID extends Bundle {
    val rs1 = UInt(5.W)
    val rs2 = UInt(5.W)
    val pc = UInt(32.W)
  }
  class EX extends Bundle {
    val aluInput1Source = ALUInput1()
    val aluInput2Source = ALUInput2()
    val memOp = MemOp()
    val memSize = MemSize()
    val imm = SInt(32.W)
  }
  class WB extends Bundle {
    val aluInput1 = SInt(32.W)
    val aluInput2 = SInt(32.W)
    val aluOp = ALUOp()
    val writeSource = WriteSource()
    val writeEnable = Bool()
    val branchType = BranchType()
    val rd = UInt(5.W)
    val pc = UInt(32.W)
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

  // STAGE: Fetch instruction
  val pc = RegInit(0.U(32.W))
  pc := pc + 4.U

  io.instrPort.addr := pc
  io.instrPort.enable := true.B

  // STAGE: Decode control signals and fetch registers
  val decoder = Module(new Decoder)
  decoder.io.instr := io.instrPort.instr

  if (debug) {
    dontTouch(io.instrPort.instr)
  }

  val registers = Module(new RegisterFile(debug))

  val dbg = if (debug) Some(IO(Output(new Debug()))) else None
  if (debug) {
    dbg.get.regs := registers.dbg.get
    dbg.get.pc := pc
  }

  registers.io.readReg1 := decoder.io.id.rs1
  registers.io.readReg2 := decoder.io.id.rs2

  // Register outputs for next stage
  val ID_EX_REG = RegInit({
    val bundle = Wire(new Pipeline.ID_EX())
    bundle := DontCare
    bundle.ex.memOp := MemOp.Noop
    bundle.wb.writeEnable := false.B
    bundle
  })

  if (debug) {
    dontTouch(ID_EX_REG)
  }

  ID_EX_REG.ex := decoder.io.ex
  ID_EX_REG.wb := decoder.io.wb
  ID_EX_REG.wb.pc := RegNext(pc)

  // STAGE: Retrieve memory and prepare ALU inputs

  val memoryAddress = registers.io.reg1Data.asSInt + ID_EX_REG.ex.imm
  io.dataPort.addr := memoryAddress.asUInt
  io.dataPort.enable := ID_EX_REG.ex.memOp =/= MemOp.Noop
  io.dataPort.writeEn := ID_EX_REG.ex.memOp === MemOp.Store
  io.dataPort.dataWrite := registers.io.reg2Data // TODO: Mux from ALU output on conflict

  val EX_WB_REG = RegInit({
    val bundle = Wire(new Pipeline.EX_WB())
    bundle := DontCare
    bundle.wb.writeEnable := false.B
    bundle
  })

  if (debug) {
    dontTouch(EX_WB_REG)
  }

  EX_WB_REG.wb := ID_EX_REG.wb
  EX_WB_REG.wb.aluInput1 := Mux(
    ID_EX_REG.ex.aluInput1Source === ALUInput1.Rs1,
    registers.io.reg1Data.asSInt,
    ID_EX_REG.wb.pc.asSInt
  )
  EX_WB_REG.wb.aluInput2 := Mux(
    ID_EX_REG.ex.aluInput2Source === ALUInput2.Rs2,
    registers.io.reg2Data.asSInt,
    ID_EX_REG.ex.imm
  )

  // STAGE: ALU and WB

  val alu = Module(new ALU())
  alu.io.a := EX_WB_REG.wb.aluInput1
  alu.io.b := EX_WB_REG.wb.aluInput2
  alu.io.op := EX_WB_REG.wb.aluOp
  val aluResult = alu.io.result.asUInt

  val branch = Module(new BranchLogic())
  branch.io.data1 := EX_WB_REG.wb.aluInput1
  branch.io.data2 := EX_WB_REG.wb.aluInput2
  branch.io.branchType := EX_WB_REG.wb.branchType

  when(branch.io.takeBranch) {
    pc := aluResult
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
