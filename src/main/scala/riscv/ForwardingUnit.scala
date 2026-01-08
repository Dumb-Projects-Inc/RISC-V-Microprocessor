package riscv

import chisel3._
import chisel3.util._

// https://cepdnaclk.github.io/e16-co502-RV32IM-pipeline-implementation-group1/5-handling_hazards/3-forwarding_unit.html
// The following comments are directly taken from the above link:

// Inputs‌ ‌to‌ ‌the‌ ‌Forwarding‌ ‌Unit‌ ‌are,‌ ‌

// OPCODE[6:0] ‌:‌ ‌Opcode‌ ‌from‌ ‌Current‌ ‌Instruction‌
// ADDR1[4:0]‌‌ ‌:‌ ‌Register‌ ‌Read‌ ‌Address‌ ‌1‌ ‌from‌ ‌Current‌ ‌Instruction‌ ‌
// ADDR2[4:0‌]‌ ‌: ‌Register‌ ‌Read‌ ‌Address‌ ‌2‌ ‌from‌ ‌Current‌ ‌Instruction‌
// OP1SEL‌‌ ‌:‌ ‌Generates‌ ‌from‌ ‌Control‌ ‌Unit‌ ‌
// OP2SEL‌‌ ‌:‌ ‌Generates‌ ‌from‌ ‌Control‌ ‌Unit‌ ‌
// WB_ADDR[4:0]‌‌ :‌ ‌Register‌ ‌Write‌ ‌Address‌ ‌from‌ ‌WriteBack‌ ‌Stage‌
// MEM_ADDR[4:0]‌‌ : ‌Register‌ ‌Write‌ ‌Address‌ ‌from‌ ‌Memory‌ ‌Stage‌**
// EXE_ADDR[4:0]‌‌ ‌:‌ ‌Register‌ ‌Write‌ ‌Address‌ ‌from‌ ‌Execute‌ ‌Stage‌

// Outputs‌ ‌generated‌ ‌from‌ ‌the‌ ‌forwarding‌ ‌unit‌ ‌are,‌ ‌ ‌

// DATA1IDSEL‌‌ ‌: Signal‌ ‌for‌ ‌Data1‌ ‌selection‌ ‌Multiplexer‌ ‌in‌ ‌instruction‌ ‌decode‌ ‌stage‌‌
// DATA2IDSEL‌‌ ‌: ‌Signal‌ ‌for‌ ‌Data2‌ ‌selection‌ ‌Multiplexer‌ ‌in‌ ‌instruction‌ ‌decode‌ ‌stage‌
// DATA1ALUSEL‌ ‌[1:0]‌‌ ‌:‌ ‌Signal‌ ‌for‌ ‌ALU‌ ‌Data1‌ ‌input‌ ‌selection‌ ‌Multiplexer‌ ‌in‌ ‌EXE‌ ‌stage‌ ‌
// DATA2ALUSEL[1:0]‌ : ‌ ‌ ‌Signal‌ ‌for‌ ‌ALU‌ ‌Data2‌ ‌input‌ ‌selection‌ ‌Multiplexer‌ ‌in‌ ‌EXE‌ ‌stage‌ ‌
// DATA1BJSEL‌ ‌[1:0]‌‌ ‌: ‌Signal‌ ‌for‌ ‌Branch‌ ‌Logic‌ ‌Data1‌ ‌input‌ ‌selection‌ ‌Multiplexer‌ ‌in‌ ‌EXE‌ ‌stage‌
// DATA2BJSEL[1:0]‌ : ‌ ‌ ‌Signal‌ ‌for‌ ‌Branch‌ ‌Logic‌ ‌Data2‌ ‌input‌ ‌selection‌ ‌Multiplexer‌ ‌in‌ ‌EXE‌ ‌stage‌ ‌
// DATAMEMSEL‌ : ‌ ‌ ‌Signal‌ ‌for‌ ‌Memory‌ ‌In‌ ‌Data‌ ‌selection‌ ‌Multiplexer‌ ‌in‌ ‌Memory‌ ‌Stage

object ForwardingUnit {
  object Sel {
    val none = 0.U(2.W)
    val wb = 1.U(2.W)
    val mem = 2.U(2.W)
    val exe = 3.U(2.W)
  }

  class Uses extends Bundle {
    val aluRs1 = Bool()
    val aluRs2 = Bool()
    val bjRs1 = Bool()
    val bjRs2 = Bool()
    val idRs1 = Bool()
    val idRs2 = Bool()
    val storeDataRs2 = Bool()
  }

  class DecIn extends Bundle {
    val rs1 = UInt(5.W)
    val rs2 = UInt(5.W)
    val uses = new Uses
  }

  class StageInfo extends Bundle {
    val rd = UInt(5.W)
    val regWrite = Bool()
    val isLoad = Bool()
  }

  class SelectOut extends Bundle {
    val data1IDSel = Bool()
    val data2IDSel = Bool()
    val data1ALUSel = UInt(2.W)
    val data2ALUSel = UInt(2.W)
    val data1BJSel = UInt(2.W)
    val data2BJSel = UInt(2.W)
    val dataMemSel = UInt(2.W)
  }
}

class ForwardingUnit extends Module {
  import ForwardingUnit._

  val io = IO(new Bundle {
    val dec = Input(new DecIn)
    val exe = Input(new StageInfo)
    val mem = Input(new StageInfo)
    val wb = Input(new StageInfo)
    val sel = Output(new SelectOut)
  })

  io.sel.data1IDSel := false.B
  io.sel.data2IDSel := false.B
  io.sel.data1ALUSel := Sel.none
  io.sel.data2ALUSel := Sel.none
  io.sel.data1BJSel := Sel.none
  io.sel.data2BJSel := Sel.none
  io.sel.dataMemSel := Sel.none

  // checks if the destination register
  def matches(stage: StageInfo, reg: UInt): Bool =
    stage.regWrite && stage.rd =/= 0.U && stage.rd === reg

  // checks if we are writing from ALU and not from load
  def matchesExe(stage: StageInfo, reg: UInt): Bool =
    matches(stage, reg) && !stage.isLoad

  // forward ALU operand 1
  when(io.dec.uses.aluRs1) {
    when(matchesExe(io.exe, io.dec.rs1)) {
      io.sel.data1ALUSel := Sel.exe
    }.elsewhen(matches(io.mem, io.dec.rs1)) {
      io.sel.data1ALUSel := Sel.mem
    }.elsewhen(matches(io.wb, io.dec.rs1)) {
      io.sel.data1ALUSel := Sel.wb
    }
  }

  // forward ALU operand 2
  when(io.dec.uses.aluRs2) {
    when(matchesExe(io.exe, io.dec.rs2)) {
      io.sel.data2ALUSel := Sel.exe
    }.elsewhen(matches(io.mem, io.dec.rs2)) {
      io.sel.data2ALUSel := Sel.mem
    }.elsewhen(matches(io.wb, io.dec.rs2)) {
      io.sel.data2ALUSel := Sel.wb
    }
  }

  // forward Branch/jump operand 1
  when(io.dec.uses.bjRs1) {
    when(matchesExe(io.exe, io.dec.rs1)) {
      io.sel.data1BJSel := Sel.exe
    }.elsewhen(matches(io.mem, io.dec.rs1)) {
      io.sel.data1BJSel := Sel.mem
    }.elsewhen(matches(io.wb, io.dec.rs1)) {
      io.sel.data1BJSel := Sel.wb
    }
  }

  // forward Branch/jump operand 2
  when(io.dec.uses.bjRs2) {
    when(matchesExe(io.exe, io.dec.rs2)) {
      io.sel.data2BJSel := Sel.exe
    }.elsewhen(matches(io.mem, io.dec.rs2)) {
      io.sel.data2BJSel := Sel.mem
    }.elsewhen(matches(io.wb, io.dec.rs2)) {
      io.sel.data2BJSel := Sel.wb
    }
  }

  // forward ID stage operands
  io.sel.data1IDSel := io.dec.uses.idRs1 && matches(io.wb, io.dec.rs1)
  io.sel.data2IDSel := io.dec.uses.idRs2 && matches(io.wb, io.dec.rs2)

  // forward store data
  when(io.dec.uses.storeDataRs2) {
    when(matchesExe(io.exe, io.dec.rs2)) {
      io.sel.dataMemSel := Sel.exe
    }.elsewhen(matches(io.mem, io.dec.rs2)) {
      io.sel.dataMemSel := Sel.mem
    }.elsewhen(matches(io.wb, io.dec.rs2)) {
      io.sel.dataMemSel := Sel.wb
    }
  }

}
