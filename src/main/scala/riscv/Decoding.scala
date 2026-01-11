package riscv

import chisel3._
import chisel3.util._

object ALUInput1 extends ChiselEnum {
  val Rs1, Pc, Zero = Value
}

object ALUInput2 extends ChiselEnum {
  val Rs2, Imm = Value
}

object ALUOp extends ChiselEnum {
  val Add, Sub, Sll, Slt, Sltu, Xor, Srl, Sra, Or, And = Value
}

object WriteSource extends ChiselEnum {
  val ALU, Memory, Pc = Value
}

object MemOp extends ChiselEnum {
  val Noop, Store, Load = Value
}

object MemSize extends ChiselEnum {
  val Word, HalfWord, Byte = Value
}

object BranchType extends ChiselEnum {
  val BEQ, BNE, NO, J, BLT, BGE, BLTU, BGEU = Value
}

object Format extends ChiselEnum {
  val R, I, S, B, U, J = Value
}

object ControlSignals {
  class EX extends Bundle {
    val aluOp = ALUOp()
    val aluInput1 = ALUInput1()
    val aluInput2 = ALUInput2()
    val branchType = BranchType()
  }
  class MEM extends Bundle {
    val memOp = MemOp()
    val memSize = MemSize()
  }
  class WB extends Bundle {
    val writeEnable = Bool()
    val writeSource = WriteSource()
  }
}

// TODO: Consider using https://www.chisel-lang.org/docs/explanations/decoder to minimize logic
class Decoder extends Module {
  val io = IO(new Bundle {
    val instr = Input(UInt(32.W))
    val rd = Output(UInt(5.W))
    val rs1 = Output(UInt(5.W))
    val rs2 = Output(UInt(5.W))
    val imm = Output(SInt(32.W))
    val control = new Bundle {
      val ex = Output(new ControlSignals.EX())
      val mem = Output(new ControlSignals.MEM())
      val wb = Output(new ControlSignals.WB())
    }
    val uses = Output(new Bundle {
      val rs1 = Bool()
      val rs2 = Bool()
    })
  })

  // val opcode = io.instr(6, 0)
  // val funct3 = io.instr(14, 12)
  // val funct7 = io.instr(31, 25)

  io.rd := io.instr(11, 7)
  io.rs1 := io.instr(19, 15)
  io.rs2 := io.instr(24, 20)

  io.control.ex := DontCare
  io.control.ex.branchType := BranchType.NO

  io.control.mem := DontCare
  io.control.mem.memOp := MemOp.Noop

  io.control.wb := DontCare
  io.control.wb.writeEnable := false.B

  val format = Wire(Format())
  format := DontCare
  val immGen = Module(new ImmGen)
  immGen.io.instr := io.instr
  immGen.io.format := format
  io.imm := immGen.io.out

  io.uses.rs1 := false.B
  io.uses.rs2 := false.B
  switch(format) {
    is(Format.R) {
      io.uses.rs1 := true.B
      io.uses.rs2 := true.B
    }
    is(Format.I) {
      io.uses.rs1 := true.B
    }
    is(Format.S) {
      io.uses.rs1 := true.B
      io.uses.rs2 := true.B
    }
    is(Format.B) {
      io.uses.rs1 := true.B
      io.uses.rs2 := true.B
    }
  }

  when(io.instr === Instruction.ADDI) {
    io.control.ex.aluOp := ALUOp.Add
    io.control.ex.aluInput1 := ALUInput1.Rs1
    io.control.ex.aluInput2 := ALUInput2.Imm
    io.control.wb.writeEnable := true.B
    io.control.wb.writeSource := WriteSource.ALU
    format := Format.I
  }
  when(io.instr === Instruction.ADD) {
    io.control.ex.aluOp := ALUOp.Add
    io.control.ex.aluInput1 := ALUInput1.Rs1
    io.control.ex.aluInput2 := ALUInput2.Rs2
    io.control.wb.writeEnable := true.B
    io.control.wb.writeSource := WriteSource.ALU
    format := Format.R
  }
  when(io.instr === Instruction.LUI) {
    io.control.ex.aluOp := ALUOp.Add
    io.control.ex.aluInput1 := ALUInput1.Zero // actually 0
    io.control.ex.aluInput2 := ALUInput2.Imm
    io.control.wb.writeEnable := true.B
    io.control.wb.writeSource := WriteSource.ALU
    format := Format.U
  }
  when(io.instr === Instruction.LW) {
    io.control.ex.aluOp := ALUOp.Add
    io.control.ex.aluInput1 := ALUInput1.Rs1
    io.control.ex.aluInput2 := ALUInput2.Imm
    io.control.wb.writeEnable := true.B
    io.control.wb.writeSource := WriteSource.Memory
    io.control.mem.memOp := MemOp.Load
    io.control.mem.memSize := MemSize.Word
    format := Format.I
  }
  when(io.instr === Instruction.SW) {
    io.control.ex.aluOp := ALUOp.Add
    io.control.ex.aluInput1 := ALUInput1.Rs1
    io.control.ex.aluInput2 := ALUInput2.Imm
    io.control.mem.memOp := MemOp.Store
    io.control.mem.memSize := MemSize.Word
    format := Format.S
  }

  // Branches
  when(io.instr === Instruction.JAL) {
    io.control.ex.aluOp := ALUOp.Add
    io.control.ex.aluInput1 := ALUInput1.Pc
    io.control.ex.aluInput2 := ALUInput2.Imm
    io.control.ex.branchType := BranchType.J
    io.control.wb.writeSource := WriteSource.Pc
    io.control.wb.writeEnable := true.B
    format := Format.J
  }
  when(io.instr === Instruction.JALR) {
    io.control.ex.aluOp := ALUOp.Add
    io.control.ex.aluInput1 := ALUInput1.Rs1
    io.control.ex.aluInput2 := ALUInput2.Imm
    io.control.ex.branchType := BranchType.J
    io.control.wb.writeSource := WriteSource.Pc
    io.control.wb.writeEnable := true.B
    format := Format.I
  }
  when(io.instr === Instruction.BEQ) {
    io.control.ex.aluInput1 := ALUInput1.Pc
    io.control.ex.aluInput2 := ALUInput2.Imm
    io.control.ex.aluOp := ALUOp.Add
    io.control.ex.branchType := BranchType.BEQ
    format := Format.B
  }
  when(io.instr === Instruction.BNE) {
    io.control.ex.aluInput1 := ALUInput1.Pc
    io.control.ex.aluInput2 := ALUInput2.Imm
    io.control.ex.aluOp := ALUOp.Add
    io.control.ex.branchType := BranchType.BNE
    format := Format.B
  }

}

class ImmGen extends Module {
  val io = IO(new Bundle {
    val instr = Input(Bits(32.W))
    val format = Input(Format())
    val out = Output(SInt(32.W))
  })

  io.out := DontCare
  switch(io.format) {
    is(Format.I) {
      io.out := io.instr(31, 20).asSInt
    }
    is(Format.S) {
      io.out := (io.instr(31, 25) ## io.instr(11, 7)).asSInt
    }
    is(Format.B) {
      io.out := (io.instr(31) ## io.instr(7) ## io.instr(30, 25) ## io.instr(
        11,
        8
      ) ## 0.U(1.W)).asSInt
    }
    is(Format.U) {
      io.out := (io.instr(31, 12) ## 0.U(12.W)).asSInt
    }
    is(Format.J) {
      io.out := (io.instr(31) ## io.instr(19, 12) ## io
        .instr(20) ## io.instr(30, 21) ## 0.U(1.W)).asSInt
    }
    // TODO: Z-Format / System instructions
  }

}
