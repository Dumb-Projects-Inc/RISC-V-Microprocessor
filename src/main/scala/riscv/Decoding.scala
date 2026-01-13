package riscv

import chisel3._
import chisel3.util._

object ALUInput1 extends ChiselEnum {
  val Rs1, Pc = Value
}

object ALUInput2 extends ChiselEnum {
  val Rs2, Imm = Value
}

object ALUOp extends ChiselEnum {
  val Add, Sub, Sll, Slt, Sltu, Xor, Srl, Sra, Or, And, Noop = Value
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

// TODO: Consider using https://www.chisel-lang.org/docs/explanations/decoder to minimize logic
class Decoder extends Module {
  val io = IO(new Bundle {
    val instr = Input(UInt(32.W))
    val id = Output(new ControlSignals.ID())
    val ex = Output(new ControlSignals.EX())
    val wb = Output(new ControlSignals.WB())
  })

  io.ex := DontCare
  io.ex.memOp := MemOp.Noop

  io.wb := DontCare
  io.wb.writeEnable := false.B
  io.wb.branchType := BranchType.NO
  io.wb.rd := io.instr(11, 7)

  io.id := DontCare
  io.id.rs1 := io.instr(19, 15)
  io.id.rs2 := io.instr(24, 20)

  val format = Wire(Format())
  format := DontCare

  val immGen = Module(new ImmGen)
  immGen.io.instr := io.instr
  immGen.io.format := format
  io.ex.imm := immGen.io.out

  when(io.instr === Instruction.ADDI) {
    io.ex.aluInput1Source := ALUInput1.Rs1
    io.ex.aluInput2Source := ALUInput2.Imm
    io.wb.aluOp := ALUOp.Add
    io.wb.writeEnable := true.B
    io.wb.writeSource := WriteSource.ALU
    format := Format.I
  }
  when(io.instr === Instruction.ADD) {
    io.wb.aluOp := ALUOp.Add
    io.ex.aluInput1Source := ALUInput1.Rs1
    io.ex.aluInput2Source := ALUInput2.Rs2
    io.wb.writeEnable := true.B
    io.wb.writeSource := WriteSource.ALU
    format := Format.R
  }
  when(io.instr === Instruction.LUI) {
    io.wb.aluOp := ALUOp.Noop
    io.ex.aluInput2Source := ALUInput2.Imm
    io.wb.writeEnable := true.B
    io.wb.writeSource := WriteSource.ALU
    format := Format.U
  }
  // when(io.instr === Instruction.AUIPC) {
  //   io.wb.aluOp := ALUOp.Add
  //   io.ex.aluInput1 := ALUInput1.Pc
  //   io.ex.aluInput2 := ALUInput2.Imm
  //   io.wb.writeEnable := false.B
  //   io.wb.writeSource := WriteSource.ALU
  //   format := Format.U
  // }
  when(io.instr === Instruction.LW) {
    io.wb.writeEnable := true.B
    io.wb.writeSource := WriteSource.Memory
    io.ex.memOp := MemOp.Load
    io.ex.memSize := MemSize.Word
    format := Format.I
  }
  when(io.instr === Instruction.SW) {
    io.ex.memOp := MemOp.Store
    io.ex.memSize := MemSize.Word
    format := Format.S
  }

  // Branches
  when(io.instr === Instruction.JAL) {
    io.wb.aluOp := ALUOp.Add
    io.ex.aluInput1Source := ALUInput1.Pc
    io.ex.aluInput2Source := ALUInput2.Imm
    io.wb.branchType := BranchType.J
    io.wb.writeSource := WriteSource.Pc
    io.wb.writeEnable := true.B
    format := Format.J
  }
  when(io.instr === Instruction.JALR) {
    io.wb.aluOp := ALUOp.Add
    io.ex.aluInput2Source := ALUInput2.Imm
    io.wb.branchType := BranchType.J
    io.wb.writeSource := WriteSource.Pc
    io.wb.writeEnable := true.B
    format := Format.I
  }
  when(io.instr === Instruction.BEQ) {
    io.ex.aluInput1Source := ALUInput1.Pc
    io.ex.aluInput2Source := ALUInput2.Imm
    io.wb.aluOp := ALUOp.Add
    io.wb.branchType := BranchType.BEQ
    format := Format.B
  }
  when(io.instr === Instruction.BNE) {
    io.ex.aluInput1Source := ALUInput1.Pc
    io.ex.aluInput2Source := ALUInput2.Imm
    io.wb.aluOp := ALUOp.Add
    io.wb.branchType := BranchType.BNE
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
