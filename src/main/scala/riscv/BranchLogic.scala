package riscv

import chisel3._
import chisel3.util._

class BranchLogic extends Module {
  val io = IO(new Bundle {
    val data1 = Input(SInt(32.W))
    val data2 = Input(SInt(32.W))
    val branchType = Input(BranchType())
    val takeBranch = Output(Bool())
  })

  io.takeBranch := false.B
  switch(io.branchType) {
    is(BranchType.J) {
      io.takeBranch := true.B
    }
    is(BranchType.NO) {
      io.takeBranch := false.B
    }
    is(BranchType.BEQ) {
      io.takeBranch := (io.data1 === io.data2)
    }
    is(BranchType.BNE) {
      io.takeBranch := (io.data1 =/= io.data2)
    }
    is(BranchType.BLT) {
      io.takeBranch := (io.data1 < io.data2)
    }
    is(BranchType.BGE) {
      io.takeBranch := (io.data1 >= io.data2)
    }
    is(BranchType.BLTU) {
      io.takeBranch := (io.data1.asUInt < io.data2.asUInt)
    }
    is(BranchType.BGEU) {
      io.takeBranch := (io.data1.asUInt >= io.data2.asUInt)
    }
  }

}
