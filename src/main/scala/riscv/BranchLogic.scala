package riscv

import chisel3._
import chisel3.util._

class BranchLogic extends Module {
  val io = IO(new Bundle {
    val data1 = Input(SInt(32.W))
    val data2 = Input(SInt(32.W))
    val branchJump = Input(BranchType())
    val pcSelect = Output(Bool())
  })

  io.pcSelect := false.B
  switch(io.branchJump) {
    is(BranchType.J) {
      io.pcSelect := true.B
    }
    is(BranchType.NO) {
      io.pcSelect := false.B
    }
    is(BranchType.BEQ) {
      io.pcSelect := (io.data1 === io.data2)
    }
    is(BranchType.BNE) {
      io.pcSelect := (io.data1 =/= io.data2)
    }
    is(BranchType.BLT) {
      io.pcSelect := (io.data1 < io.data2)
    }
    is(BranchType.BGE) {
      io.pcSelect := (io.data1 >= io.data2)
    }
    is(BranchType.BLTU) {
      io.pcSelect := (io.data1.asUInt < io.data2.asUInt)
    }
    is(BranchType.BGEU) {
      io.pcSelect := (io.data1.asUInt >= io.data2.asUInt)
    }
  }

}
