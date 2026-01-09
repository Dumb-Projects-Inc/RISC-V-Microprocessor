package riscv

import chisel3._
import riscv.HazardUnit.IdInfo
import riscv.HazardUnit.ExInfo
import riscv.HazardUnit.Out

object HazardUnit {
  class IdInfo extends Bundle {
    val rs1 = UInt(5.W)
    val rs2 = UInt(5.W)
    val usesRs1 = Bool()
    val usesRs2 = Bool()
  }

  class ExInfo extends Bundle {
    val rd = UInt(5.W)
    val isLoad = Bool()
  }

  class Out extends Bundle {
    val stallPC = Bool()
    val stallIFID = Bool()
    val flushIFID = Bool()
    val flushIDEX = Bool()
  }
}

class HazardUnit extends Module {
  val io = IO(new Bundle {
    val id = Input(new IdInfo)
    val ex = Input(new ExInfo)
    val exRedirect = Input(Bool())
    val out = Output(new Out)
  })

  val UsefulEX = io.ex.rd =/= 0.U
  val Rs1Hazerd = io.id.usesRs1 && UsefulEX && (io.ex.rd === io.id.rs1)
  val Rs2Hazerd = io.id.usesRs2 && UsefulEX && (io.ex.rd === io.id.rs2)
  val loadHazard = io.ex.isLoad && (Rs1Hazerd || Rs2Hazerd)

  io.out.stallPC := loadHazard && !io.exRedirect
  io.out.stallIFID := loadHazard && !io.exRedirect

  io.out.flushIFID := io.exRedirect
  io.out.flushIDEX := io.exRedirect || loadHazard
}
