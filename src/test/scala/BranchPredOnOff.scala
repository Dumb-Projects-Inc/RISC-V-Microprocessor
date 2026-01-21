package riscv

import chisel3._
import os._
import chisel3.simulator.scalatest.ChiselSim
import chisel3.util.experimental.loadMemoryFromFile
import riscv.memory.AssemblerCompat

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.Tag

object BranchPredOnOff extends Tag("BranchPredOnOff")

class TestTopPred(instr: String) extends Module {
  val io = IO(new Bundle {
    val dbg = Output(Vec(32, UInt(32.W)))
    val pc = Output(UInt(32.W))
  })

  val pipeline = Module(
    new Pipeline(debug = true, debugPrint = false, useBranchPred = true)
  )
  io.dbg := pipeline.dbg.get.regs
  io.pc := pipeline.dbg.get.pc

  val program = SyncReadMem(1024, UInt(32.W))
  val hex = AssemblerCompat.fromString(instr.stripMargin)
  val tempFile = os.temp(suffix = ".hex")
  os.write.over(tempFile, hex)
  loadMemoryFromFile(program, tempFile.toString)

  pipeline.io.instrPort.stall := false.B
  pipeline.io.instrPort.instr := program.read(
    pipeline.io.instrPort.addr(31, 2),
    pipeline.io.instrPort.enable
  )

  val dmem = SyncReadMem(1024, UInt(32.W))
  val dmemAddr = pipeline.io.dataPort.addr(31, 2)
  pipeline.io.dataPort.dataRead := dmem.read(
    dmemAddr,
    pipeline.io.dataPort.enable
  )
  when(
    (pipeline.io.dataPort.memOp === MemOp.Store) && pipeline.io.dataPort.enable
  ) {
    dmem.write(dmemAddr, pipeline.io.dataPort.dataWrite)
  }
  pipeline.io.dataPort.stall := false.B
}

class TestTopNoPred(instr: String) extends Module {
  val io = IO(new Bundle {
    val dbg = Output(Vec(32, UInt(32.W)))
    val pc = Output(UInt(32.W))
  })

  val pipeline = Module(
    new Pipeline(debug = true, debugPrint = false, useBranchPred = false)
  )
  io.dbg := pipeline.dbg.get.regs
  io.pc := pipeline.dbg.get.pc

  val program = SyncReadMem(1024, UInt(32.W))
  val hex = AssemblerCompat.fromString(instr.stripMargin)
  val tempFile = os.temp(suffix = ".hex")
  os.write.over(tempFile, hex)
  loadMemoryFromFile(program, tempFile.toString)

  pipeline.io.instrPort.stall := false.B
  pipeline.io.instrPort.instr := program.read(
    pipeline.io.instrPort.addr(31, 2),
    pipeline.io.instrPort.enable
  )

  val dmem = SyncReadMem(1024, UInt(32.W))
  val dmemAddr = pipeline.io.dataPort.addr(31, 2)
  pipeline.io.dataPort.dataRead := dmem.read(
    dmemAddr,
    pipeline.io.dataPort.enable
  )
  when(
    (pipeline.io.dataPort.memOp === MemOp.Store) && pipeline.io.dataPort.enable
  ) {
    dmem.write(dmemAddr, pipeline.io.dataPort.dataWrite)
  }
  pipeline.io.dataPort.stall := false.B
}

class BranchPredOnOffSpec extends AnyFunSpec with ChiselSim {

  private def resetDutPred(dut: TestTopPred): Unit = {
    dut.reset.poke(true.B)
    dut.clock.step(1)
    dut.reset.poke(false.B)
  }

  private def resetDutNoPred(dut: TestTopNoPred): Unit = {
    dut.reset.poke(true.B)
    dut.clock.step(1)
    dut.reset.poke(false.B)
  }

  private def runUntilRegEqualsPred(
      dut: TestTopPred,
      regIdx: Int,
      value: BigInt,
      maxCycles: Int
  ): Int = {
    var cycles = 0
    while (cycles < maxCycles && dut.io.dbg(regIdx).peek().litValue != value) {
      dut.clock.step(1)
      cycles += 1
    }
    cycles
  }

  private def runUntilRegEqualsNoPred(
      dut: TestTopNoPred,
      regIdx: Int,
      value: BigInt,
      maxCycles: Int
  ): Int = {
    var cycles = 0
    while (cycles < maxCycles && dut.io.dbg(regIdx).peek().litValue != value) {
      dut.clock.step(1)
      cycles += 1
    }
    cycles
  }

  describe("Branch prediction ON vs OFF") {

    it("prediction ON", BranchPredOnOff) {
      val n = 2000

      val input =
        s"""
          addi x1, x0, 0
          addi x2, x0, $n
          addi x1, x1, 1
          bne  x1, x2, -4
          addi x3, x0, 123
          addi x0, x0, 0
          addi x0, x0, 0
          addi x0, x0, 0
        """

      simulate(new TestTopPred(input)) { dut =>
        resetDutPred(dut)
        val cycles =
          runUntilRegEqualsPred(
            dut,
            regIdx = 3,
            value = 123,
            maxCycles = 5_000_000
          )
        assert(dut.io.dbg(3).peek().litValue == 123)
        info(s"[BP=ON]  cycles to x3=123: $cycles (n=$n)")
      }
    }

    it("prediction OFF", BranchPredOnOff) {
      val n = 2000

      val input =
        s"""
          addi x1, x0, 0
          addi x2, x0, $n
          addi x1, x1, 1
          bne  x1, x2, -4
          addi x3, x0, 123
          addi x0, x0, 0
          addi x0, x0, 0
          addi x0, x0, 0
        """

      simulate(new TestTopNoPred(input)) { dut =>
        resetDutNoPred(dut)
        val cycles =
          runUntilRegEqualsNoPred(
            dut,
            regIdx = 3,
            value = 123,
            maxCycles = 20_000_000
          )
        assert(dut.io.dbg(3).peek().litValue == 123)
        info(s"[BP=OFF] cycles to x3=123: $cycles (n=$n)")
      }
    }
  }
}
