package riscv

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import chisel3._
import chisel3.simulator.scalatest.ChiselSim

class BranchLogicSpec extends AnyFunSpec with ChiselSim with Matchers {
  describe("BranchLogic for RV32I") {

    it("Selects the right PC based on branch conditions") {
      simulate(new BranchLogic()) { dut =>
        // Initialize inputs
        for (bt <- BranchType.all) {
          dut.io.branchType.poke(bt)
          dut.io.data1.poke(10.S)
          dut.io.data2.poke(10.S)
          dut.clock.step()

          bt match {
            case BranchType.BEQ =>
              dut.io.takeBranch.expect(true.B)
            case BranchType.BNE =>
              dut.io.takeBranch.expect(false.B)
            case BranchType.BLT =>
              dut.io.takeBranch.expect(false.B)
            case BranchType.BGE =>
              dut.io.takeBranch.expect(true.B)
            case BranchType.BLTU =>
              dut.io.takeBranch.expect(false.B)
            case BranchType.BGEU =>
              dut.io.takeBranch.expect(true.B)
            case BranchType.J =>
              dut.io.takeBranch.expect(true.B)
            case BranchType.NO =>
              dut.io.takeBranch.expect(false.B)
          }
        }
      }
    }
  }
}
