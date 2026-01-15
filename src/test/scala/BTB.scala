package riscv

import chisel3._
import org.scalatest.funspec.AnyFunSpec
import chisel3.simulator.scalatest.ChiselSim

class BTBSpec extends AnyFunSpec with ChiselSim {
  describe("BTB") {
    it("should predict branches correctly") {
      simulate(new riscv.branchPred.BTB(16)) { dut => }
    }
  }
}
