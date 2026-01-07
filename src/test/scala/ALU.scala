package riscv

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import chisel3._
import chisel3.simulator.scalatest.ChiselSim

class ALUSpec extends AnyFunSpec with ChiselSim with Matchers {
  describe("ALU for RV32I") {

    it("Switch operations successfully") {
      simulate(new ALU()) { dut =>
        // Initialize inputs
        dut.io.a.poke(20.S)
        dut.io.b.poke(10.S)
        dut.io.op.poke(ALUOp.Add)
        dut.clock.step()

        dut.io.result.expect(30.S)
        dut.io.op.poke(ALUOp.Sub)
        dut.clock.step()
        dut.io.result.expect(10.S)
      }
    }
    it("Handles negative numbers correctly") {
      simulate(new ALU()) { dut =>
        // Initialize inputs
        dut.io.a.poke((-10).S)
        dut.io.b.poke(5.S)
        dut.io.op.poke(ALUOp.Add)
        dut.clock.step()

        dut.io.result.expect((-5).S)
        dut.io.op.poke(ALUOp.Sub)
        dut.clock.step()
        dut.io.result.expect((-15).S)
      }
    }

  }
}
