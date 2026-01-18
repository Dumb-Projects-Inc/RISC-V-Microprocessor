package riscv

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CSRSpec extends AnyFunSpec with ChiselSim with Matchers {
  describe("CSR trap/return redirect behavior") {

    it("should not redirect") {
      simulate(new CSR()) { dut =>
        dut.io.currentPc.poke(0.U)
        dut.io.trapValid.poke(false.B)
        dut.io.trapCause.poke(0.U)
        dut.io.retValid.poke(false.B)

        dut.clock.step()

        dut.io.redirectValid.expect(false.B)
        dut.io.redirectPc.expect(0.U)
      }
    }

    it("should redirect to tvec on trap and return to saved epc on ret") {
      simulate(new CSR()) { dut =>
        dut.io.trapValid.poke(false.B)
        dut.io.retValid.poke(false.B)
        dut.io.trapCause.poke(0.U)
        dut.io.currentPc.poke(0.U)
        dut.clock.step()

        dut.io.currentPc.poke("h00000100".U)
        dut.io.trapCause.poke("h0000000B".U) // arbitrary value for testing
        dut.io.trapValid.poke(true.B)
        dut.io.retValid.poke(false.B)
        dut.clock.step()

        // on trap redirecting to tvex
        dut.io.redirectValid.expect(true.B)
        dut.io.redirectPc.expect("h00000000".U)
        dut.io.trapValid.poke(false.B)
        dut.io.currentPc.poke("h00000300".U)
        dut.clock.step()

        // should go back to saved epc = 0x100
        dut.io.retValid.poke(true.B)
        dut.clock.step()

        dut.io.redirectValid.expect(true.B)
        dut.io.redirectPc.expect("h00000100".U)

        // clear
        dut.io.retValid.poke(false.B)
        dut.clock.step()

        dut.io.redirectValid.expect(false.B)
        dut.io.redirectPc.expect(0.U)
      }
    }

    // TODO:
    // it("Should handle multiple traps and returns") {
    //  simulate(new CSR()) { dut =>
    //  }
    // }
  }
}
