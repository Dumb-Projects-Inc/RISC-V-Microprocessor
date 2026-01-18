package riscv.branchPred

import chisel3._
import org.scalatest.funspec.AnyFunSpec
import chisel3.simulator.scalatest.ChiselSim

class BHTSpec extends AnyFunSpec with ChiselSim {
  describe("BHT") {
    it("should predict branches correctly") {
      simulate(new BHT(16)) { dut =>
        dut.io.update.poke(false.B)
        dut.io.taken.poke(false.B)
        dut.io.updatePc.poke(0.U)

        for (i <- 0 until 16) {
          dut.io.currentPc.poke((i * 4).U)
          dut.clock.step()
          dut.io.pred.expect(false.B)
        }

        dut.io.update.poke(true.B)
        dut.io.taken.poke(true.B)
        for (i <- 0 until 8) {
          dut.io.updatePc.poke((i * 4).U)
          dut.clock.step()
        }

        // stop updating while checking
        dut.io.update.poke(false.B)

        // first 8 now weak taken (2.U), rest still 1.U
        for (i <- 0 until 16) {
          dut.io.currentPc.poke((i * 4).U)
          dut.clock.step()
          if (i < 8) {
            dut.io.pred.expect(true.B)
          } else {
            dut.io.pred.expect(false.B)
          }
        }

        // mark first 4 branches as not taken going from weak taken (2.U) to weak not taken (1.U)
        dut.io.update.poke(true.B)
        dut.io.taken.poke(false.B)
        for (i <- 0 until 4) {
          dut.io.updatePc.poke((i * 4).U)
          dut.clock.step()
        }

        // stop updating while checking
        dut.io.update.poke(false.B)

        // first 4 now weak not taken (1.U), next 4 weak taken (2.U), rest still 1.U
        for (i <- 0 until 16) {
          dut.io.currentPc.poke((i * 4).U)
          dut.clock.step()
          if (i < 4) {
            dut.io.pred.expect(false.B)
          } else if (i < 8) {
            dut.io.pred.expect(true.B)
          } else {
            dut.io.pred.expect(false.B)
          }
        }

        // mark first 4 branches as not taken going from weak not taken (1.U) to not taken (0.U)
        dut.io.update.poke(true.B)
        dut.io.taken.poke(false.B)
        for (i <- 0 until 4) {
          dut.io.updatePc.poke((i * 4).U)
          dut.clock.step()
        }

        // stop updating while checking
        dut.io.update.poke(false.B)

        // first 4 now not taken (0.U), next 4 weak taken (2.U), rest still 1.U
        for (i <- 0 until 16) {
          dut.io.currentPc.poke((i * 4).U)
          dut.clock.step()
          if (i < 4) {
            dut.io.pred.expect(false.B)
          } else if (i < 8) {
            dut.io.pred.expect(true.B)
          } else {
            dut.io.pred.expect(false.B)
          }
        }

        // mark all branches as taken to test saturating at 3.U
        dut.io.update.poke(true.B)
        dut.io.taken.poke(true.B)
        for (i <- 0 until 16) {
          dut.io.updatePc.poke((i * 4).U)
          dut.clock.step()
        }

        // stop updating while checking
        dut.io.update.poke(false.B)

        // first 4 now weak not taken (1.U), next 4 taken (3.U), rest weak taken (2.U)
        for (i <- 0 until 16) {
          dut.io.currentPc.poke((i * 4).U)
          dut.clock.step()
          if (i < 4) {
            dut.io.pred.expect(false.B)
          } else if (i < 8) {
            dut.io.pred.expect(true.B)
          } else {
            dut.io.pred.expect(true.B)
          }
        }

      }
    }
  }
}
