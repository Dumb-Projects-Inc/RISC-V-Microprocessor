package riscv

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import chisel3._
import chisel3.simulator.scalatest.ChiselSim

class HazardUnitSpec extends AnyFunSpec with ChiselSim with Matchers {
  describe("HazardUnit") {

    it("does nothing when there is no hazard and no redirect") {
      simulate(new HazardUnit()) { dut =>
        dut.io.id.rs1.poke(1.U)
        dut.io.id.rs2.poke(2.U)
        dut.io.id.usesRs1.poke(true.B)
        dut.io.id.usesRs2.poke(true.B)

        dut.io.ex.rd.poke(3.U)
        dut.io.ex.isLoad.poke(false.B)

        dut.io.exRedirect.poke(false.B)

        dut.clock.step()

        dut.io.out.stallPC.expect(false.B)
        dut.io.out.stallIFID.expect(false.B)
        dut.io.out.flushIFID.expect(false.B)
        dut.io.out.flushIDEX.expect(false.B)
      }
    }

    it("stalls on load-use hazard when x5 is used as rs1") {
      simulate(new HazardUnit()) { dut =>
        dut.io.id.rs1.poke(5.U)
        dut.io.id.rs2.poke(0.U)
        dut.io.id.usesRs1.poke(true.B)
        dut.io.id.usesRs2.poke(false.B)

        dut.io.ex.rd.poke(5.U)
        dut.io.ex.isLoad.poke(true.B)

        dut.io.exRedirect.poke(false.B)

        dut.clock.step()

        dut.io.out.stallPC.expect(true.B)
        dut.io.out.stallIFID.expect(true.B)
        dut.io.out.flushIFID.expect(false.B)
        dut.io.out.flushIDEX.expect(true.B)
      }
    }

    it("stalls on load-use hazard when x9 is used as rs2") {
      simulate(new HazardUnit()) { dut =>
        dut.io.id.rs1.poke(0.U)
        dut.io.id.rs2.poke(9.U)
        dut.io.id.usesRs1.poke(false.B)
        dut.io.id.usesRs2.poke(true.B)

        dut.io.ex.rd.poke(9.U)
        dut.io.ex.isLoad.poke(true.B)

        dut.io.exRedirect.poke(false.B)

        dut.clock.step()

        dut.io.out.stallPC.expect(true.B)
        dut.io.out.stallIFID.expect(true.B)
        dut.io.out.flushIFID.expect(false.B)
        dut.io.out.flushIDEX.expect(true.B)
      }
    }

    it("does not stall if EX is not a load, even if rd matches rs1") {
      simulate(new HazardUnit()) { dut =>
        dut.io.id.rs1.poke(7.U)
        dut.io.id.rs2.poke(0.U)
        dut.io.id.usesRs1.poke(true.B)
        dut.io.id.usesRs2.poke(false.B)

        dut.io.ex.rd.poke(7.U)
        dut.io.ex.isLoad.poke(false.B)

        dut.io.exRedirect.poke(false.B)

        dut.clock.step()

        dut.io.out.stallPC.expect(false.B)
        dut.io.out.stallIFID.expect(false.B)
        dut.io.out.flushIFID.expect(false.B)
        dut.io.out.flushIDEX.expect(false.B)
      }
    }

    it("flushes IF/ID and ID/EX on redirect without stalling") {
      simulate(new HazardUnit()) { dut =>
        dut.io.id.rs1.poke(1.U)
        dut.io.id.rs2.poke(2.U)
        dut.io.id.usesRs1.poke(true.B)
        dut.io.id.usesRs2.poke(true.B)

        dut.io.ex.rd.poke(10.U)
        dut.io.ex.isLoad.poke(false.B)

        dut.io.exRedirect.poke(true.B)

        dut.clock.step()

        dut.io.out.stallPC.expect(false.B)
        dut.io.out.stallIFID.expect(false.B)
        dut.io.out.flushIFID.expect(true.B)
        dut.io.out.flushIDEX.expect(true.B)
      }
    }

    it("redirect instead of load-use stall") {
      simulate(new HazardUnit()) { dut =>
        dut.io.id.rs1.poke(6.U)
        dut.io.id.rs2.poke(0.U)
        dut.io.id.usesRs1.poke(true.B)
        dut.io.id.usesRs2.poke(false.B)

        dut.io.ex.rd.poke(6.U)
        dut.io.ex.isLoad.poke(true.B)

        dut.io.exRedirect.poke(true.B)

        dut.clock.step()

        dut.io.out.stallPC.expect(false.B)
        dut.io.out.stallIFID.expect(false.B)
        dut.io.out.flushIFID.expect(true.B)
        dut.io.out.flushIDEX.expect(true.B)
      }
    }
  }
}
