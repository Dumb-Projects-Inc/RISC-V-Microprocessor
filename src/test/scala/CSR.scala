package riscv

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CSRSpec extends AnyFunSpec with ChiselSim with Matchers {

  private val CSR_MTVEC = "h305".U

  private def idle(dut: CSR): Unit = {
    dut.io.csr.valid.poke(false.B)
    dut.io.csr.cmd.poke(CSRCmd.RW)
    dut.io.csr.addr.poke(0.U)
    dut.io.csr.writeData.poke(0.U)

    dut.io.currentPc.poke(0.U)
    dut.io.trap.valid.poke(false.B)
    dut.io.trap.cause.poke(0.U)
    dut.io.retValid.poke(false.B)
  }

  private def writeCsr(dut: CSR, addr: UInt, data: UInt): Unit = {
    dut.io.csr.valid.poke(true.B)
    dut.io.csr.cmd.poke(CSRCmd.RW)
    dut.io.csr.addr.poke(addr)
    dut.io.csr.writeData.poke(data)

    dut.clock.step(1)

    // IMPORTANT: stop writing on following cycles
    dut.io.csr.valid.poke(false.B)
    dut.io.csr.addr.poke(0.U)
    dut.io.csr.writeData.poke(0.U)
  }

  describe("CSR trap/return redirect behavior") {

    it("should not redirect when no trap/ret") {
      simulate(new CSR()) { dut =>
        idle(dut)
        dut.clock.step(1)

        dut.io.redirect.valid.expect(false.B)
        dut.io.redirect.pc.expect(0.U)
      }
    }

    it("should redirect to mtvec on trap and to saved mepc on ret") {
      simulate(new CSR()) { dut =>
        // ===== reset / idle =====
        idle(dut)
        dut.clock.step(1)

        // ===== program mtvec = 0x80 =====
        writeCsr(dut, CSR_MTVEC, "h00000080".U)

        // ===== trap at PC=0x100 =====
        dut.io.currentPc.poke("h00000100".U)
        dut.io.trap.cause.poke("h0000000B".U)
        dut.io.trap.valid.poke(true.B)
        dut.io.retValid.poke(false.B)

        dut.clock.step(1)

        // should redirect to mtvec
        dut.io.redirect.valid.expect(true.B)
        dut.io.redirect.pc.expect("h00000080".U)

        // deassert trap
        dut.io.trap.valid.poke(false.B)
        dut.clock.step(1)

        // should go back to no redirect
        dut.io.redirect.valid.expect(false.B)
        dut.io.redirect.pc.expect(0.U)

        // ===== return (mret) =====
        dut.io.retValid.poke(true.B)
        dut.clock.step(1)

        // should redirect to saved mepc (the trapped PC)
        dut.io.redirect.valid.expect(true.B)
        dut.io.redirect.pc.expect("h00000100".U)

        // clear retValid
        dut.io.retValid.poke(false.B)
        dut.clock.step(1)

        dut.io.redirect.valid.expect(false.B)
        dut.io.redirect.pc.expect(0.U)
      }
    }
  }
}
