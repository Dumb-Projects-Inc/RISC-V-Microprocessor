package riscv

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CSRSpec extends AnyFunSpec with ChiselSim with Matchers {

  val MSTATUS = 0x300
  val MTVEC = 0x305
  val MSCRATCH = 0x340
  val MEPC = 0x341
  val MCAUSE = 0x342
  val MTVAL = 0x343
  val CYCLE = 0xc00
  val CYCLEH = 0xc80

  def writeCSR(dut: CSR, addr: Int, data: BigInt, cmd: CSRCmd.Type): Unit = {
    dut.io.csr.valid.poke(true.B)
    dut.io.csr.addr.poke(addr.U)
    dut.io.csr.cmd.poke(cmd)
    dut.io.csr.writeData.poke(data.U)
  }

  def readCSR(dut: CSR, addr: Int): Unit = {
    dut.io.csr.valid.poke(true.B)
    dut.io.csr.addr.poke(addr.U)
    dut.io.csr.cmd.poke(CSRCmd.RS)
    dut.io.csr.writeData.poke(0.U)
  }

  def clearCSR(dut: CSR): Unit = {
    dut.io.csr.valid.poke(false.B)
    dut.io.csr.addr.poke(0.U)
    dut.io.csr.cmd.poke(CSRCmd.RW)
    dut.io.csr.writeData.poke(0.U)
  }

  def clearTrap(dut: CSR): Unit = {
    dut.io.trap.valid.poke(false.B)
    dut.io.trap.cause.poke(0.U)
  }

  describe("CSR") {

    it("should write and read mtvec via CSR port (RW)") {
      simulate(new CSR()) { dut =>
        dut.io.currentPc.poke(0.U)
        dut.io.retValid.poke(false.B)
        clearTrap(dut)
        clearCSR(dut)
        dut.clock.step()

        // write mtvec = 0x80
        writeCSR(dut, MTVEC, BigInt("80", 16), CSRCmd.RW)
        dut.clock.step()
        clearCSR(dut)
        dut.clock.step()

        // read mtvec back
        readCSR(dut, MTVEC)
        dut.clock.step()
        dut.io.csr.readData.expect("h00000080".U)

        clearCSR(dut)
        dut.clock.step()
      }
    }

    it("should implement RS (set bits) and RC (clear bits) correctly") {
      simulate(new CSR()) { dut =>
        dut.io.currentPc.poke(0.U)
        dut.io.retValid.poke(false.B)
        clearTrap(dut)
        clearCSR(dut)
        dut.clock.step()

        // mstatus = 0x0F
        writeCSR(dut, MSTATUS, BigInt("0F", 16), CSRCmd.RW)
        dut.clock.step()

        // RS set bit 0x10 => 0x1F
        writeCSR(dut, MSTATUS, BigInt("10", 16), CSRCmd.RS)
        dut.clock.step()
        readCSR(dut, MSTATUS)
        dut.clock.step()
        dut.io.csr.readData.expect("h0000001F".U)

        // RC clear bits 0x03 => 0x1C
        writeCSR(dut, MSTATUS, BigInt("03", 16), CSRCmd.RC)
        dut.clock.step()
        readCSR(dut, MSTATUS)
        dut.clock.step()
        dut.io.csr.readData.expect("h0000001C".U)
      }
    }

    it("should redirect to mtvec on trap, and save mepc/mcause") {
      simulate(new CSR()) { dut =>
        dut.io.currentPc.poke(0.U)
        dut.io.retValid.poke(false.B)
        clearTrap(dut)
        clearCSR(dut)
        dut.clock.step()

        // set mtvec = 0x80
        writeCSR(dut, MTVEC, BigInt("80", 16), CSRCmd.RW)
        dut.clock.step()
        clearCSR(dut)

        // trigger trap at PC=0x100 with cause=11
        dut.io.currentPc.poke("h00000100".U)
        dut.io.trap.valid.poke(true.B)
        dut.io.trap.cause.poke(11.U)
        dut.io.retValid.poke(false.B)
        dut.clock.step()

        // should redirect to mtvec (0x80)
        dut.io.redirect.valid.expect(true.B)
        dut.io.redirect.pc.expect("h00000080".U)

        // mepc should now be 0x100, mcause should be 11
        dut.io.trap.valid.poke(false.B)
        dut.clock.step()

        readCSR(dut, MEPC)
        dut.clock.step()
        dut.io.csr.readData.expect("h00000100".U)

        readCSR(dut, MCAUSE)
        dut.clock.step()
        dut.io.csr.readData.expect(11.U)
      }
    }

    it("should redirect to mepc on retValid (mret)") {
      simulate(new CSR()) { dut =>
        dut.io.currentPc.poke(0.U)
        dut.io.retValid.poke(false.B)
        clearTrap(dut)
        clearCSR(dut)
        dut.clock.step()

        // seed mepc = 0x1234 via CSR write (optional but supported)
        writeCSR(dut, MEPC, BigInt("1234", 16), CSRCmd.RW)
        dut.clock.step()
        clearCSR(dut)

        // mret
        dut.io.retValid.poke(true.B)
        dut.clock.step()

        dut.io.redirect.valid.expect(true.B)
        dut.io.redirect.pc.expect("h00001234".U)

        dut.io.retValid.poke(false.B)
        dut.clock.step()
      }
    }

    it("should prioritize trap over retValid over csr.valid") {
      simulate(new CSR()) { dut =>
        dut.io.currentPc.poke("h00000100".U)
        clearTrap(dut)
        clearCSR(dut)
        dut.io.retValid.poke(false.B)
        dut.clock.step()

        // set mtvec=0x80
        writeCSR(dut, MTVEC, BigInt("80", 16), CSRCmd.RW)
        dut.clock.step()
        clearCSR(dut)
        dut.clock.step()

        // Now assert ALL three BEFORE the same clock edge
        dut.io.trap.valid.poke(true.B)
        dut.io.trap.cause.poke(7.U)
        dut.io.retValid.poke(true.B)

        dut.io.csr.valid.poke(true.B)
        dut.io.csr.addr.poke(MTVEC.U)
        dut.io.csr.cmd.poke(CSRCmd.RW)
        dut.io.csr.writeData.poke("h00000200".U)

        dut.clock.step()

        // Clear them immediately after
        dut.io.trap.valid.poke(false.B)
        dut.io.retValid.poke(false.B)
        clearCSR(dut)
        dut.clock.step()

        // Read mtvec using non-destructive read
        dut.io.csr.valid.poke(true.B)
        dut.io.csr.addr.poke(MTVEC.U)
        dut.io.csr.cmd.poke(CSRCmd.RS)
        dut.io.csr.writeData.poke(0.U)
        dut.clock.step()

        dut.io.csr.readData.expect("h00000080".U)
      }
    }

  }
}
