package riscv

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import chisel3._
import chisel3.simulator.scalatest.ChiselSim

class RegisterFileSpec extends AnyFunSpec with ChiselSim with Matchers {
  describe("RegisterFile for RV32I") {

    it("write to a register and read it back") {
      simulate(new RegisterFile()) { dut =>
        // Initialize inputs
        dut.io.writeData.poke(0.U)
        dut.clock.step()

        // Write 0xDEADBEEF to register 1
        dut.io.writeReg.valid.poke(true.B)
        dut.io.writeReg.bits.poke(1.U)
        dut.io.writeData.poke(0xdeadbeefL.U)
        dut.clock.step()
        dut.io.writeReg.valid.poke(false.B)
        // Read from register 1
        dut.io.readReg1.poke(1.U)

        // Step to allow SRAM read (synchronous read)
        dut.clock.step()

        // Check data
        dut.io.readData1.expect(0xdeadbeefL.U)
      }
    }

    it("support dual read ports") {
      simulate(new RegisterFile()) { dut =>
        // Write 10 to x1 and 20 to x2
        dut.io.writeReg.valid.poke(true.B)
        dut.io.writeReg.bits.poke(1.U)
        dut.io.writeData.poke(10.U)
        dut.clock.step()

        dut.io.writeReg.valid.poke(true.B)
        dut.io.writeReg.bits.poke(2.U)
        dut.io.writeData.poke(20.U)
        dut.clock.step()

        dut.io.writeReg.valid.poke(false.B)
        // Read x1 and x2 simultaneously
        dut.io.readReg1.poke(1.U)
        dut.io.readReg2.poke(2.U)
        dut.clock.step()

        dut.io.readData1.expect(10.U)
        dut.io.readData2.expect(20.U)
      }
    }

    it("not write to register 0") {
      simulate(new RegisterFile()) { dut =>
        // Try to write to x0
        dut.io.writeReg.bits.poke(0.U)
        dut.io.writeReg.valid.poke(true.B)
        dut.io.writeData.poke(12345.U)
        dut.clock.step()
        dut.io.writeReg.valid.poke(false.B)

        // Read x0
        dut.io.readReg1.poke(0.U)
        dut.clock.step()

        dut.io.readData1.expect(0.U)
      }
    }
  }
}
