package riscv

import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec
import java.nio.file.{Files, Paths}

import rvsim.VM

/** End-to-End tests for the RISC-V Microprocessor
  * @note
  *   This is a placeholder for future E2E tests.
  */

class E2ESpec extends AnyFunSpec with ChiselSim {

  describe("RISC-V Microprocessor E2E Tests") {
    it("should execute a simple program correctly") {
      // clock step through a simple program
      simulate(new RV32I(true)) { dut =>
        val path = Paths.get("src/test/resources/cae/bin/addneg.bin")
        val program =
          Files.readAllBytes(path)
        val sim = new VM()
        // setup inputs

        sim.loadProgram(program)
        while (sim.step()) {
          val simRegs = sim.getRegisters()
          dut.clock.step()
          print(dut.dbg.get.pc.peek())
          // Compare register file contents

        }

      }
    }
  }

}
