package riscv

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec
import java.nio.file.Files
import java.io.File
import rvsim.VM

class E2ESpec extends AnyFunSpec with ChiselSim {

  // Helper to locate binary files
  def getTestBinaries(folder: String): Array[File] = {
    val dir = new File(folder)
    if (dir.exists && dir.isDirectory)
      dir.listFiles.filter(_.getName.endsWith(".bin"))
    else Array.empty
  }

  describe("RISC-V Microprocessor E2E Tests") {
    val binFolder = "src/test/resources/cae/bin"
    val testFiles = getTestBinaries(binFolder)

    if (testFiles.isEmpty) {
      it("should find test binaries") {
        fail(s"No binary files found in $binFolder")
      }
    }

    testFiles.foreach { file =>
      it(s"should pass ${file.getName}") {
        val sim = new VM()
        val programBytes = Files.readAllBytes(file.toPath)
        sim.loadProgram(programBytes)

        var steps = 0
        val maxSteps = 1000
        while (sim.step() && steps < maxSteps) {
          steps += 1
        }

        if (steps >= maxSteps)
          fail(s"Golden model timed out on ${file.getName}")

        val expectedRegs = sim.getRegistersArray()
        val expectedExitPC = sim.getPc()

        simulate(new RV32ITop(program = file.getAbsolutePath, debug = true)) {
          dut =>
            dut.clock.step()

            var cycles = 0
            var finished = false

            while (!finished && cycles < maxSteps) {
              dut.clock.step()
              cycles += 1

              if (dut.dbg.get.writebackPc.peek().litValue == expectedExitPC) {
                finished = true
                dut.clock.step()
              }
            }

            if (!finished) {
              fail(
                s"Hardware simulation timed out. Did not reach exit PC: 0x${expectedExitPC.toHexString}"
              )
            }

            for (i <- 1 until 32) {
              val hwVal = dut.dbg.get.regs(i).peek().litValue
              val simVal =
                expectedRegs(i)

              assert(
                hwVal == simVal,
                s"Register x$i mismatch on ${file.getName}: HW=0x${hwVal.toString(16)} vs SIM=0x${simVal.toHexString}"
              )
            }
        }
      }
    }
  }
}
