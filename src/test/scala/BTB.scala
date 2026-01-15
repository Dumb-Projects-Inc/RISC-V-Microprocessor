package riscv

import chisel3._
import org.scalatest.funspec.AnyFunSpec
import chisel3.simulator.scalatest.ChiselSim

class BTBSpec extends AnyFunSpec with ChiselSim {
  describe("BTB") {
    it("should hit/miss and handle 2-way replacement in a readable way") {
      simulate(new riscv.branchPred.BTB(16)) { dut =>
        // made some helper functions for more easy use
        def pokeLookup(pc: BigInt): Unit = {
          dut.io.currentPc.poke(pc.U)
          dut.clock.step()
        }

        def expectMiss(pc: BigInt): Unit = {
          dut.io.currentPc.poke(pc.U)
          dut.clock.step()
          dut.io.hit.expect(false.B)
        }

        def expectHit(pc: BigInt, expectedTarget: BigInt): Unit = {
          dut.io.currentPc.poke(pc.U)
          dut.clock.step()
          dut.io.hit.expect(true.B)
          dut.io.targetPc.expect(expectedTarget.U)
        }

        def doUpdate(pc: BigInt, target: BigInt): Unit = {
          dut.io.currentPc.poke(pc.U)
          dut.io.update.pc.poke(pc.U)
          dut.io.update.valid.poke(true.B)
          dut.io.update.targetPc.poke(target.U)
          dut.clock.step()
          dut.io.update.valid.poke(false.B)
        }

        // init
        dut.io.update.valid.poke(false.B)
        dut.io.update.pc.poke(0.U)
        dut.io.update.targetPc.poke(0.U)
        dut.io.currentPc.poke(0.U)
        dut.clock.step()

        val pcA = BigInt("00000000", 16) // index 0
        val pcB = BigInt("00000020", 16) // also index 0, different tag
        val pcC = BigInt(
          "00000040",
          16
        ) // also index 0, different tag will replace

        val tA = BigInt("10000100", 16)
        val tB = BigInt("20000200", 16)
        val tC = BigInt("30000300", 16)

        // all miss
        expectMiss(pcA)
        expectMiss(pcB)
        expectMiss(pcC)

        // insert A then hit A
        doUpdate(pcA, tA)
        expectHit(pcA, tA)
        expectMiss(pcB)

        // insert B in the same set so A and B hits
        doUpdate(pcB, tB)
        expectHit(pcA, tA)
        expectHit(pcB, tB)

        // insert C which should evict A since B was more recently used
        doUpdate(pcC, tC)

        // B and C must hit, miss on A
        expectHit(pcC, tC)
        expectHit(pcB, tB)
        expectMiss(pcA)

      }
    }
  }
}
