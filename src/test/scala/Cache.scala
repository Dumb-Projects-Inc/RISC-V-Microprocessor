package riscv

import chisel3._
import org.scalatest.funspec.AnyFunSpec
import chisel3.simulator.scalatest.ChiselSim

class CacheSpec extends AnyFunSpec with ChiselSim {
  describe("Cache") {
    it("should correctly handle hits and misses") {
      simulate(new Cache(64, 64)) { dut =>
        dut.io.addr.poke(0.U)
        dut.io.writeData.poke("hDEADBEEFCAFEBABE0123456789ABCDEF".U)
        dut.io.writeEnable.poke(true.B)
        dut.clock.step()
        dut.io.addr.poke(100.U)
        dut.io.writeEnable.poke(false.B)
        dut.clock.step()
        dut.io.addr.poke(0.U)
        dut.io.hit.expect(true.B)
        dut.io.readData.valid
          .expect(false.B) // first cycle after read, not valid yet
        dut.clock.step(2)

        // dut.io.readData.valid.expect(true.B)

        // new tag, should miss in same cycle
        dut.io.addr.poke("h00010000".U)
        dut.io.hit.expect(false.B)
        dut.io.readData.valid.expect(true.B)

      }
    }
    it("should have zero latency within same index") {
      simulate(new Cache(64, 64)) { dut =>
        dut.io.addr.poke(0.U)
        dut.io.writeData.poke(
          "hDEADBEEFCAFEBABE0123456789ABCDEFDEADBEEFCAFEBABE0123456789ABCDEFDEADBEEFCAFEBABE0123456789ABCDEFDEADBEEFCAFEBABE0123456789ABCDEF".U
        )
        dut.io.writeEnable.poke(true.B)
        dut.clock.step()
        dut.io.writeEnable.poke(false.B)
        dut.clock.step()
        dut.io.addr.poke(0.U)
        dut.clock.step()
        dut.io.hit.expect(true.B)
        dut.io.readData.valid.expect(true.B)
        dut.io.readData.bits.expect("hDEADBEEF".U)

        // next offset same line
        dut.io.addr.poke(4.U)
        dut.io.hit.expect(true.B)
        dut.io.readData.valid.expect(true.B)
        dut.io.readData.bits.expect("hCAFEBABE".U)

      }
    }
  }
}
