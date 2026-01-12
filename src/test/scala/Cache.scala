package riscv

import chisel3._
import org.scalatest.funspec.AnyFunSpec
import chisel3.simulator.scalatest.ChiselSim

class CacheSpec extends AnyFunSpec with ChiselSim {
  describe("Cache") {
    it("should correctly handle hits and misses") {
      simulate(new Cache(64, 4)) { dut =>
        dut.io.addr.poke(0.U)
        dut.io.writeData.poke("hDEADBEEFCAFEBABE0123456789ABCDEF".U)
        dut.io.writeEnable.poke(true.B)
        dut.clock.step()
        dut.io.addr.poke(100.U)
        dut.io.writeEnable.poke(false.B)
        dut.clock.step()
        dut.io.addr.poke(0.U)
        dut.io.hit.expect(true.B)
        dut.clock.step(2)

        // dut.io.readData.valid.expect(true.B)

        // new tag, should miss
        dut.io.addr.poke("h10000000".U)
        dut.clock.step()
        dut.io.hit.expect(false.B)

      }
    }
    it("should have zero latency within same index") {
      simulate(new Cache(64, 16)) { dut =>
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
        dut.io.readData.expect("h89ABCDEF".U)

        // next offset same line
        dut.io.addr.poke(4.U)
        dut.clock.step()
        dut.io.hit.expect(true.B)
        dut.io.readData.expect("h01234567".U)

      }
    }
  }
}
