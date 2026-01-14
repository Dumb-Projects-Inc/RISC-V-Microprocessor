package riscv

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import lib.Bus

class TestSlave(val addrBase: Long, val latency: Int = 0) extends Module {
  val io = IO(new Bundle { val bus = Bus.RespondPort.apply() })

  io.bus.init()

  // 1KB memory (256 x 32-bit words)
  val mem = RegInit(0.U(32.W))

  val isSelected =
    (io.bus.addr >= addrBase.U) && (io.bus.addr < (addrBase + 0x400).U)

  val rdValid = RegInit(false.B)
  when(isSelected) {
    when(io.bus.write) {
      mem := io.bus.wrData
    }

    when(io.bus.read) {
      io.bus.rdData := mem
      rdValid := true.B
    }
  }
  io.bus.rdValid := rdValid

}

class master extends Module {
  val io = IO(new Bundle {
    // Translate requestport to respondport
    val masterBus = Bus.RespondPort.apply()
    val bus = Bus.RequestPort.apply()
  })
  // io.bus.init()

  io.bus.addr := io.masterBus.addr
  io.bus.read := io.masterBus.read
  io.bus.write := io.masterBus.write
  io.bus.wrData := io.masterBus.wrData
  io.masterBus.rdData := io.bus.rdData
  io.masterBus.stall := io.bus.stall
  io.masterBus.rdValid := io.bus.rdValid

}

class TestBusTop extends Module {
  val io = IO(new Bundle {
    val masterBus = Flipped(Bus.RequestPort.apply())
  })

  val translator = Module(new master())
  io.masterBus.rdData := translator.io.masterBus.rdData
  io.masterBus.stall := translator.io.masterBus.stall
  io.masterBus.rdValid := translator.io.masterBus.rdValid
  translator.io.masterBus.addr := io.masterBus.addr
  translator.io.masterBus.read := io.masterBus.read
  translator.io.masterBus.write := io.masterBus.write
  translator.io.masterBus.wrData := io.masterBus.wrData

  // Two slaves
  val slave1 = Module(new TestSlave(0x10000000L, latency = 0))
  val slave2 = Module(new TestSlave(0x20000000L, latency = 0))

  translator.io.bus <> slave1.io.bus
  translator.io.bus <> slave2.io.bus

}

class BusSpec extends AnyFunSpec with ChiselSim with Matchers {
  describe("Bus Interconnect") {
    it("should write and read from Slave 1") {
      simulate(new TestBusTop()) { dut =>
        dut.io.masterBus.read.poke(false.B)
        dut.io.masterBus.write.poke(false.B)
        dut.clock.step()

        // Write 42 to Slave 1 (0x10000000)
        dut.io.masterBus.addr.poke(0x10000000.U)
        dut.io.masterBus.wrData.poke(42.U)
        dut.io.masterBus.write.poke(true.B)
        dut.io.masterBus.read.poke(false.B)
        dut.clock.step()

        // Read from Slave 1
        dut.io.masterBus.addr.poke(0x10000000.U)
        dut.io.masterBus.read.poke(true.B)
        dut.io.masterBus.write.poke(false.B)
        dut.clock.step()

        // dut.io.masterBus.rdValid.expect(true.B)
        dut.io.masterBus.rdData.expect(42.U)
      }
    }

    it("should write and read from Slave 2 without affecting Slave 1") {
      simulate(new TestBusTop) { dut =>
        dut.io.masterBus.read.poke(false.B)
        dut.io.masterBus.write.poke(false.B)
        dut.clock.step()

        // Write 100 to Slave 1
        dut.io.masterBus.addr.poke(0x10000000.U)
        dut.io.masterBus.wrData.poke(100.U)
        dut.io.masterBus.write.poke(true.B)
        dut.clock.step()

        // Write 200 to Slave 2
        dut.io.masterBus.addr.poke(0x20000000.U)
        dut.io.masterBus.wrData.poke(200.U)
        dut.io.masterBus.write.poke(true.B)
        dut.clock.step()

        // Clear
        dut.io.masterBus.read.poke(false.B)
        dut.io.masterBus.write.poke(false.B)
        dut.clock.step()

        // Read Slave 1
        dut.io.masterBus.addr.poke(0x10000000.U)
        dut.io.masterBus.read.poke(true.B)
        dut.clock.step()
        dut.io.masterBus.rdData.expect(100.U)
        dut.io.masterBus.rdValid.expect(true.B)

        // Read Slave 2
        dut.io.masterBus.addr.poke(0x20000000.U)
        dut.io.masterBus.read.poke(true.B)
        dut.clock.step()
        dut.io.masterBus.rdData.expect(200.U)
        dut.io.masterBus.rdValid.expect(true.B)
      }
    }
  }
}
