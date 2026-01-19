package riscv.memory

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import riscv.MemSize
import lib.Bus

class CacheControllerSpec extends AnyFlatSpec with ChiselSim with Matchers {
  behavior of "CacheController"

  it should "deliver the correct instruction when crossing memory region boundaries" in {
    val romSize = 1024
    // 1, 2, 3, ..., romSize in word
    val romContent = (1 until romSize + 1).map(i => i.U(32.W)).toSeq

    class TestWrapper extends Module {
      val io = IO(new Bundle {
        val addr = Input(UInt(32.W))
        val instr = Output(UInt(32.W))
        val instrStall = Output(Bool())
      })

      val dut = Module(new CacheController())
      val rom = Module(new InstructionROM(romContent))

      rom.io.addr := io.addr
      dut.io.ROMIn := rom.io.instruction
      dut.io.bus.rdData := "hDEADBEEF".U
      dut.io.bus.rdValid := false.B
      dut.io.bus.stall := false.B

      dut.io.instrPort.addr := io.addr
      dut.io.instrPort.enable := true.B

      dut.io.dataPort.addr := 0.U
      dut.io.dataPort.enable := false.B
      dut.io.dataPort.writeEn := false.B
      dut.io.dataPort.dataWrite := 0.U
      dut.io.dataPort.memSize := MemSize.Word

      io.instr := dut.io.instrPort.instr
      io.instrStall := dut.io.instrPort.stall
    }

    simulate(new TestWrapper) { c =>
      c.io.addr.poke("hfff10000".U)
      c.clock.step()
      c.io.addr.poke("hfff10004".U)
      c.io.instr.expect("h00000001".U)
      c.clock.step()
      c.io.instr.expect("h00000002".U)
      c.io.addr.poke("h00001000".U) // Peripheral region
      c.io.instrStall.expect(false.B)
      c.clock.step()
      // c.io.instrStall.expect(true.B) // we don't stall as no bus for that exists to handle it

    }
  }
}
