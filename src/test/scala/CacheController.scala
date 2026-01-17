package riscv.memory

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CacheControllerSpec extends AnyFlatSpec with ChiselSim with Matchers {
  behavior of "CacheController"

  it should "deliver the correct instruction when crossing memory region boundaries" in {
    val romSize = 1024
    val magicWord = "deadbeef"
    val romContent =
      Seq.fill(romSize - 1)("00000013") :+ magicWord
    val romString = romContent.mkString("\n")

    class TestWrapper extends Module {
      val io = IO(new Bundle {
        val addr = Input(UInt(32.W))
        val instr = Output(UInt(32.W))
      })

      val dut = Module(new CacheController())
      val rom = Module(new InstructionROM(romString))

      rom.io.addr := io.addr
      dut.io.ROMIn := rom.io.instruction

      dut.io.instrPort.addr := io.addr
      dut.io.instrPort.enable := true.B

      dut.io.dataPort.addr := 0.U
      dut.io.dataPort.enable := false.B
      dut.io.dataPort.writeEn := false.B
      dut.io.dataPort.dataWrite := 0.U

      io.instr := dut.io.instrPort.instr
    }

    simulate(new TestWrapper) { c =>
      c.io.addr.poke("h00000FFC".U)
      c.clock.step()
      c.io.addr.poke("h00010000".U)
      c.io.instr.expect("hdeadbeef".U)
    }
  }
}
