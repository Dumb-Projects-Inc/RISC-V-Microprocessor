package riscv

import chisel3._
import os._
import chisel3.simulator.scalatest.ChiselSim
import chisel3.util.experimental.loadMemoryFromFile
import com.carlosedp.riscvassembler.RISCVAssembler

import org.scalatest.funspec.AnyFunSpec

class Top extends Module {
  val pipeline = Module(new Pipeline(debug = true))
  val program = SyncReadMem(1024, UInt(32.W))

  val input =
    """addi x1, x0, 123
       addi x2, x0, 456""".stripMargin
  val hex = RISCVAssembler.fromString(input)
  println(hex)

  val tempFile = os.temp(suffix = ".hex")
  os.write.over(tempFile, hex)
  loadMemoryFromFile(program, tempFile.toString)

  pipeline.io.instrPort.instr := program.read(
    pipeline.io.instrPort.addr(31, 2),
    pipeline.io.instrPort.enable
  )

  val dmem = SyncReadMem(1024, UInt(32.W))
  val dmemAddr = pipeline.io.dataPort.addr(31, 2)
  pipeline.io.dataPort.dataRead := dmem.read(
    dmemAddr,
    pipeline.io.dataPort.enable
  )

  when(pipeline.io.dataPort.writeEn) {
    dmem.write(dmemAddr, pipeline.io.dataPort.dataWrite)
  }
}

class Test extends AnyFunSpec with ChiselSim {
  describe("Test") {
    it("should correctly reconstruct immediates") {
      simulate(new Top) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(1)

        dut.reset.poke(false.B)

        dut.clock.step(50)
      }
    }
  }
}
