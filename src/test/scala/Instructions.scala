package riscv

import chisel3._
import os._
import chisel3.simulator.scalatest.ChiselSim
import chisel3.util.experimental.loadMemoryFromFile
import com.carlosedp.riscvassembler.RISCVAssembler

import org.scalatest.funspec.AnyFunSpec

class TestTop(instr: String) extends Module {
  val io = IO(new Bundle {
    val dbg = Output(Vec(32, UInt(32.W)))
    val pc = Output(UInt(32.W))
  })

  val pipeline = Module(new Pipeline(debug = true, debugPrint = true))
  io.dbg := pipeline.dbg.get.regs
  io.pc := pipeline.dbg.get.pc
  val program = SyncReadMem(1024, UInt(32.W))

  val hex = RISCVAssembler.fromString(instr.stripMargin)

  val tempFile = os.temp(suffix = ".hex")
  os.write.over(tempFile, hex)
  loadMemoryFromFile(program, tempFile.toString)
  pipeline.io.instrPort.stall := false.B
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
  pipeline.io.dataPort.stall := false.B
}

class Instructions extends AnyFunSpec with ChiselSim {
  describe("Instructions") {
    it("should correctly load and store words") {
      val input =
        """
       addi x1, x0, 123
       addi x0, x0, 0
       addi x0, x0, 0
       addi x0, x0, 0
       sw x1, 8(x0)
       addi x0, x0, 0
       addi x0, x0, 0
       addi x0, x0, 0
       lw x2, 8(x0)
       addi x0, x0, 0
       addi x0, x0, 0
       """
      simulate(new TestTop(input)) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(1)
        dut.reset.poke(false.B)

        dut.clock.step(15)

        dut.io.dbg(0).expect(0.U)
        dut.io.dbg(1).expect(123)
        dut.io.dbg(2).expect(123)
      }
    }
    it("should add correctly") {
      val input =
        """
       addi x1, x0, 10
       addi x2, x0, 20
       addi x0, x0, 0
       addi x0, x0, 0
       addi x0, x0, 0
       add x3, x1, x2
       addi x0, x0, 0
       addi x0, x0, 0
       addi x0, x0, 0
       """
      simulate(new TestTop(input)) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(1)
        dut.reset.poke(false.B)

        dut.clock.step(15)

        dut.io.dbg(1).expect(10.U)
        dut.io.dbg(2).expect(20.U)
        dut.io.dbg(3).expect(30.U)
      }
    }
    it("should handle conditional branches (BEQ)") {
      val input =
        """
        addi x1, x0, 10 // 0
        addi x2, x0, 10 // 4
        addi x0, x0, 0  // 8
        addi x0, x0, 0  // 12
        addi x0, x0, 0  // 16
        beq x1, x2, taken // 20
        addi x3, x0, 0 // 24
        jal x0, end // 28
        taken:
        addi x3, x0, 1 // 32
        end:
        addi x0, x0, 0 // 36
        """
      simulate(new TestTop(input)) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(1)
        dut.reset.poke(false.B)

        dut.clock.step(20)

        dut.io.dbg(3).expect(1.U)
      }
    }

    it("should handle loops (BNE)") {
      val input =
        """
        addi x1, x0, 0
        addi x2, x0, 5
        loop:
        addi x1, x1, 1
        addi x0, x0, 0  // NOP 1
        addi x0, x0, 0  // NOP 2
        addi x0, x0, 0  // NOP 3
        bne x1, x2, loop
        addi x3, x0, 100
        """
      simulate(new TestTop(input)) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(1)
        dut.reset.poke(false.B)

        dut.clock.step(50)

        dut.io.dbg(1).expect(5.U)
        dut.io.dbg(3).expect(100.U)
        dut.io.dbg(1).expect(1.U)
      }
    }
    it("should jump correctly") {
      val input =
        """    
       addi x1, x0, 1
       jal x0, 32
       addi x0, x0, 0
       addi x0, x0, 0
       addi x0, x0, 0
       addi x0, x0, 0
       addi x1, x1, 1
       addi x0, x0, 0
       addi x0, x0, 0
       addi x0, x0, 0
       addi x0, x0, 0
       addi x0, x0, 0
       """
      simulate(new TestTop(input)) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(1)
        dut.reset.poke(false.B)

        dut.clock.step(15)

        dut.io.dbg(1).expect(1.U)
      }
    }
    it("should work with LUI") {
      val input =
        """    
       lui x1, 0x12345
       addi x0, x0, 0
       addi x0, x0, 0
       addi x0, x0, 0
       """
      simulate(new TestTop(input)) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(1)
        dut.reset.poke(false.B)

        dut.clock.step(10)

        dut.io.dbg(1).expect(0x12345000.U)
      }
    }
  }
}
