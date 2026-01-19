package riscv

import chisel3._
import os._
import chisel3.simulator.scalatest.ChiselSim
import chisel3.util.experimental.loadMemoryFromFile
import com.carlosedp.riscvassembler.RISCVAssembler

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.Tag

object Basic extends Tag("Basic")
object Hazard extends Tag("Hazard")
object Branch extends Tag("Branch")

class TestTop(instr: String) extends Module {
  val io = IO(new Bundle {
    val dbg = Output(Vec(32, UInt(32.W)))
    val pc = Output(UInt(32.W))
    val dataaddr = Output(UInt(32.W))
    val dataEn = Output(Bool())
    val dataOut = Output(UInt(32.W))
  })

  val pipeline = Module(new Pipeline(debug = true, debugPrint = false))
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

  when(
    (pipeline.io.dataPort.memOp === MemOp.Store) && pipeline.io.dataPort.enable
  ) {
    dmem.write(dmemAddr, pipeline.io.dataPort.dataWrite)
  }
  pipeline.io.dataPort.stall := false.B

  io.dataaddr := pipeline.io.dataPort.addr
  io.dataEn := pipeline.io.dataPort.enable
  io.dataOut := pipeline.io.dataPort.dataWrite
}

class Instructions extends AnyFunSpec with ChiselSim {
  describe("Basic instructions") {
    it("should correctly load and store words", Basic) {
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
        addi x0, x0, 0
       """
      simulate(new TestTop(input)) { dut =>
        dut.clock.step(15)

        dut.io.dbg(0).expect(0.U)
        dut.io.dbg(1).expect(123)
        dut.io.dbg(2).expect(123)
      }
    }
    it("should implement addi", Basic) {
      val input =
        """
       addi x1, x0, 10
       addi x0, x0, 0
       addi x0, x0, 0
       addi x0, x0, 0
       addi x0, x0, 0
       """
      simulate(new TestTop(input)) { dut =>
        dut.clock.step(5)
        dut.io.dbg(1).expect(10.U)
      }
    }
    it("should implement add", Basic) {
      val input =
        """
       addi x1, x0, 10
       addi x2, x0, 15
       addi x0, x0, 0
       addi x0, x0, 0
       addi x0, x0, 0
       add  x3, x1, x2
       """
      simulate(new TestTop(input)) { dut =>
        dut.clock.step(10)

        dut.io.dbg(3).expect(25)
      }
    }
    it("should implement jal", Branch) {
      val input =
        """
       addi x1, x0, 1
       jal x2, 16
       addi x1, x1, 1
       addi x1, x1, 1
       addi x1, x1, 1
       addi x1, x1, 1
       addi x0, x0, 0
       addi x0, x0, 0
       addi x0, x0, 0
       addi x0, x0, 0
       addi x0, x0, 0
       """
      simulate(new TestTop(input)) { dut =>
        dut.clock.step(15)

        dut.io.dbg(1).expect(2.U)
        dut.io.dbg(2).expect(8.U)
      }
    }
    it("should handle conditional branches (BEQ)", Branch) {
      val input =
        """
        beq x0, x0, 24
        addi x1, x0, 1
        addi x1, x0, 1
        addi x1, x0, 1
        addi x1, x0, 1
        addi x1, x0, 1
        addi x2, x0, 1
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        """
      simulate(new TestTop(input)) { dut =>
        dut.clock.step(10)

        dut.io.dbg(1).expect(0.U)
        dut.io.dbg(2).expect(1.U)
      }
    }

    it("should handle loops (BNE)", Branch) {
      val input =
        """
        addi x1, x0, 0
        addi x2, x0, 5
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        loop:
        addi x1, x1, 1
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        bne x1, x2, loop
        addi x3, x0, 100
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        """
      simulate(new TestTop(input)) { dut =>
        dut.clock.step(50)
        dut.io.dbg(1).expect(5.U)
        dut.io.dbg(3).expect(100.U)
      }
    }
    it("should handle negative memory offsets (LW/SW)", Basic) {
      val input =
        """
        addi x1, x0, 100
        addi x2, x0, 0xAA
        addi x3, x0, 0xBB

        sw x2, 4(x1)
        sw x3, -4(x1)

        lw x4, 4(x1)
        lw x5, -4(x1)
        """
      simulate(new TestTop(input)) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(1)
        dut.reset.poke(false.B)

        dut.clock.step(20)

        dut.io.dbg(4).expect(0xaa.U)
        dut.io.dbg(5).expect(0xbb.U)
      }
    }
    it("should handle not-taken branches correctly", Branch) {
      val input =
        """
        addi x1, x0, 10
        addi x2, x0, 20
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        beq x1, x2, skip
        addi x3, x0, 5
        jal x0, end
        skip:
        addi x3, x0, 99
        end:
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

        dut.io.dbg(3).expect(5.U)
      }
    }
    it("should execute LUI correctly", Basic) {
      val input =
        """
        lui x1, 0x12345
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        """
      simulate(new TestTop(input)) { dut =>
        dut.clock.step(10)
        dut.io.dbg(1).expect("h12345000".U)
      }
    }

    it("should execute AUIPC correctly", Basic) {
      val input =
        """
        addi x0, x0, 0
        addi x0, x0, 0
        auipc x1, 0x12345
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        """
      simulate(new TestTop(input)) { dut =>
        dut.clock.step(15)
        // addi (0), addi (4), auipc (8)
        // PC for auipc is 8.
        // Result: 8 + 0x12345000 = 0x12345008
        dut.io.dbg(1).expect("h12345008".U)
      }
    }

    it("should combine LUI and ADDI", Basic) {
      val input =
        """
        lui x1, 0x12345
        addi x1, x1, 0x678
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        """
      simulate(new TestTop(input)) { dut =>
        dut.clock.step(10)
        dut.io.dbg(1).expect("h12345678".U)
      }
    }

    it("should execute JALR (Computed Jump)", Branch) {
      val input =
        """
        addi x1, x0, 16
        jalr x0, x1, 0
        addi x2, x0, 0xAA
        addi x2, x0, 0xBB
        addi x2, x0, 0xCC 
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        """
      simulate(new TestTop(input)) { dut =>
        dut.clock.step(20)
        dut.io.dbg(2).expect(0xcc.U)
      }
    }

    it("should execute JALR with Link (Function Call return setup)", Branch) {
      val input =
        """
        addi x1, x0, 16
        jalr x2, x1, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x3, x2, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        """
      simulate(new TestTop(input)) { dut =>
        dut.clock.step(20)
        dut.io.dbg(2).expect(8.U)
        dut.io
          .dbg(3)
          .expect(8.U)
      }
    }
  }

  describe("Pipeline Hazards & Forwarding") {
    it("should forward ALU result to memory store", Hazard) {
      val input =
        """
        addi x1, x0, 4  
        addi x2, x0, 42
        sw   x2, 0(x1)
        lw   x3, 0(x1)
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        """
      simulate(new TestTop(input)) { dut =>
        dut.clock.step(10)
        dut.io.dbg(3).expect(42.U)
      }
    }
    it("should forward ALU result to ALU input", Hazard) {
      val input =
        """
        addi x1, x0, 10
        addi x2, x0, 20
        add  x3, x1, x2
        add  x4, x3, x1
        add  x5, x4, x4
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        """

      simulate(new TestTop(input)) { dut =>
        dut.clock.step(10)

        dut.io.dbg(3).expect(30)
        dut.io.dbg(4).expect(40)
        dut.io.dbg(5).expect(80)
      }
    }
    it("should forward Memory Load to ALU input (Load-Use no stall)", Hazard) {
      val input =
        """
        addi x1, x0, 100
        addi x2, x0, 55
        sw   x2, 0(x1)
        lw   x3, 0(x1)
        addi x4, x3, 5
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        """

      simulate(new TestTop(input)) { dut =>
        dut.clock.step(10)

        dut.io.dbg(3).expect(55)
        dut.io.dbg(4).expect(60)
      }
    }
    it("should forward registers to memory address", Hazard) {
      val input =
        """
        addi x2, x0, 100
        addi x1, x0, 200
        sw   x2, 0(x1)
        lw   x3, 0(x2)
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        """
      simulate(new TestTop(input)) { dut =>
        dut.clock.step(5)
        dut.io.dataaddr.expect(200.U)
        dut.io.dataEn.expect(true.B)
        dut.clock.step()
        dut.io.dataaddr.expect(100.U)
        dut.io.dataEn.expect(true.B)
      }
    }
  }
  it("should flush on taken Branch", Branch) {
    val input =
      """
        jal x0, end
        addi x1, x0, 101
        addi x1, x0, 102
        addi x1, x0, 104
        addi x1, x0, 108
        end:
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        """
    simulate(new TestTop(input)) { dut =>
      dut.clock.step(10)
      dut.io.dbg(1).expect(0.U) // x3 should be 100
    }
  }
  it("should flush memory accesses when branching", Branch) {
    val input =
      """
        addi x1, x0, 100
        sw x0, 0(x1)
        addi x2, x0, 55
        bne x0, x1, end
        sw   x2, 0(x1)
        end:
          lw   x3, 0(x1)
          addi x0, x0, 0
          addi x0, x0, 0
          addi x0, x0, 0
      """
    simulate(new TestTop(input)) { dut =>
      dut.clock.step(30)
      dut.io.dbg(3).expect(0.U) // x3 should be 0 since store was flushed
    }
  }
  it("should NOT forward values written to x0", Hazard) {
    val input =
      """
        addi x1, x0, 10
        addi x0, x1, 20
        add  x2, x0, x1
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        """

    simulate(new TestTop(input)) { dut =>
      dut.clock.step(10)

      // x2 should be 10 (0 + 10), NOT 40 (30 + 10)
      dut.io.dbg(2).expect(10.U)
    }
  }
  it("should handle Branch condition hazard (Forwarding to Branch)", Hazard) {
    val input =
      """
        addi x1, x0, 2
        addi x2, x0, 1
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x1, x0, 10
        addi x2, x0, 10
        beq x1, x2, taken
        addi x3, x0, 1
        jal x0, end
        taken:
        addi x3, x0, 0x2
        end:
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        """
    simulate(new TestTop(input)) { dut =>
      dut.clock.step(17)
      dut.io.dbg(3).expect(0x2.U)
    }
  }
  it("should forward LUI result to ADDI", Hazard) {
    val input =
      """
        lui x1, 0x00001
        addi x2, x1, 4
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        """
    simulate(new TestTop(input)) { dut =>
      dut.clock.step(10)
      dut.io.dbg(2).expect(4100.U)
    }
  }
}
