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

  when(pipeline.io.dataPort.writeEn) {
    dmem.write(dmemAddr, pipeline.io.dataPort.dataWrite)
  }
  pipeline.io.dataPort.stall := false.B
}

class Instructions extends AnyFunSpec with ChiselSim {
  describe("Basic instructions") {
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
        addi x0, x0, 0
       """
      simulate(new TestTop(input)) { dut =>
        dut.clock.step(15)

        dut.io.dbg(0).expect(0.U)
        dut.io.dbg(1).expect(123)
        dut.io.dbg(2).expect(123)
      }
    }
    it("should implement addi") {
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
    it("should implement add") {
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
    it("should implement jal") {
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
    it("should handle conditional branches (BEQ)") {
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

    it("should handle loops (BNE)") {
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

    // it("should work with LUI") {
    //   val input =
    //     """
    //    lui x1, 0x12345
    //    addi x0, x0, 0
    //    addi x0, x0, 0
    //    addi x0, x0, 0
    //    """
    //   simulate(new TestTop(input)) { dut =>
    //     dut.reset.poke(true.B)
    //     dut.clock.step(1)
    //     dut.reset.poke(false.B)
    //
    //     dut.clock.step(10)
    //
    //     dut.io.dbg(1).expect(0x12345000.U)
    //   }
    // }
    // it("should handle function calls (JAL + JALR)") {
    //   val input =
    //     """
    //     addi x5, x0, 10    // Init x5 = 10
    //     jal x1, func       // Jump to func, x1 = PC+4
    //     addi x5, x5, 1     // This should run AFTER return (x5 = 20 + 1 = 21)
    //     jal x0, end        // Jump to end
    //     func:
    //     addi x5, x5, 10    // x5 = 20
    //     jalr x0, x1, 0     // Return to address in x1
    //     end:
    //     addi x0, x0, 0
    //     """
    //   simulate(new TestTop(input)) { dut =>
    //     dut.reset.poke(true.B)
    //     dut.clock.step(1)
    //     dut.reset.poke(false.B)
    //
    //     dut.clock.step(20)
    //
    //     dut.io.dbg(5).expect(21.U)
    //   }
    // }
    //
    // it("should flush pipeline on branch") {
    //   val input =
    //     """
    //       addi x1, x0, 1
    //       beq x0,x0, skip
    //       addi x1, x1, 100
    //       addi x1, x1, 100
    //       addi x1, x1, 100
    //       skip:
    //       addi x1, x1, 1
    //     """
    //   simulate(new TestTop(input)) { dut =>
    //     dut.reset.poke(true.B)
    //     dut.clock.step(1)
    //     dut.reset.poke(false.B)
    //     dut.clock.step(20)
    //
    //     dut.io.dbg(1).expect(2.U)
    //   }
    // }
    //
    it("should handle negative memory offsets (LW/SW)") {
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

    // it("should construct large values using LUI and ADDI") {
    //   val input =
    //     """
    //     lui x1, 0x12345      // x1 = 0x12345000
    //     addi x1, x1, 0x678   // x1 = 0x12345678
    //     """
    //   simulate(new TestTop(input)) { dut =>
    //     dut.reset.poke(true.B)
    //     dut.clock.step(1)
    //     dut.reset.poke(false.B)
    //
    //     dut.clock.step(10)
    //
    //     dut.io.dbg(1).expect(0x12345678.U)
    //   }
    // }
    //
    it("should handle not-taken branches correctly") {
      val input =
        """
        addi x1, x0, 10
        addi x2, x0, 20
        addi x0, x0, 0
        addi x0, x0, 0
        addi x0, x0, 0
        beq x1, x2, skip    // Should NOT take branch (10 != 20)
        addi x3, x0, 5      // Should execute
        jal x0, end
        skip:
        addi x3, x0, 99     // Should NOT execute
        end:
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

  }
  describe("Pipeline Hazards & Forwarding") {
    //   it("should handle EX->EX forwarding (Back-to-back dependency)") {
    //     val input =
    //       """
    //       addi x1, x0, 10
    //       addi x2, x1, 5   // Depends on x1 immediately
    //       addi x3, x0, 0   // Buffer
    //       """
    //     simulate(new TestTop(input)) { dut =>
    //       dut.reset.poke(true.B)
    //       dut.clock.step(1)
    //       dut.reset.poke(false.B)
    //       dut.clock.step(10)
    //       dut.io.dbg(1).expect(10.U)
    //       dut.io.dbg(2).expect(15.U) // 10 + 5
    //     }
    //   }
    //
    //   it("should handle MEM->EX forwarding (1-cycle gap)") {
    //     val input =
    //       """
    //       addi x1, x0, 10
    //       addi x3, x0, 0   // NOP
    //       addi x2, x1, 5   // Depends on x1 (now in WB)
    //       """
    //     simulate(new TestTop(input)) { dut =>
    //       dut.reset.poke(true.B)
    //       dut.clock.step(1)
    //       dut.reset.poke(false.B)
    //       dut.clock.step(10)
    //       dut.io.dbg(2).expect(15.U)
    //     }
    //   }
    //
    //   it("should stall on Load-Use Hazard") {
    //     val input =
    //       """
    //       addi x5, x0, 20
    //       sw   x5, 4(x0)   // Store 20 at address 4
    //       lw   x1, 4(x0)   // Load 20 into x1
    //       addi x2, x1, 10  // Use x1 immediately (Should be 30)
    //       """
    //     simulate(new TestTop(input)) { dut =>
    //       dut.reset.poke(true.B)
    //       dut.clock.step(1)
    //       dut.reset.poke(false.B)
    //       dut.clock.step(20) // Give extra time for the stall
    //       dut.io.dbg(1).expect(20.U)
    //       dut.io.dbg(2).expect(30.U)
    //     }
    //   }
    //
    // it("should correctly handle Store-to-Load forwarding via memory") {
    //   val input =
    //     """
    //        addi x1, x0, 42
    //        sw   x1, 0(x0)
    //        lw   x2, 0(x0)
    //        """
    //   simulate(new TestTop(input)) { dut =>
    //     dut.reset.poke(true.B)
    //     dut.clock.step(1)
    //     dut.reset.poke(false.B)
    //     dut.clock.step(15)
    //     dut.io.dbg(2).expect(42.U)
    //   }
    // }
    it("should forward ALU result to memory store") {
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
        """
      simulate(new TestTop(input)) { dut =>
        dut.clock.step(10)
        dut.io.dbg(3).expect(42.U)
      }
    }
    it("should flush on taken Branch") {
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
        """
      simulate(new TestTop(input)) { dut =>
        dut.clock.step(10)
        dut.io.dbg(1).expect(0.U) // x3 should be 100
      }
    }

  }
}
