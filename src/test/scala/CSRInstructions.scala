package riscv

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.Tag

object CSRTag extends Tag("CSR")

// t0=x5
// t1=x6
// t2=x7
// t3=x28
// t4=x29
// t5=x30

// a0=x10
// a1=x11
// a2=x12,
// a3=x13

class CSRInstructionsSpec extends AnyFunSpec with ChiselSim {

  describe("CSR + ECALL/MRET (TestTop style)") {

    it(
      "should set mtvec, trap on ecall, read mcause, advance mepc, and mret",
      CSRTag
    ) {
      val input =
        """
            addi t0, x0, 0x80
            csrw mtvec, t0
            csrr t3, mtvec
            addi a0, x0, 32
            addi a1, x0, 64
            add  a2, a0, a1
          
            ecall
          
            addi a3, x0, 0x123
          
          done:
            jal x0, done
          
            .org 0x80
          trap_handler:
            csrr t2, mcause
            csrr t1, mepc
            addi t1, t1, 4
            csrw mepc, t1
            mret
          """.stripMargin

      simulate(new TestTop(input)) { dut =>
        for (i <- 0 until 25) {
          dut.clock.step(1)
        }

        dut.io.dbg(5).expect("h00000080".U) // t0
        dut.io.dbg(28).expect("h00000080".U) // t3 = csrr t3, mtvec
        dut.io.dbg(7).expect(11.U) // t2 = mcause
        dut.io.dbg(13).expect("h00000123".U) // a3 executed after mret
        dut.io.dbg(6).expect("h0000001c".U) // t1 = mepc + 4
      }
    }
    it(
      "should implement CSRRS/CSRRC with correct old-value writeback",
      CSRTag
    ) {
      val input =
        """
      # Put handler at 0x80 (just to keep the style consistent)
      addi t0, x0, 0x80
      csrw mtvec, t0

      # Start with mstatus = 0
      addi t1, x0, 0
      csrw mstatus, t1

      # t1 = 8 (bit3)
      addi t1, x0, 8

      # CSRRS: rd gets old(0), CSR becomes 0|8 = 8
      csrrs t2, mstatus, t1     # t2 should become 0
      csrr  t3, mstatus         # t3 should become 8

      # CSRRC: rd gets old(8), CSR becomes 8 & ~8 = 0
      csrrc t4, mstatus, t1     # t4 should become 8
      csrr  t5, mstatus         # t5 should become 0

    done:
      jal x0, done

      .org 0x80
    trap_handler:
      mret
    """.stripMargin

      simulate(new TestTop(input)) { dut =>
        dut.clock.step(40)

        // ABI mapping: t0=x5, t1=x6, t2=x7, t3=x28, t4=x29, t5=x30
        dut.io.dbg(7).expect(0.U) // t2 old mstatus from CSRRS
        dut.io.dbg(28).expect(8.U) // t3 readback after CSRRS
        dut.io.dbg(29).expect(8.U) // t4 old mstatus from CSRRC
        dut.io.dbg(30).expect(0.U) // t5 readback after CSRRC
      }
    }

    it(
      "should not write back when rd=x0 (csrrw x0, ...) but CSR should update",
      CSRTag
    ) {
      val input =
        """
      addi t0, x0, 0x80
      csrw mtvec, t0

      # Put a known value in t1, then execute csrrw x0, mscratch, t1
      addi t1, x0, 0x33
      csrrw x0, mscratch, t1      # should write mscratch = 0x33, but NOT modify x0
      csrr t2, mscratch           # should read back 0x33 into t2

      ecall

      addi a3, x0, 0x99
    done:
      jal x0, done

      .org 0x80
    trap_handler:
      csrr a0, mcause
      csrr a1, mepc
      addi a1, a1, 4
      csrw mepc, a1
      mret
    """.stripMargin

      simulate(new TestTop(input)) { dut =>
        dut.clock.step(40)

        dut.io.dbg(6).expect("h00000033".U) // t1
        dut.io.dbg(7).expect("h00000033".U) // t2 reads mscratch
        dut.io.dbg(0).expect(0.U) // x0 stays 0
        dut.io.dbg(13).expect("h00000099".U) // a3 after return
      }
    }

    it(
      "should save correct mepc for ECALL and return to instruction after ECALL",
      CSRTag
    ) {
      val input =
        """
      addi t0, x0, 0x80
      csrw mtvec, t0

      # pad so ecall lands at known pc:
      nop
      nop
      nop
      nop
      nop

      # At this point, ECALL should be at a fixed pc depending on your assembler;
      # We'll verify behavior by returning to the instruction right after it.
      ecall

      # must execute after return
      addi a3, x0, 0x123

    done:
      jal x0, done

      .org 0x80
    trap_handler:
      csrr t1, mepc
      addi t2, t1, 4
      csrw mepc, t2
      mret
    """.stripMargin

      simulate(new TestTop(input)) { dut =>
        dut.clock.step(50)

        dut.io.dbg(13).expect("h00000123".U) // proves returned correctly
        // t2 was computed as mepc+4 inside handler, but note: t2 is x7; t1 is x6
        // If you want: ensure t2 == t1+4 (soft check via reading both)
        val mepc = dut.io.dbg(6).peek().litValue
        val mepcPlus4 = dut.io.dbg(7).peek().litValue
        assert(mepcPlus4 == (mepc + 4))
      }
    }
    it("should handle two ecalls with correct trap/return both times", CSRTag) {
      val input =
        """
      addi t0, x0, 0x80
      csrw mtvec, t0

      # first trap
      ecall
      addi a0, x0, 1

      # second trap
      ecall
      addi a1, x0, 2

    done:
      jal x0, done

      .org 0x80
    trap_handler:
      csrr t2, mcause
      csrr t1, mepc
      addi t1, t1, 4
      csrw mepc, t1
      mret
    """.stripMargin

      simulate(new TestTop(input)) { dut =>
        dut.clock.step(60)

        dut.io.dbg(10).expect(1.U) // a0 set after first return
        dut.io.dbg(11).expect(2.U) // a1 set after second return
        dut.io.dbg(7).expect(11.U) // last mcause read still 11
      }
    }
    it("should use updated mtvec when mtvec is rewritten", CSRTag) {
      val input =
        """
      # Set mtvec=0x80 first
      addi t0, x0, 0x80
      csrw mtvec, t0

      # Now change mtvec to 0x100
      addi t0, x0, 0x100
      csrw mtvec, t0
      csrr t3, mtvec

      ecall
      addi a3, x0, 0x77

    done:
      jal x0, done

      .org 0x100
    trap_handler2:
      csrr t2, mcause
      csrr t1, mepc
      addi t1, t1, 4
      csrw mepc, t1
      mret
    """.stripMargin

      simulate(new TestTop(input)) { dut =>
        dut.clock.step(60)

        dut.io.dbg(28).expect("h00000100".U) // t3 readback mtvec
        dut.io.dbg(13).expect("h00000077".U) // returned
        dut.io.pc.peek() // optional: just to ensure sim runs
      }
    }
    it(
      "optional: mtvec low bits behavior (mask/keep) - choose expected behavior",
      CSRTag
    ) {
      val input =
        """
      # write mtvec = 0x81 (misaligned / mode bit set)
      addi t0, x0, 0x81
      csrw mtvec, t0
      csrr t3, mtvec
      ecall
      addi a3, x0, 0x11

    done:
      jal x0, done

      .org 0x80
    trap_handler:
      csrr t2, mcause
      csrr t1, mepc
      addi t1, t1, 4
      csrw mepc, t1
      mret
    """.stripMargin

      simulate(new TestTop(input)) { dut =>
        dut.clock.step(60)

        // Pick ONE depending on your spec:
        // If you mask low bits: expect 0x80
        // dut.io.dbg(28).expect("h00000080".U)

        // If you store raw: expect 0x81
        // dut.io.dbg(28).expect("h00000081".U)

        dut.io.dbg(13).expect("h00000011".U)
      }
    }

  }
}
