package riscv

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.Tag

object CSRTag extends Tag("CSR")

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
          
            # Do some math: a2 = -32 + -64 = -96
            addi a0, x0, -32
            addi a1, x0, -64
            add  a2, a0, a1
          
            # Trap
            ecall
          
            # Must execute after return
            addi a3, x0, 0x123
          
            # park here (avoid depending on “exit” semantics)
          done:
            jal x0, done
          
            # Trap handler at 0x80
            .org 0x80
          trap_handler:
            csrr t2, mcause        # should be 11 for M-mode ECALL in your decode
            csrr t1, mepc
            addi t1, t1, 4         # skip the ECALL instruction
            csrw mepc, t1
            mret
          """.stripMargin

      simulate(new TestTop(input)) { dut =>
        for (i <- 0 until 20) {
          val pc = dut.io.pc.peek().litValue
          val a2 = dut.io.dbg(12).peek().litValue
          val mcause = dut.io.dbg(7).peek().litValue
          val a3 = dut.io.dbg(13).peek().litValue
          val t0 = dut.io.dbg(5).peek().litValue
          println(
            f"cycle=$i pc=0x$pc%08x a2=0x$a2%08x mcause=$mcause a3=0x$a3%08x mtvec=0x$t0%08x"
          )
          dut.clock.step(1)
        }

        for (_ <- 0 until 300) {
          val pc = dut.io.pc.peek().litValue
          if (pc == 0x80L) hitHandler80 = true
          if (pc == 0x84L) hitHandler84 = true
          dut.clock.step(1)
        }

        dut.io.dbg(28).expect("h00000080".U)
        dut.io.dbg(12).expect("hFFFFFFA0".U) // a2
        dut.io.dbg(7).expect(11.U) // t2 = mcause
        dut.io.dbg(13).expect("h00000123".U)
      }
    }
  }
}
