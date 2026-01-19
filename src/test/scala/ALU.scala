package riscv

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import chisel3._
import chisel3.simulator.scalatest.ChiselSim

class ALUSpec extends AnyFunSpec with ChiselSim with Matchers {
  describe("ALU Basic Operations") {

    it("should perform addition correctly") {
      simulate(new ALU) { dut =>
        dut.io.op.poke(ALUOp.Add)

        dut.io.a.poke(10.S)
        dut.io.b.poke(20.S)
        dut.clock.step()
        dut.io.result.expect(30.S)

        dut.io.a.poke(-10.S)
        dut.io.b.poke(20.S)
        dut.clock.step()
        dut.io.result.expect(10.S)

        dut.io.a.poke(-10.S)
        dut.io.b.poke(-20.S)
        dut.clock.step()
        dut.io.result.expect(-30.S)
      }
    }

    it("should perform subtraction correctly") {
      simulate(new ALU) { dut =>
        dut.io.op.poke(ALUOp.Sub)

        dut.io.a.poke(30.S)
        dut.io.b.poke(20.S)
        dut.clock.step()
        dut.io.result.expect(10.S)

        dut.io.a.poke(10.S)
        dut.io.b.poke(20.S)
        dut.clock.step()
        dut.io.result.expect(-10.S)

        dut.io.a.poke(-10.S)
        dut.io.b.poke(-20.S)
        dut.clock.step()
        dut.io.result.expect(10.S)
      }
    }

    it("should perform bitwise AND correctly") {
      simulate(new ALU) { dut =>
        dut.io.op.poke(ALUOp.And)

        dut.io.a.poke(0xf0.S)
        dut.io.b.poke(0x0f.S)
        dut.clock.step()
        dut.io.result.expect(0.S)

        dut.io.a.poke(0xff.S)
        dut.io.b.poke(0x0f.S)
        dut.clock.step()
        dut.io.result.expect(0x0f.S)
      }
    }

    it("should perform bitwise OR correctly") {
      simulate(new ALU) { dut =>
        dut.io.op.poke(ALUOp.Or)

        dut.io.a.poke(0xf0.S)
        dut.io.b.poke(0x0f.S)
        dut.clock.step()
        dut.io.result.expect(0xff.S)
      }
    }

    it("should perform bitwise XOR correctly") {
      simulate(new ALU) { dut =>
        dut.io.op.poke(ALUOp.Xor)

        dut.io.a.poke(0xff.S)
        dut.io.b.poke(0x0f.S)
        dut.clock.step()
        dut.io.result.expect(0xf0.S)
      }
    }
  }

  describe("ALU Shift Operations") {
    it("should perform SLL (Shift Left Logical) correctly") {
      simulate(new ALU) { dut =>
        dut.io.op.poke(ALUOp.Sll)

        dut.io.a.poke(1.S)
        dut.io.b.poke(4.S)
        dut.clock.step()
        dut.io.result.expect(16.S)

        dut.io.a.poke(-1.S)
        dut.io.b.poke(1.S)
        dut.clock.step()
        dut.io.result.expect(-2.S)
      }
    }

    it("should perform SRL (Shift Right Logical) correctly") {
      simulate(new ALU) { dut =>
        dut.io.op.poke(ALUOp.Srl)

        dut.io.a.poke(-1.S)
        dut.io.b.poke(1.S)
        dut.clock.step()
        dut.io.result.expect(0x7fffffff.S)

        dut.io.a.poke(4.S)
        dut.io.b.poke(1.S)
        dut.clock.step()
        dut.io.result.expect(2.S)
      }
    }

    it("should perform SRA (Shift Right Arithmetic) correctly") {
      simulate(new ALU) { dut =>
        dut.io.op.poke(ALUOp.Sra)

        dut.io.a.poke(-4.S)
        dut.io.b.poke(1.S)
        dut.clock.step()
        dut.io.result.expect(-2.S)

        dut.io.a.poke(-2.S)
        dut.io.b.poke(1.S)
        dut.clock.step()
        dut.io.result.expect(-1.S)
      }
    }
  }

  describe("ALU Comparison Operations") {
    it("should perform SLT (Set Less Than Signed) correctly") {
      simulate(new ALU) { dut =>
        dut.io.op.poke(ALUOp.Slt)

        dut.io.a.poke(10.S)
        dut.io.b.poke(20.S)
        dut.clock.step()
        dut.io.result.expect(1.S)

        dut.io.a.poke(20.S)
        dut.io.b.poke(10.S)
        dut.clock.step()
        dut.io.result.expect(0.S)

        dut.io.a.poke(-10.S)
        dut.io.b.poke(10.S)
        dut.clock.step()
        dut.io.result.expect(1.S)
      }
    }

    it("should perform SLTU (Set Less Than Unsigned) correctly") {
      simulate(new ALU) { dut =>
        dut.io.op.poke(ALUOp.Sltu)

        dut.io.a.poke(10.S)
        dut.io.b.poke(20.S)
        dut.clock.step()
        dut.io.result.expect(1.S)

        dut.io.a.poke(-1.S) // 0xFFFFFFFF
        dut.io.b.poke(1.S)
        dut.clock.step()
        dut.io.result.expect(0.S)

        dut.io.a.poke(1.S)
        dut.io.b.poke(-1.S)
        dut.clock.step()
        dut.io.result.expect(1.S)
      }
    }

    it("should perform NOOP correctly") {
      simulate(new ALU) { dut =>
        dut.io.op.poke(ALUOp.Noop)

        dut.io.a.poke(123.S)
        dut.io.b.poke(456.S)
        dut.clock.step()
        dut.io.result.expect(456.S)
      }
    }
  }
}
