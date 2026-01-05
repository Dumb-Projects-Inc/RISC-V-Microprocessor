package riscv

import chisel3._
import org.scalatest.funspec.AnyFunSpec
import chisel3.simulator.scalatest.ChiselSim

class ImmGenTB extends AnyFunSpec with ChiselSim {
  describe("ImmGen") {
    it("should correctly reconstruct immediates") {
      simulate(new ImmGen) { dut =>
        // addi	x1,x0,1
        dut.io.instr.poke("h00100093".U)
        dut.io.format.poke(Format.I)
        dut.io.out.expect(1.S)

        // addi	x2,x0,-1
        dut.io.instr.poke("hfff00113".U)
        dut.io.format.poke(Format.I)
        dut.io.out.expect(-1.S)

        // addi	x3,x0,1024
        dut.io.instr.poke("h40000193".U)
        dut.io.format.poke(Format.I)
        dut.io.out.expect(1024.S)

        // addi    x4,x0,-2048
        dut.io.instr.poke("h80000213".U)
        dut.io.format.poke(Format.I)
        dut.io.out.expect(-2048.S)

        // lui     x5,0x1
        dut.io.instr.poke("h000012b7".U)
        dut.io.format.poke(Format.U)
        dut.io.out.expect(4096.S)

        // lui     x6,0xfffff
        dut.io.instr.poke("hfffff337".U)
        dut.io.format.poke(Format.U)
        dut.io.out.expect(-4096.S)

        // auipc   x7,0x12345
        dut.io.instr.poke("h12345397".U)
        dut.io.format.poke(Format.U)
        dut.io.out.expect(305418240.S)

        // sw      x1,4(x2)
        dut.io.instr.poke("h00112223".U)
        dut.io.format.poke(Format.S)
        dut.io.out.expect(4.S)

        // sw      x1,-4(x2)
        dut.io.instr.poke("hfe112e23".U)
        dut.io.format.poke(Format.S)
        dut.io.out.expect(-4.S)

        // sw      x1,100(x2)
        dut.io.instr.poke("h06112223".U)
        dut.io.format.poke(Format.S)
        dut.io.out.expect(100.S)

        // beq     x0,x0,16
        dut.io.instr.poke("h00000863".U)
        dut.io.format.poke(Format.B)
        dut.io.out.expect(16.S)

        // beq     x0,x0,0
        dut.io.instr.poke("h00000063".U)
        dut.io.format.poke(Format.B)
        dut.io.out.expect(0.S)

        // beq     x0,x0,-12
        dut.io.instr.poke("hfe000ae3".U)
        dut.io.format.poke(Format.B)
        dut.io.out.expect(-12.S)

        // addi	x0,x0,0
        dut.io.instr.poke("h00000013".U)
        dut.io.format.poke(Format.I)
        dut.io.out.expect(0.S)

        // jal     x0,48, 12
        dut.io.instr.poke("h00c0006f".U)
        dut.io.format.poke(Format.J)
        dut.io.out.expect(12.S)

        // jal     x0,0, -72
        dut.io.instr.poke("hfb9ff06f".U)
        dut.io.format.poke(Format.J)
        dut.io.out.expect(-72.S)

        // assorted ai generated cases

        // S-Type Max Positive (2047)
        dut.io.instr.poke("h7e000fa3".U)
        dut.io.format.poke(Format.S)
        dut.io.out.expect(2047.S)

        // S-Type Min Negative (-2048)
        dut.io.instr.poke("h80002023".U)
        dut.io.format.poke(Format.S)
        dut.io.out.expect(-2048.S)

        // B-Type Max Positive (4094) - Critical check for bit 11 vs 12 swap
        dut.io.instr.poke("h7e000fe3".U)
        dut.io.format.poke(Format.B)
        dut.io.out.expect(4094.S)

        // B-Type Min Negative (-4096)
        dut.io.instr.poke("h80000063".U)
        dut.io.format.poke(Format.B)
        dut.io.out.expect(-4096.S)

        // J-Type Max Positive (1048574)
        dut.io.instr.poke("h7ffff06f".U)
        dut.io.format.poke(Format.J)
        dut.io.out.expect(1048574.S)

        // J-Type Min Negative (-1MB)
        dut.io.instr.poke("h8000006f".U)
        dut.io.format.poke(Format.J)
        dut.io.out.expect(-1048576.S)

      }
    }
  }
}
