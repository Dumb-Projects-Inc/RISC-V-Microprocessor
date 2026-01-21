package riscv.memory

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import riscv.MemOp
import riscv.MemSize

class CacheControllerSpec extends AnyFlatSpec with ChiselSim with Matchers {
  behavior of "CacheController (data path)"

  /** Drive one-cycle store request */
  private def driveStore(
      c: CacheController,
      addr: BigInt,
      size: MemSize.Type,
      data: BigInt
  ): Unit = {
    c.io.dataPort.enable.poke(true.B)
    c.io.dataPort.memOp.poke(MemOp.Store)
    c.io.dataPort.memSize.poke(size)
    c.io.dataPort.addr.poke(addr.U)
    c.io.dataPort.dataWrite.poke(data.U)

    // data reads are irrelevant on store cycles
  }

  /** Drive one-cycle load request (signed/unsigned) */
  private def driveLoad(
      c: CacheController,
      addr: BigInt,
      size: MemSize.Type,
      unsigned: Boolean
  ): Unit = {
    c.io.dataPort.enable.poke(true.B)
    c.io.dataPort.memOp.poke(if (unsigned) MemOp.LoadUnsigned else MemOp.Load)
    c.io.dataPort.memSize.poke(size)
    c.io.dataPort.addr.poke(addr.U)
    c.io.dataPort.dataWrite.poke(0.U)
  }

  /** Idle all ports */
  private def driveIdle(c: CacheController): Unit = {
    c.io.dataPort.enable.poke(false.B)
    c.io.dataPort.memOp.poke(MemOp.Load)
    c.io.dataPort.memSize.poke(MemSize.Word)
    c.io.dataPort.addr.poke(0.U)
    c.io.dataPort.dataWrite.poke(0.U)
  }

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
      rom.io.dataAddr := dut.io.dataPort.addr
      dut.io.ROMDataIn := rom.io.data
      dut.io.bus.rdData := "hDEADBEEF".U
      dut.io.bus.rdValid := false.B
      dut.io.bus.stall := false.B

      dut.io.instrPort.addr := io.addr
      dut.io.instrPort.enable := true.B

      dut.io.dataPort.addr := 0.U
      dut.io.dataPort.enable := false.B
      dut.io.dataPort.memOp := MemOp.Load
      dut.io.dataPort.dataWrite := 0.U
      dut.io.dataPort.memSize := MemSize.Word

      io.instr := dut.io.instrPort.instr
      io.instrStall := dut.io.instrPort.stall
    }

    simulate(new TestWrapper) { c =>
      c.io.addr.poke("h00000000".U)
      c.clock.step()
      c.io.addr.poke("h00000004".U)
      c.io.instr.expect("h00000001".U)
      c.clock.step()
      c.io.instr.expect("h00000002".U)
      c.io.addr.poke("h00001000".U) // Peripheral region
      c.io.instrStall.expect(false.B)
      c.clock.step()
      // c.io.instrStall.expect(true.B) // we don't stall as no bus for that exists to handle it

    }
  }

  it should "store/load a 32-bit word from ProgramMemory (DDat)" in {
    simulate(new CacheController) { c =>
      // Tie off unused instruction side / bus responses
      c.io.instrPort.enable.poke(false.B)
      c.io.instrPort.addr.poke(0.U)
      c.io.ROMIn.poke(0.U)
      c.io.bus.rdData.poke(0.U)
      c.io.bus.rdValid.poke(false.B)
      c.io.bus.stall.poke(false.B)

      driveIdle(c)
      c.clock.step()

      val addr = 0x00010000L
      val w = BigInt("DEADBEEF", 16)

      // Store (cycle N)
      driveStore(c, addr, MemSize.Word, w)
      c.clock.step()

      // Load request (cycle N+1)
      driveLoad(c, addr, MemSize.Word, unsigned = false)
      c.clock.step()

      // Response available now (cycle N+2): one-cycle SyncReadMem latency + RegNext response
      c.io.dataPort.dataRead.expect(w.U)

      driveIdle(c)
      c.clock.step()
    }
  }

  it should "support byte stores with masking and signed/unsigned byte loads" in {
    simulate(new CacheController) { c =>
      c.io.instrPort.enable.poke(false.B)
      c.io.instrPort.addr.poke(0.U)
      c.io.ROMIn.poke(0.U)
      c.io.bus.rdData.poke(0.U)
      c.io.bus.rdValid.poke(false.B)
      c.io.bus.stall.poke(false.B)

      driveIdle(c); c.clock.step()

      val base = 0x00010010L

      // Initialize the word to 0
      driveStore(c, base, MemSize.Word, 0x00000000L)
      c.clock.step()

      // Store byte 0x80 at base+1 (offset=1)
      driveStore(c, base + 1, MemSize.Byte, 0x80)
      c.clock.step()

      // Signed byte load from base+1 -> 0xFFFFFF80
      driveLoad(c, base + 1, MemSize.Byte, unsigned = false)
      c.clock.step()
      c.io.dataPort.dataRead.expect("hFFFFFF80".U)

      // Unsigned byte load from base+1 -> 0x00000080
      driveLoad(c, base + 1, MemSize.Byte, unsigned = true)
      c.clock.step()
      c.io.dataPort.dataRead.expect("h00000080".U)

      // Also verify other bytes still 0 by reading full word:
      driveLoad(c, base, MemSize.Word, unsigned = false)
      c.clock.step()
      // byte lane 1 should be 0x80, others 0
      c.io.dataPort.dataRead.expect("h00008000".U)
    }
  }

  it should "support halfword stores with masking and signed/unsigned halfword loads" in {
    simulate(new CacheController) { c =>
      c.io.instrPort.enable.poke(false.B)
      c.io.instrPort.addr.poke(0.U)
      c.io.ROMIn.poke(0.U)
      c.io.bus.rdData.poke(0.U)
      c.io.bus.rdValid.poke(false.B)
      c.io.bus.stall.poke(false.B)

      driveIdle(c); c.clock.step()

      val base = 0x00010020L

      // Initialize word
      driveStore(c, base, MemSize.Word, 0x00000000L)
      c.clock.step()

      // Store halfword 0x8001 at base+2 (offset=2, upper halfword)
      driveStore(c, base + 2, MemSize.HalfWord, 0x8001)
      c.clock.step()

      // Signed halfword load from base+2 -> 0xFFFF8001
      driveLoad(c, base + 2, MemSize.HalfWord, unsigned = false)
      c.clock.step()
      c.io.dataPort.dataRead.expect("hFFFF8001".U)

      // Unsigned halfword load from base+2 -> 0x00008001
      driveLoad(c, base + 2, MemSize.HalfWord, unsigned = true)
      c.clock.step()
      c.io.dataPort.dataRead.expect("h00008001".U)

      // Verify full word reflects halfword in upper lanes
      driveLoad(c, base, MemSize.Word, unsigned = false)
      c.clock.step()
      c.io.dataPort.dataRead.expect("h80010000".U)
    }
  }

  it should "preserve unaffected bytes when mixing byte/halfword stores in the same word" in {
    simulate(new CacheController) { c =>
      c.io.instrPort.enable.poke(false.B)
      c.io.instrPort.addr.poke(0.U)
      c.io.ROMIn.poke(0.U)
      c.io.bus.rdData.poke(0.U)
      c.io.bus.rdValid.poke(false.B)
      c.io.bus.stall.poke(false.B)

      driveIdle(c); c.clock.step()

      val base = 0x00010040L

      // Start with a known pattern
      driveStore(c, base, MemSize.Word, 0xaabbccddL)
      c.clock.step()

      // Overwrite byte at offset 0 with 0x11 -> 0xAABBCC11
      driveStore(c, base + 0, MemSize.Byte, 0x11)
      c.clock.step()

      // Overwrite halfword at offset 2 with 0x2222 -> 0x2222CC11? (upper halfword becomes 0x2222)
      driveStore(c, base + 2, MemSize.HalfWord, 0x2222)
      c.clock.step()

      // Read back full word
      driveLoad(c, base, MemSize.Word, unsigned = false)
      c.clock.step()
      c.io.dataPort.dataRead.expect("h2222CC11".U)
    }
  }
}
