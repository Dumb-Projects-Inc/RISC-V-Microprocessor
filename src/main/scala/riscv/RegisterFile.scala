package riscv

import chisel3._

/** Register File implementation for RISC-V, using SyncReadMem
  * @param debug
  *   enable register mirroring for easy debugging (should not be synthesized to
  *   board)
  */
class RegisterFile(debug: Boolean = false)
    extends Module
    with RequireSyncReset {
  val io = IO(new Bundle {
    val readReg1 = Input(UInt(5.W))
    val readReg2 = Input(UInt(5.W))
    val writeReg = Input(UInt(5.W))
    val wrEn = Input(Bool())
    val writeData = Input(UInt(32.W))
    val reg1Data = Output(UInt(32.W))
    val reg2Data = Output(UInt(32.W))
  })

  // ##################################################
  // Debugging interface, should never be synthesized to board
  // ##################################################
  val dbg = if (debug) Some(IO(Output(Vec(32, UInt(32.W))))) else None

  // Back to Asynchronous Read (RegInit) architecture which passed all tests.
  // This uses Flip-Flops, allowing combinational reads.
  val regs = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))

  if (debug) {
    dbg.get := regs
  }

  val r1Data = regs(io.readReg1)
  val r2Data = regs(io.readReg2)

  val isZero1 = io.readReg1 === 0.U
  val isZero2 = io.readReg2 === 0.U

  io.reg1Data := Mux(isZero1, 0.U, r1Data)
  io.reg2Data := Mux(isZero2, 0.U, r2Data)

  when(io.wrEn && (io.writeReg =/= 0.U)) {
    regs(io.writeReg) := io.writeData
  }

}
