package riscv

import chisel3._
import chisel3.util.{Valid}

/** Register File implementation for RISC-V, using SyncReadMem
  * @param debug
  *   enable register mirroring for easy debugging (should not be synthesized to
  *   board)
  */
class RegisterFile(debug: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val readReg1 = Input(UInt(5.W))
    val readReg2 = Input(UInt(5.W))
    val writeReg = Input(UInt(5.W))
    val wrEn = Input(Bool())
    val writeData = Input(UInt(32.W))
    val reg1Data = Output(UInt(32.W))
    val reg2Data = Output(UInt(32.W))
  })

  // Yes this wastes 32 bits for x0, but simplifies logic
  val regFile = SyncReadMem(32, UInt(32.W))

  val r1Data = regFile.read(io.readReg1, true.B)
  val r2Data = regFile.read(io.readReg2, true.B)

  val isZero1 = RegNext(io.readReg1 === 0.U)
  val isZero2 = RegNext(io.readReg2 === 0.U)

  io.reg1Data := Mux(isZero1, 0.U, r1Data)
  io.reg2Data := Mux(isZero2, 0.U, r2Data)

  when(io.wrEn) {
    regFile.write(io.writeReg, io.writeData)
  }

  // ##################################################
  // Debugging interface, should never be synthesized to board
  // ##################################################
  val dbg = if (debug) Some(IO(Output(Vec(32, UInt(32.W))))) else None

  if (debug) {

    val debugRegs = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
    when(io.wrEn && (io.writeReg =/= 0.U)) {
      debugRegs(io.writeReg) := io.writeData
    }
    dbg.get := debugRegs
  }

}
