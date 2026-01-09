package riscv

import chisel3._
import chisel3.util.{Valid}

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

  val r1 = regFile.read(io.readReg1, true.B)
  val r2 = regFile.read(io.readReg2, true.B)

  io.reg1Data := Mux(io.readReg1 === 0.U, 0.U, r1)
  io.reg2Data := Mux(io.readReg2 === 0.U, 0.U, r2)

  when(io.wrEn && (io.writeReg =/= 0.U)) {
    regFile.write(io.writeReg, io.writeData)
  }

  val dbg = if (debug) Some(IO(Output(Vec(32, UInt(32.W))))) else None

  if (debug) {
    val debugRegs = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
    when(io.wrEn && (io.writeReg =/= 0.U)) {
      debugRegs(io.writeReg) := io.writeData
    }
    dbg.get := debugRegs
  }

}
