package riscv

import chisel3._
import chisel3.util.{Valid}

class RegisterFile extends Module {
  val io = IO(new Bundle {
    val readReg1 = Input(UInt(5.W))
    val readReg2 = Input(UInt(5.W))
    val writeReg = Input(Valid(UInt(5.W)))
    val writeData = Input(UInt(32.W))
    val readData1 = Output(UInt(32.W))
    val readData2 = Output(UInt(32.W))
  })

  // Yes this wastes 32 bits for x0, but simplifies logic
  val regFile = SyncReadMem(32, UInt(32.W))

  val r1 = regFile.read(io.readReg1, true.B)
  val r2 = regFile.read(io.readReg2, true.B)

  io.readData1 := Mux(io.readReg1 === 0.U, 0.U, r1)
  io.readData2 := Mux(io.readReg2 === 0.U, 0.U, r2)

  when(io.writeReg.valid && (io.writeReg.bits =/= 0.U)) {
    regFile.write(io.writeReg.bits, io.writeData)
  }

}
