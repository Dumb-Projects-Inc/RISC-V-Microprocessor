package riscv

import chisel3._
import chisel3.util.{Decoupled}

class RegisterFile extends Module {
  val io = IO(new Bundle {
    val readReg1 = Flipped(Decoupled(UInt(5.W)))
    val readReg2 = Flipped(Decoupled(UInt(5.W)))
    val writeReg = Flipped(Decoupled(UInt(5.W)))
    val writeData = Input(UInt(32.W))
    val readData1 = Decoupled(UInt(32.W))
    val readData2 = Decoupled(UInt(32.W))
  })

  // always readu for read requests
  io.readReg1.ready := true.B
  io.readReg2.ready := true.B
  io.writeReg.ready := true.B

  // Yes this wastes 32 bits for x0, but simplifies logic
  val regFile = SyncReadMem(32, UInt(32.W))

  val r1 = regFile.read(io.readReg1.bits, io.readReg1.valid)
  val r2 = regFile.read(io.readReg2.bits, io.readReg2.valid)
  val r1ValidReg = RegNext(io.readReg1.valid, false.B)
  val r2ValidReg = RegNext(io.readReg2.valid, false.B)

  io.readData1.bits := Mux(io.readReg1.bits === 0.U, 0.U, r1)
  io.readData2.bits := Mux(io.readReg2.bits === 0.U, 0.U, r2)
  // 1-clock delay for read data valid
  io.readData1.valid := r1ValidReg
  io.readData2.valid := r2ValidReg

  when(io.writeReg.valid && (io.writeReg.bits =/= 0.U)) {
    regFile.write(io.writeReg.bits, io.writeData)
  }

}
