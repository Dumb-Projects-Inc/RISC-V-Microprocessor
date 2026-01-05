package riscv

import chisel3._
import chisel3.util.SRAM
import chisel3.util.Decoupled

class RegisterFile extends Module {
  val io = IO(new Bundle {
    val readReg1 = Flipped(Decoupled(UInt(5.W)))
    val readReg2 = Flipped(Decoupled(UInt(5.W)))
    val writeReg = Flipped(Decoupled(UInt(5.W)))
    val writeData = Input(UInt(32.W))
    val readData1 = Decoupled(UInt(32.W))
    val readData2 = Decoupled(UInt(32.W))
  })

  // Regfile as 2 port read 1 port write and 0 read/write ports
  val regFile = SRAM(32, UInt(32.W), 2, 1, 0)

  when(io.readReg1.valid) {
    regFile.readPorts(0).address := io.readReg1.bits
    regFile.readPorts(0).enable := true.B
  }.otherwise {
    regFile.readPorts(0).enable := false.B
  }
  when(io.readReg2.valid) {
    regFile.readPorts(1).address := io.readReg2.bits
    regFile.readPorts(1).enable := true.B
  }.otherwise {
    regFile.readPorts(1).enable := false.B
  }

  // When data is read, set the valid signal
  io.readData1.valid := false.B
  io.readData2.valid := false.B
  io.readData1.bits := regFile.readPorts(0).data
  io.readData2.bits := regFile.readPorts(1).data

  when(regFile.readPorts(0).enable) {
    io.readData1.valid := true.B
  }

  when(regFile.readPorts(1).enable) {
    io.readData2.valid := true.B
  }

  when(io.writeReg.valid && io.writeReg.bits =/= 0.U) {
    regFile.writePorts(0).address := io.writeReg.bits
    regFile.writePorts(0).data := io.writeData
    regFile.writePorts(0).enable := true.B
  }.otherwise {
    regFile.writePorts(0).enable := false.B
  }
}
