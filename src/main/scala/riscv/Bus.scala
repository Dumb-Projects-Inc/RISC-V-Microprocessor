package riscv

import chisel3._
import chisel3.util._

object Bus {

  sealed abstract class Port extends Bundle {
    val read = Output(Bool())
    val write = Output(Bool())
    val addr = Output(UInt(32.W))
    val wrData = Output(UInt(32.W))

    val rdData = Input(UInt(32.W))
    val stall = Input(Bool())
    val rdValid = Input(Bool())

    def init(): Unit
  }

  sealed class RequestPort extends Port {
    def writeRequest(a: UInt, d: UInt): Unit = {
      write := true.B
      read := false.B
      addr := a
      wrData := d
    }

    def readRequest(a: UInt): Unit = {
      write := false.B
      read := true.B
      addr := a
      wrData := DontCare
    }

    def init(): Unit = {
      write := false.B
      read := false.B
      addr := DontCare
      wrData := DontCare
    }
  }

  sealed class RespondPort extends Port {
    def hasWriteRequestAt(a: UInt): Bool = write && (addr === a)
    def hasReadRequestAt(a: UInt): Bool = read && (addr === a)

    def init(): Unit = {
      rdData := 0.U
      stall := false.B
      rdValid := false.B
    }
  }

  object RespondPort {
    def apply(): RespondPort = Flipped(new RespondPort)
  }
  object RequestPort {
    def apply(): RequestPort = new RequestPort
  }
}

class DataPortBus extends Module {
  val io = IO(new Bundle {
    val cpu = Flipped(new dataPort)
    val bus = Bus.RequestPort()
  })
  io.bus.wrData := io.cpu.dataWrite
  io.bus.addr := io.cpu.addr

  val isReq = io.cpu.enable
  io.bus.write := isReq && io.cpu.writeEn
  io.bus.read := isReq && !io.cpu.writeEn

  io.cpu.stall := io.bus.stall

  io.cpu.dataRead := io.bus.rdData
}

class SharedBus extends Module {
  val io = IO(new Bundle {
    val master = Bus.RequestPort()
    val led = Bus.RespondPort()
    val mem = Bus.RespondPort()
  })

  val ledAddr = "h00001000".U(32.W)

  val rdData = WireDefault(0.U(32.W))
  val stall = WireDefault(false.B)
  val rdValid = WireDefault(false.B)
  val selectLED = (io.master.addr === ledAddr)

  // led
  io.led.read := io.master.read
  io.led.write := io.master.write
  io.led.addr := io.master.addr
  io.led.wrData := io.master.wrData

  // memory
  io.mem.read := io.master.read
  io.mem.write := io.master.write
  io.mem.addr := io.master.addr
  io.mem.wrData := io.master.wrData

  when(selectLED) {
    rdData := io.led.rdData
    stall := io.led.stall
    rdValid := io.led.rdValid
  }.otherwise {
    rdData := io.mem.rdData
    stall := io.mem.stall
    rdValid := io.mem.rdValid
  }

  io.master.rdData := rdData
  io.master.stall := stall
  io.master.rdValid := rdValid

}
