package lib

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
