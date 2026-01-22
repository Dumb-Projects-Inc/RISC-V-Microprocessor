package lib.peripherals

import chisel3._
import chisel3.util._
import lib.Bus
import lib.Addresses

class MemoryMappedLeds(
    cnt: Int,
    baseAddr: BigInt = Addresses.LED_ADDR
) extends Module {
  require(cnt <= 32, "Only up to 32 LEDs can be controlled through the bus")

  val io = IO(new Bundle {
    val bus = Bus.RespondPort()
    val pins = Output(UInt(cnt.W))
  })
  io.bus.init()

  val ADDR = baseAddr.U(32.W)
  val ledReg = RegInit(0.U(cnt.W))
  val hitRead = io.bus.hasReadRequestAt(ADDR)
  val hitWrite = io.bus.hasWriteRequestAt(ADDR)

  io.pins := ledReg

  when(hitWrite) {
    ledReg := io.bus.wrData(cnt - 1, 0)
  }

  // TODO: further logic
}
