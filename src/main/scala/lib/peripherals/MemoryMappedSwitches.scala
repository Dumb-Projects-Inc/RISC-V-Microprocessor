package lib.peripherals

import chisel3._
import chisel3.util._
import lib.Bus
import lib.Addresses

class MemoryMappedSwitches(
    cnt: Int,
    baseAddr: BigInt = Addresses.SWITCH_ADDR
) extends Module {
  require(cnt <= 32, "Only up to 32 Switches can be controlled through the bus")

  val io = IO(new Bundle {
    val bus = Bus.RespondPort()
    val pins = Input(UInt(cnt.W))
  })
  io.bus.init()

  val ADDR = baseAddr.U(32.W)
  val switchReg = RegInit(0.U(cnt.W))
  val hitRead = io.bus.hasReadRequestAt(ADDR)

  switchReg := switchReg

  when(RegNext(hitRead, false.B)) {
    io.bus.rdData := Cat(0.U((32 - cnt).W), io.pins)
  }

}
