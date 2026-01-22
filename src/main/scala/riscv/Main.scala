package riscv

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, FirtoolOption}
import riscv.memory.CacheController
import riscv.memory.Bootloader
import lib.peripherals.{MMIOUart, MemoryMappedLeds, MemoryMappedSwitches}
import riscv.memory.MemoryMap
import lib.Addresses

class RV32Debug extends Bundle {
  val instrAddr = UInt(32.W)
  val instr = UInt(32.W)
  val dataAddr = UInt(32.W)
  val dataEn = Bool()
  val writebackPc = UInt(32.W)
  val regs = Vec(32, UInt(32.W))
}

/** Top-level module for RV32I processor, this maps the pipeline to memory, as
  * well as maps the bus to peripherals.
  *
  * @param debug
  */
class RV32ITop(program: String = "", debug: Boolean = false)
    extends Module
    with RequireSyncReset {
  val io = IO(new Bundle {
    val sw = Input(Bits(16.W)) // 16 switches that should be readable
    val LED = Output(Bits(16.W)) // 16 LEDs that should be writable

    val txd = Output(Bool()) // for uart
    val rxd = Input(Bool()) // for uart
  })
  var prog_bytes = Bootloader.HELLO_WORLD_HEX
  if (program.nonEmpty) {
    prog_bytes = Bootloader.filetoSeq(program)
  }

  val MMU = Module(
    new CacheController(rom = prog_bytes)
  )

  val pipeline = Module(
    new Pipeline(
      debug = debug,
      pcInit = MemoryMap.romStart
    )
  )

  pipeline.io.instrPort <> MMU.io.instrPort
  pipeline.io.dataPort <> MMU.io.dataPort

  val uart = Module(
    new MMIOUart(Freq = 100000000, BaudRate = 9600)
  )
  io.txd := uart.io.tx
  uart.io.rx := io.rxd

  MMU.io.bus <> uart.io.dbus

  // val led = Module(new MemoryMappedLeds(16, baseAddr = Addresses.LED_ADDR))
  // led.io.bus <> MMU.io.bus
  // io.LED := led.io.pins
  val led = RegInit(0.U(16.W))
  io.LED := led
  when(pipeline.io.dataPort.enable) {
    led := pipeline.io.dataPort.addr(15, 0)
  }

  // val switches = Module(
  //  new MemoryMappedSwitches(16, baseAddr = Addresses.SWITCH_ADDR)
  // )
  // switches.io.pins := io.sw
  // switches.io.bus <> MMU.io.bus
  // ###############################################
  // Debugging interface
  // ###############################################
  val dbg = if (debug) Some(IO(Output(new RV32Debug()))) else None
  if (debug) {
    dbg.get.instrAddr := pipeline.dbg.get.pc
    dbg.get.dataAddr := pipeline.io.dataPort.addr
    dbg.get.instr := pipeline.io.instrPort.instr
    dbg.get.dataEn := pipeline.io.dataPort.enable
    dbg.get.writebackPc := pipeline.dbg.get.writebackPc
    dbg.get.regs := pipeline.dbg.get.regs
  }

}

object RV32IMain {
  def main(args: Array[String]): Unit = {
    // ChiselStage invocation
    val stageArgs = Array(
      "--target",
      "systemverilog",
      "--target-dir",
      "generated",
      "--split-verilog"
    )
    var program = ""
    if (args.nonEmpty) {
      program = args(0)
    }

    (new ChiselStage).execute(
      stageArgs,
      Seq(
        ChiselGeneratorAnnotation(() => new RV32ITop(program = program)),
        FirtoolOption("--disable-all-randomization")
      )
    )
  }
}
