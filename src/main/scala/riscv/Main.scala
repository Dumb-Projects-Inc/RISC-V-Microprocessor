package riscv

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, FirtoolOption}
import riscv.memory.CacheController
import riscv.memory.InstructionROM
import riscv.memory.Bootloader
import lib.peripherals.MMIOUart

class RV32Debug extends Bundle {
  val instrAddr = UInt(32.W)
  val instr = UInt(32.W)
  val dataAddr = UInt(32.W)
  val dataEn = Bool()
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
    // val sw = Input(Bits(16.W)) // 16 switches that should be readable
    val btn = Input(Bits(4.W)) // 4 buttons (1 for reset)
    val LED = Output(Bits(16.W)) // 16 LEDs that should be writable

    val txd = Output(Bool()) // for uart
    val rxd = Input(Bool()) // for uart
  })
  var prog_bytes = Bootloader.HELLO_WORLD_HEX
  if (program.nonEmpty) {
    prog_bytes = Bootloader.filetoSeq(program)
  }

  val ROM = Module(new InstructionROM(program = prog_bytes))
  val MMU = Module(
    new CacheController()
  )

  val deb_btn = RegNext(RegNext(io.btn), 0.U) // simple debouncer

  val pipeline = Module(new Pipeline(debug = debug))

  pipeline.io.instrPort <> MMU.io.instrPort
  pipeline.io.dataPort <> MMU.io.dataPort

  ROM.io.addr := pipeline.io.instrPort.addr
  MMU.io.ROMIn := ROM.io.instruction

  val uart = Module(
    new MMIOUart(Freq = 100000000, BaudRate = 9600, baseAddr = 0x00001000)
  )
  io.txd := uart.io.tx
  uart.io.rx := io.rxd

  MMU.io.bus <> uart.io.dbus

  val led = RegInit(255.U(16.W))
  when(pipeline.io.dataPort.enable) {
    led := pipeline.io.dataPort.addr(15, 0)
  }
  io.LED := led

  // ###############################################
  // Debugging interface
  // ###############################################
  val dbg = if (debug) Some(IO(Output(new RV32Debug()))) else None
  if (debug) {
    dbg.get.instrAddr := pipeline.dbg.get.pc
    dbg.get.dataAddr := pipeline.io.dataPort.addr
    dbg.get.instr := pipeline.io.instrPort.instr
    dbg.get.dataEn := pipeline.io.dataPort.enable
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
