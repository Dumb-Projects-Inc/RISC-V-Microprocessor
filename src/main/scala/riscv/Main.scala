package riscv

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, FirtoolOption}
import riscv.memory.CacheController
import riscv.memory.InstructionROM
import riscv.memory.Bootloader

class RV32Debug extends Bundle {
  val instrAddr = UInt(32.W)
  val instr = UInt(32.W)
}

/** Top-level module for RV32I processor, this maps the pipeline to memory, as
  * well as maps the bus to peripherals.
  *
  * @param debug
  */
class RV32ITop(debug: Boolean = false) extends Module {
  val io = IO(new Bundle {
    // val sw = Input(Bits(16.W)) // 16 switches that should be readable
    val btn = Input(Bits(4.W)) // 4 buttons (1 for reset)
    val LED = Output(Bits(16.W)) // 16 LEDs that should be writable

    // val txd = Output(Bool()) // for uart
    // val rxd = Input(Bool()) // for uart
  })

  val ROM = Module(new InstructionROM(program = Bootloader.TEST_HEX))
  val MMU = Module(
    new CacheController()
  )

  val deb_btn = RegNext(RegNext(io.btn), 0.U) // simple debouncer

  val pipeline = Module(new Pipeline(debug = debug))

  pipeline.io.instrPort <> MMU.io.instrPort
  pipeline.io.dataPort <> MMU.io.dataPort

  ROM.io.addr := pipeline.io.instrPort.addr
  MMU.io.ROMIn := ROM.io.instruction
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
    dbg.get.instr := pipeline.io.instrPort.instr
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

    (new ChiselStage).execute(
      stageArgs,
      Seq(
        ChiselGeneratorAnnotation(() => new RV32ITop()),
        FirtoolOption("--disable-all-randomization")
      )
    )
  }
}
