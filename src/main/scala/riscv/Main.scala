package riscv

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, FirtoolOption}

// very simple bootloader that waits for a button press and jumps to an address in memory (the program)
object Bootloader {
  val ROM = """
  addi x1, x0, 0xfff 'load some memory region
  addi x2, x0, 1000 'counter (4 bytes * 1000 = 4 KB)
  addi x3, x0, 0
  add x4, x0, x1 'increasing address
  fill_cache:
    lw x5, 0(x4)      'load from memory
    addi x4, x4, 4    'next word
    addi x3, x3, 1    'increment counter
    blt x3, x2, fill_cache 'loop until done

  addi x2, x0, 0x1002 'btn_c address
  wait:
    lw x3, 0(x2)       'load btn_c state
    beq x3, x0, wait    'wait for button press
  jalr x0, 0(x1)      'jump to loaded address
"""
  val TEST = """
  addi x1, x0, 4
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  on:
    lw x2, 0(x1)
  lui x3, 0x006D0
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  delay:
    addi x3, x3, -1
    addi x0, x0, 0
    addi x0, x0, 0
    addi x0, x0, 0
    bne x3, x0, delay
    addi x0, x0, 0
    addi x0, x0, 0
    addi x0, x0, 0
    addi x0, x0, 0
    addi x0, x0, 0
  lw x2, 0(x0) 
  lui x3, 0x006D0
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  delay2:
    addi x3, x3, -1
    addi x0, x0, 0
    addi x0, x0, 0
    addi x0, x0, 0
    addi x0, x0, 0
    addi x0, x0, 0
    bne x3, x0, delay2
    addi x0, x0, 0
    addi x0, x0, 0
    addi x0, x0, 0
    addi x0, x0, 0
    addi x0, x0, 0
  jal x0, on  
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
"""

}

class RV32Debug extends Bundle {
  val instrAddr = UInt(32.W)
  val instr = UInt(32.W)
}

class RV32ITop(debug: Boolean = false) extends Module {
  val io = IO(new Bundle {
    // val sw = Input(Bits(16.W)) // 16 switches that should be readable
    // val btn = Input(Bits(4.W)) // 4 buttons (1 for reset)
    val LED = Output(Bits(16.W)) // 16 LEDs that should be writable

    // val txd = Output(Bool()) // for uart
    // val rxd = Input(Bool()) // for uart
  })

  val dbg = if (debug) Some(IO(Output(new RV32Debug()))) else None

  val ROM = Module(new InstructionROM(program = Bootloader.TEST))
  val MMU = Module(
    new CacheController()
  )

  val pipeline = Module(new Pipeline(debug = debug))
  if (debug) {
    dbg.get.instrAddr := pipeline.dbg.get.pc
    dbg.get.instr := pipeline.io.instrPort.instr
  }

  pipeline.io.instrPort <> MMU.io.instrPort
  pipeline.io.dataPort <> MMU.io.dataPort

  ROM.io.addr := pipeline.io.instrPort.addr
  MMU.io.ROMIn := ROM.io.instruction
  val led = RegInit(255.U(16.W))
  when(pipeline.io.dataPort.enable) {
    led := pipeline.io.dataPort.addr(15, 0)
  }
  io.LED := led

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
