package riscv

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, FirtoolOption}
import com.carlosedp.riscvassembler.RISCVAssembler

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
  addi x1, x0, 0xff
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  on:
    lw x2, 0(x1)
  lui x3, 0xFFFFF #delay
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  delay:
    addi x3, x3, -1
    bne x3, x0, delay
    addi x0, x0, 0
    addi x0, x0, 0
    addi x0, x0, 0
    addi x0, x0, 0
    addi x0, x0, 0
  lw x2, 0(x0) # turn off
  lui x3, 0xFFFFF #delay
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0
  delay2:
    addi x3, x3, -1
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

  val romhex = RISCVAssembler.fromString(TEST.stripMargin)
}
class RV32ITop extends Module {
  val io = IO(new Bundle {
    // val sw = Input(Bits(16.W)) // 16 switches that should be readable
    // val btn = Input(Bits(3.W)) // 5 buttons
    val LED = Output(Bits(16.W)) // 16 LEDs that should be writable

    // val txd = Output(Bool()) // for uart
    // val rxd = Input(Bool()) // for uart
  })

  val MMU = Module(
    new CacheController(ROMProgram = Bootloader.romhex)
  ) // 64 KB memory
  val pipeline = Module(new Pipeline(debug = true, debugPrint = true))
  pipeline.io.instrPort <> MMU.io.instrPort
  pipeline.io.dataPort <> MMU.io.dataPort
  io.LED := pipeline.io.dataPort.addr(15, 0)

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
