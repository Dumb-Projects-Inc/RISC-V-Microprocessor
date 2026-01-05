package riscv

import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, FirtoolOption}

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
