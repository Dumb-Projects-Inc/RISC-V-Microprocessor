package riscv.memory

import com.carlosedp.riscvassembler.RISCVAssembler
import chisel3._

/** Bootloader assembly programs, this should include a way to read files later.
  */

// very simple bootloader that waits for a button press and jumps to an address in memory (the program)
object Bootloader {
  private val ROM = """
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
  private val TEST = """
  addi x1, x0, 252
  on:
    lw x2, 0(x1)
  lui x3, 0x006D0
  delay:
    addi x3, x3, -1
    bne x3, x0, delay
  lw x2, 0(x0) 
  lui x3, 0x006D0
  delay2:
    addi x3, x3, -1
    bne x3, x0, delay2
  jal x0, on
  addi x0, x0, 0
  addi x0, x0, 0
  addi x0, x0, 0 
"""
  private val helloWorld = """
  addi x14, x0, 0
  lui x14, 0x000010 
  addi x11, x0, 72
  sb x11, 0(x14)
  addi x11, x0 , 101
  sb x11, 1(x14)
  addi x11, x0, 108
  sb x11, 2(x14)
  sb x11, 3(x14)
  addi x11, x0, 111
  sb x11, 4(x14)
  addi x11, x0, 44
  sb x11, 5(x14)
  addi x11, x0, 32
  sb x11, 6(x14)
  addi x11, x0, 87
  sb x11, 7(x14)
  addi x11, x0, 111
  sb x11, 8(x14)
  addi x11, x0, 114
  sb x11, 9(x14)
  addi x11, x0, 108
  sb x11, 10(x14)
  addi x11, x0, 100
  sb x11, 11(x14)
  addi x11, x0, 33
  sb x11, 12(x14)
  addi x11, x0, 10
  sb x11, 13(x14)

start:
  addi x14, x0, 0
  lui x14, 0x000010
  lui  x10, 0x00001
  addi x3, x0, 14

write:
  lbu  x11, 0(x14)
  sw   x11, 0(x10)
  addi x14, x14, 1
  addi x3, x3, -1
  bne  x3, x0, write

  lui  x3, 0x006D0
delay:
  addi x3, x3, -1
  bne  x3, x0, delay
  jal  x0, start

  """

  def assemble(program: String): Seq[UInt] = {
    val prog = RISCVAssembler
      .fromString(program)
      .split("\\R")
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(h => BigInt(h, 16).U(32.W))
      .toSeq
    if (prog.isEmpty) {
      println("Warning: failed to assemble a program")

    }
    prog
  }

  def BinToSeq(bin: Array[Byte]): Seq[UInt] = {
    bin
      .grouped(4)
      .map { bytes =>
        val word = bytes.zipWithIndex
          .map { case (b, i) => BigInt(b & 0xff) << (8 * i) }
          .foldLeft(BigInt(0))(_ | _)
        word.U(32.W)
      }
      .toSeq
  }

  def filetoSeq(path: String): Seq[UInt] = {
    val bin = java.nio.file.Files.readAllBytes(
      java.nio.file.Paths.get(path)
    )
    println("Loading file " + path + " of size " + bin.length + " bytes")
    BinToSeq(bin)
  }

  val TEST_HEX: Seq[UInt] = assemble(TEST)
  val ROM_HEX: Seq[UInt] = assemble(ROM)
  val HELLO_WORLD_HEX: Seq[UInt] = assemble(helloWorld)
}
