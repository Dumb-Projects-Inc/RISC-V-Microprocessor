package riscv.memory

import com.carlosedp.riscvassembler.RISCVAssembler

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
  def assemble(program: String): String = {
    RISCVAssembler
      .fromString(program)
  }

  val TEST_HEX: String = assemble(TEST)
  val ROM_HEX: String = assemble(ROM)
}
