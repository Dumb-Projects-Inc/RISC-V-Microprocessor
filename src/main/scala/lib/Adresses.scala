package lib

import riscv.memory.MemoryMap

object Addresses {
  val UART_ADDR =
    MemoryMap.peripheralsStart // 2 words = 0x00010000 to 0x00010008
  val LED_ADDR =
    MemoryMap.peripheralsStart + 0x08 // 1 words = 0x00001008 to 0x0000100B
  val SWITCH_ADDR =
    MemoryMap.peripheralsStart + 0x0c // 1 words = 0x00001010 to 0x00001014
}
