package lib

object Addresses {
  val UART_ADDR = 0x00001000L // 2 words = 0x00010000 to 0x00010008
  val LED_ADDR = 0x000001008L // 1 words = 0x00001008 to 0x0000100B
  val SWITCH_ADDR = 0x0000100cL // 1 words = 0x00001010 to 0x00001014
}
