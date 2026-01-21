.text
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
ecall