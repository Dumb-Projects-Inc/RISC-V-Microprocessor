.text
.global main
main:
    li sp, 0x20000
    addi sp, sp, -8 # 0x1fff8
    
    li t0, 0xDEADBEEF
    sw t0, 0(sp) # 0x1fff8
    lw t1, 0(sp)
    
    li t2, 0x12345678
    sw t2, 4(sp) # 0x1fffc
    lw t3, 4(sp)

    li a7, 10
    ecall
    nop
    nop
    nop
    nop
