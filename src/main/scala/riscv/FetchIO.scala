package riscv

import chisel3._
import chisel3.util._

class FetchReq extends Bundle {
    val addr = UInt(32.W)
}

class FetchResp extends Bundle {
    val data = UInt(32.W)
}

class FetchPort extends Bundle {
    val req  = Flipped(Decoupled(new FetchReq))
    val resp = Decoupled(new FetchResp)
}