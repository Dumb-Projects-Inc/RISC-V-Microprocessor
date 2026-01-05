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

class InstrMem(depthWords: Int) extends Module {
    val io = IO(new Bundle {
        val fetch = new FetchPort
    })

    val mem = SyncReadMem(depthWords, UInt(32.W))

    val respHolding = RegInit(false.B)
    val respDataReg = Reg(UInt(32.W))

    io.fetch.req.ready := !respHolding

    val doRead = io.fetch.req.fire
    val readIdx  = io.fetch.req.bits.addr(31, 2)
    val readData = mem.read(readIdx, doRead)

    val respValidNext = RegNext(doRead, init = false.B)

    when(respValidNext) {
        respHolding := true.B
        respDataReg := readData
    }.elsewhen(io.fetch.resp.fire) {
        respHolding := false.B
    }

    io.fetch.resp.valid     := respHolding
    io.fetch.resp.bits.data := respDataReg
}
