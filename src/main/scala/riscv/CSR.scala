package riscv

import chisel3._
import chisel3.util._

object CSRCmd extends ChiselEnum {
  val RW, RS, RC = Value
}

class CSR extends Module {
  val io = IO(new Bundle {
    val currentPc = Input(UInt(32.W))
    val retValid = Input(Bool())

    val trap = new Bundle {
      val valid = Input(Bool())
      val cause = Input(UInt(32.W))
    }

    val redirect = new Bundle {
      val valid = Output(Bool())
      val pc = Output(UInt(32.W))
    }

    val csr = new Bundle {
      val cmd = Input(CSRCmd())
      val addr = Input(UInt(12.W))
      val valid = Input(Bool())
      val writeData = Input(UInt(32.W))
      val readData = Output(UInt(32.W))
    }

  })

  val mstatus = RegInit(0.U(32.W))
  val mtvec = RegInit(0.U(32.W))
  val mscratch = RegInit(0.U(32.W))
  val mepc = RegInit(0.U(32.W))
  val mcause = RegInit(0.U(32.W))
  val mtval = RegInit(0.U(32.W))

  val cycle = RegInit(0.U(64.W))

  io.redirect.valid := false.B
  io.redirect.pc := 0.U
  io.csr.readData := 0.U

  cycle := cycle + 1.U

  def csrRead(addr: UInt): UInt = MuxLookup(addr, 0.U)(
    Seq(
      "h300".U -> mstatus, // used for interrupt enable/disable. Do we need it?
      "h305".U -> mtvec,
      "h340".U -> mscratch, // used as a temporary register by trap handlers.
      "h341".U -> mepc,
      "h342".U -> mcause,
      "h343".U -> mtval, // used for more details on traps. Might not needed too?

      // debugging counters
      "hC00".U -> cycle(31, 0),
      "hC80".U -> cycle(63, 32)
    )
  )

  def csrWrite(old: UInt, wdata: UInt, cmd: CSRCmd.Type): UInt =
    MuxLookup(cmd.asUInt, old)(
      Seq(
        CSRCmd.RW.asUInt -> wdata,
        CSRCmd.RS.asUInt -> (old | wdata),
        CSRCmd.RC.asUInt -> (old & ~wdata)
      )
    )

  when(io.trap.valid) {
    mepc := io.currentPc
    mcause := io.trap.cause
    io.redirect.valid := true.B
    io.redirect.pc := mtvec
  }.elsewhen(io.retValid) {
    io.redirect.valid := true.B
    io.redirect.pc := mepc
  }.elsewhen(io.csr.valid) {
    val old = csrRead(io.csr.addr)
    io.csr.readData := old

    val newVal = csrWrite(old, io.csr.writeData, io.csr.cmd)

    switch(io.csr.addr) {
      is("h300".U) { mstatus := newVal }
      is("h305".U) { mtvec := newVal }
      is("h340".U) { mscratch := newVal }
      is("h341".U) { mepc := newVal }
      is("h342".U) { mcause := newVal }
      is("h343".U) { mtval := newVal }
    }
  }
}
