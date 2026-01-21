package riscv.memory

import chisel3._
import chisel3.util._
import lib.Bus
import riscv.MemSize
import riscv.MemOp

// Static Memory mapping
// Following the https://riscv.org/blog/design-approaches-and-architectures-of-risc-v-socs/ embedded memory map
// 0x0001_0000 - 0x0070_0000  : Program Memory (RAM)
// 0x0000_1000 - 0x0000_FFFF : Peripherals 60kb
// 0x0000_0000 - 0x0000_0FFF : ROM 4kb

object MemoryRegions extends ChiselEnum {
  val ROM, Peripherals, ProgramMemory = Value
}

object MemoryMap {
  val romStart = 0x00000000L
  val romEnd = 0x00000fffL // 4kb
  val peripheralsStart = 0x00001000L
  val peripheralsEnd = 0x0000ffffL // 60kb
  val dataStart = 0x00010000L
  val dataEnd = 0x0070ffffL // ~7MB
}

object CacheSignals {
  class Request extends Bundle {
    val valid = Bool()
    val memOp = MemOp()
    val region = MemoryRegions()
    val size = MemSize()
    val data = UInt(32.W)
  }

  class Response extends Bundle {
    val valid = Bool()
    val memOp = MemOp()
    val size = MemSize()
    val region = MemoryRegions()
    val respAddr = UInt(32.W)
  }

  object Response {
    def apply(
        valid: Bool,
        memOp: MemOp.Type,
        size: MemSize.Type,
        region: MemoryRegions.Type,
        respAddr: UInt = 0.U
    ) = {
      val bundle = Wire(new Response())
      bundle.valid := valid
      bundle.memOp := memOp
      bundle.size := size
      bundle.region := region
      bundle.respAddr := respAddr
      bundle
    }
  }

  object Request {
    def apply(
        valid: Bool,
        memOp: MemOp.Type,
        region: MemoryRegions.Type,
        size: MemSize.Type,
        data: UInt
    ) = {
      val bundle = Wire(new Request())
      bundle.valid := valid
      bundle.memOp := memOp
      bundle.region := region
      bundle.size := size
      bundle.data := data

      bundle
    }
  }
}

/** CacheController using [[Cache]] and memory regions This cachecontroller
  * implements the (Modified) Harvard architecture by separating instruction and
  * data caches. Cache coherency is only ensured for I-cache if fence.i is
  * called (needs to be implemented).
  */
class CacheController(rom: Seq[UInt]) extends Module {
  import CacheSignals._
  val io = IO(new Bundle {
    val instrPort = Flipped(new riscv.instrPort())
    val dataPort = Flipped(new riscv.dataPort())
    val bus = Bus.RequestPort()
    // val flush = Input(Bool()) // fence.i instruction
  })

  io.bus.init()
  io.instrPort := DontCare
  io.dataPort := DontCare

  val ROM = VecInit(rom) // Simple ROM implementation for BIOS
  val ROM_MAX = log2Ceil(rom.length)

  // Check region of instruction fetch
  def getRegion(addr: UInt): MemoryRegions.Type =
    MuxCase(
      MemoryRegions.ProgramMemory,
      Seq(
        (addr >= MemoryMap.dataStart.U && addr <= MemoryMap.dataEnd.U) -> MemoryRegions.ProgramMemory,
        (addr >= MemoryMap.peripheralsStart.U && addr <= MemoryMap.peripheralsEnd.U) -> MemoryRegions.Peripherals,
        (addr >= MemoryMap.romStart.U && addr <= MemoryMap.romEnd.U) -> MemoryRegions.ROM
      )
    )

  // val IDat = SyncReadMem(2048, UInt(32.W)) // 4KB data cache
  val DDat = SyncReadMem(
    256000,
    Vec(4, UInt(8.W)),
    SyncReadMem.WriteFirst
  ) // 1mb data cache // vec to enable masks

  val IReq =
    Request(
      io.instrPort.enable,
      MemOp.Load,
      getRegion(io.instrPort.addr),
      MemSize.Word,
      0.U
    )
  val IResp = RegNext(
    Response(
      IReq.valid,
      MemOp.Load,
      MemSize.Word,
      getRegion(io.instrPort.addr),
      io.instrPort.addr
    ),
    0.U.asTypeOf(new Response)
  )

  val DReq = Request(
    io.dataPort.enable,
    io.dataPort.memOp,
    getRegion(io.dataPort.addr),
    io.dataPort.memSize,
    io.dataPort.dataWrite
  )
  val DResp = RegNext(
    Response(
      DReq.valid,
      io.dataPort.memOp,
      io.dataPort.memSize,
      getRegion(io.dataPort.addr),
      io.dataPort.addr
    ),
    0.U.asTypeOf(new Response)
  )

  // always fetch instructions from both idat and rom
  val iDatData = DDat.read(io.instrPort.addr(19, 2), IReq.valid)
  val iRomData = RegNext(ROM(io.instrPort.addr(ROM_MAX + 1, 2)), 0.U)

  when(IResp.valid) {
    io.instrPort.instr := MuxCase(
      0.U,
      Seq(
        (IResp.region === MemoryRegions.ProgramMemory) -> iDatData.asUInt,
        (IResp.region === MemoryRegions.ROM) -> iRomData
      )
    )
  }

  // always fetch data from  ddat
  val dDatData = DDat.read(
    io.dataPort.addr(19, 2),
    DReq.memOp === MemOp.Load || DReq.memOp === MemOp.LoadUnsigned
  )
  val dRomData = RegNext(ROM(io.dataPort.addr(ROM_MAX + 1, 2)), 0.U)

  when(DReq.valid) {
    // load request
    when(DReq.region === MemoryRegions.Peripherals) {
      // peripheral read through bus
      when(DReq.memOp === MemOp.Store) {
        // write to peripheral, this only supports word writes for now
        io.bus.writeRequest(
          io.dataPort.addr,
          io.dataPort.dataWrite
        )
      }.elsewhen(
        DReq.memOp === MemOp.Load || DReq.memOp === MemOp.LoadUnsigned
      ) {
        // read from peripheral
        io.bus.readRequest(io.dataPort.addr)
      }
    }.elsewhen(DReq.memOp === MemOp.Store) {
      // store to data cache only
      val offset = io.dataPort.addr(1, 0)
      val writeData = MuxCase(
        0.U,
        Seq(
          (DReq.size === MemSize.Byte) -> Fill(4, DReq.data(7, 0)),
          (DReq.size === MemSize.HalfWord) -> Fill(2, DReq.data(15, 0)),
          (DReq.size === MemSize.Word) -> DReq.data
        )
      )
      val mask = MuxCase(
        "b0000".U(4.W),
        Seq(
          (DReq.size === MemSize.Byte) -> (1.U(4.W) << offset),
          (DReq.size === MemSize.HalfWord) -> (3.U(4.W) << (offset(1) << 1)),
          (DReq.size === MemSize.Word) -> "b1111".U(4.W)
        )
      )
      DDat.write(
        io.dataPort.addr(19, 2),
        writeData.asTypeOf(Vec(4, UInt(8.W))),
        mask.asTypeOf(Vec(4, Bool()))
      )
    }
  }

  when(DResp.valid) {
    val data = MuxCase(
      0.U,
      Seq(
        (DResp.region === MemoryRegions.ProgramMemory) -> dDatData.asUInt,
        (DResp.region === MemoryRegions.Peripherals) -> io.bus.rdData,
        (DResp.region === MemoryRegions.ROM) -> dRomData
      )
    )
    when(DResp.memOp === MemOp.Load || DResp.memOp === MemOp.LoadUnsigned) {
      // load
      val offset = DResp.respAddr(1, 0)

// Narrow extracts
      val byteVal = (data >> (offset << 3))(7, 0)
      val halfVal = (data >> (offset(1) << 4))(15, 0)

// Extend to 32 based on op
      val extended = Wire(UInt(32.W))
      extended := MuxCase(
        data, // word load (already 32)
        Seq(
          (DResp.size === MemSize.Byte) -> Mux(
            DResp.memOp === MemOp.LoadUnsigned,
            Cat(0.U(24.W), byteVal), // zero-extend
            Cat(Fill(24, byteVal(7)), byteVal) // sign-extend
          ),
          (DResp.size === MemSize.HalfWord) -> Mux(
            DResp.memOp === MemOp.LoadUnsigned,
            Cat(0.U(16.W), halfVal), // zero-extend
            Cat(Fill(16, halfVal(15)), halfVal) // sign-extend
          )
        )
      )

      io.dataPort.dataRead := extended
    }
  }

}
