package riscv.memory

import chisel3._
import chisel3.util._
import lib.Bus
import riscv.MemSize
// Static Memory mapping
// Following the https://riscv.org/blog/design-approaches-and-architectures-of-risc-v-socs/ embedded memory map
// 0x0001_0000 - 0x0070_0000  : Program Memory (RAM)
// 0x0000_1000 - 0x0000_FFFF : Peripherals 60kb
// 0x0000_0000 - 0x0000_0FFF : ROM 4kb

object MemoryRegions extends ChiselEnum {
  val ROM, Peripherals, ProgramMemory = Value
}

object MemoryMap {
  val dataStart = 0x00000000L
  val dataEnd = 0x00000fffL // 4kb
  // Unmapped data region 0x00002000 - 0x00000FFF
  val peripheralsStart = 0x00001000L
  val peripheralsEnd = 0x0000ffffL // 60kb
  val romStart = 0xfff10000L
  val romEnd = 0xfff1ffffL // 4kb
}

class Pending extends Bundle {
  val pending = Bool()
  val region = MemoryRegions()
  val addr = UInt(32.W)
  val isBus = Bool()
}
object CacheSignals {
  class Request extends Bundle {
    val valid = Bool()
    val load = Bool()
    val region = MemoryRegions()
  }

  class Response extends Bundle {
    val valid = Bool()
    val write = Bool()
    val size = MemSize()
    val region = MemoryRegions()
    val respAddr = UInt(32.W)
    val data = UInt(32.W)
  }

  object Response {
    def apply(
        valid: Bool,
        write: Bool,
        size: MemSize.Type,
        region: MemoryRegions.Type,
        respAddr: UInt,
        data: UInt
    ) = {
      val bundle = Wire(new Response())
      bundle.valid := valid
      bundle.write := write
      bundle.size := size
      bundle.region := region
      bundle.respAddr := respAddr
      bundle.data := data
      bundle
    }
  }

  object Request {
    def apply(valid: Bool, load: Bool, region: MemoryRegions.Type) = {
      val bundle = Wire(new Request())
      bundle.valid := valid
      bundle.load := load
      bundle.region := region
      bundle
    }
  }
}

/** CacheController using [[Cache]] and memory regions This cachecontroller
  * implements the (Modified) Harvard architecture by separating instruction and
  * data caches. Cache coherency is only ensured for I-cache if fence.i is
  * called (needs to be implemented).
  */
class CacheController() extends Module {
  import CacheSignals._
  val io = IO(new Bundle {
    val instrPort = Flipped(new riscv.instrPort())
    val ROMIn = Input(UInt(32.W))
    val dataPort = Flipped(new riscv.dataPort())
    val bus = Bus.RequestPort()
    // val flush = Input(Bool()) // fence.i instruction
  })

  io.bus.init()
  io.instrPort := DontCare
  io.dataPort := DontCare

  // Check region of instruction fetch
  def getRegion(addr: UInt): MemoryRegions.Type =
    MuxCase(
      MemoryRegions.ProgramMemory,
      Seq(
        (addr <= (MemoryMap.dataEnd - 1).U) -> MemoryRegions.ProgramMemory,
        (addr >= MemoryMap.peripheralsStart.U && addr <= MemoryMap.peripheralsEnd.U) -> MemoryRegions.Peripherals,
        (addr >= MemoryMap.romStart.U && addr <= MemoryMap.romEnd.U) -> MemoryRegions.ROM
      )
    )

  // val IDat = SyncReadMem(2048, UInt(32.W)) // 4KB data cache
  val DDat = SyncReadMem(
    4096,
    Vec(4, UInt(8.W))
  ) // 4KB data cache // vec to enable masks

  val IReq = Request(io.instrPort.enable, true.B, getRegion(io.instrPort.addr))
  val IResp = RegNext(
    Response(
      IReq.valid,
      false.B,
      MemSize.Word,
      IReq.region,
      io.instrPort.addr,
      0.U
    ),
    0.U.asTypeOf(new Response)
  )

  val DReq = Request(
    io.dataPort.enable,
    !io.dataPort.writeEn,
    getRegion(io.dataPort.addr)
  )
  val DResp = RegNext(
    Response(
      DReq.valid,
      io.dataPort.writeEn,
      io.dataPort.memSize,
      DReq.region,
      io.dataPort.addr,
      io.dataPort.dataWrite
    ),
    0.U.asTypeOf(new Response)
  )

  // always fetch instructions from both idat and rom
  val iDatData = DDat.read(io.instrPort.addr(13, 2), IReq.valid)
  val iRomData = io.ROMIn

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
  val dDatData = DDat.read(io.dataPort.addr(13, 2), DReq.valid)
  when(DReq.valid) {
    // load request
    when(DReq.region === MemoryRegions.Peripherals) {
      // peripheral read through bus
      when(DReq.load) {
        // read from peripheral
        io.bus.readRequest(io.dataPort.addr)
      }.otherwise {
        // write to peripheral, this only supports word writes for now
        io.bus.writeRequest(
          io.dataPort.addr,
          io.dataPort.dataWrite
        )
      }
    }
  }

  when(DResp.valid) {
    val data = MuxCase(
      0.U,
      Seq(
        (DResp.region === MemoryRegions.ProgramMemory) -> dDatData.asUInt,
        (DResp.region === MemoryRegions.Peripherals) -> io.bus.rdData
      )
    )
    when(!DResp.write) {
      // load
      val offset = DResp.respAddr(1, 0)
      io.dataPort.dataRead := MuxCase(
        0.U,
        Seq(
          (DResp.size === MemSize.Byte) -> (data.asUInt >> (offset << 3))(7, 0),
          (DResp.size === MemSize.HalfWord) -> (data.asUInt >> (offset(
            1
          ) << 4))(15, 0),
          (DResp.size === MemSize.Word) -> data.asUInt
        )
      )
    }.elsewhen(DResp.region === MemoryRegions.ProgramMemory && DResp.write) {
      // store to data cache only
      val offset = DResp.respAddr(1, 0)
      val writeData = DResp.data
      val mask = MuxCase(
        "b0000".U(4.W),
        Seq(
          (DResp.size === MemSize.Byte) -> (1.U(4.W) << offset),
          (DResp.size === MemSize.HalfWord) -> (3.U(4.W) << (offset(1) << 1)),
          (DResp.size === MemSize.Word) -> "b1111".U(4.W)
        )
      )
      DDat.write(
        DResp.respAddr(12, 2),
        writeData.asTypeOf(Vec(4, UInt(8.W))),
        mask.asTypeOf(Vec(4, Bool()))
      )

    }
  }

}
