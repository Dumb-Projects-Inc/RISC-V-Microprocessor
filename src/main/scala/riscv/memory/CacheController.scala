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
  // val RomStart = 0x00000000
  val RomEnd = 0x00000fff
  // val PeripheralsStart = 0x00001000
  val PeripheralsEnd = 0x0000ffff
  // val ProgramMemoryStart = 0x00010000
  // val ProgramMemoryEnd = 0x00700000 // External memory decides how much is available
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
        (addr <= MemoryMap.RomEnd.U) -> MemoryRegions.ROM,
        (addr <= MemoryMap.PeripheralsEnd.U) -> MemoryRegions.Peripherals
      )
    )

  val IDat = SyncReadMem(2048, UInt(32.W)) // 4KB data cache
  val DDat = SyncReadMem(2048, UInt(32.W)) // 4KB data cache

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

  // Issue reads in request
  val ramRd = IDat.read(
    io.instrPort.addr(11, 2),
    IReq.region === MemoryRegions.ProgramMemory
  )

  io.instrPort.stall := IResp.region === MemoryRegions.Peripherals

  when(IResp.valid) {
    io.instrPort.instr := MuxCase(
      0.U,
      Seq(
        (IResp.region === MemoryRegions.ROM) -> io.ROMIn,
        (IResp.region === MemoryRegions.ProgramMemory) -> ramRd
      )
    )
  }.otherwise {
    io.instrPort.instr := 0.U
  }

  // ******************************
  // Data Port
  // ******************************

  val DReq = Request(
    io.dataPort.enable,
    !io.dataPort.writeEn,
    getRegion(io.dataPort.addr)
  )
  val DResp = RegNext(
    Response(
      DReq.valid,
      !DReq.load,
      io.dataPort.memSize,
      DReq.region,
      io.dataPort.addr,
      io.dataPort.dataWrite
    ),
    0.U.asTypeOf(new Response)
  )

  // always get data if in Program Memory region
  val ramIndexReq = (io.dataPort.addr - 0x00010000.U)(11, 2)
  val ramData =
    DDat.read(
      ramIndexReq,
      DReq.valid && DReq.region === MemoryRegions.ProgramMemory
    )

  val busReq = DReq.region === MemoryRegions.Peripherals && DReq.valid
  when(busReq) {
    when(DReq.load) {
      io.bus.readRequest(io.dataPort.addr)
    }.otherwise {
      io.bus.writeRequest(io.dataPort.addr, io.dataPort.dataWrite)
    }
  }

  val lastRead = Reg(UInt(32.W))
  when(DResp.valid) {
    lastRead := ramData
    when(!DResp.write) {
      val data = MuxCase(
        0.U,
        Seq(
          (DResp.region === MemoryRegions.ROM) -> io.ROMIn,
          (DResp.region === MemoryRegions.Peripherals) -> io.bus.rdData,
          (DResp.region === MemoryRegions.ProgramMemory) -> ramData
        )
      )
      io.dataPort.dataRead := MuxCase(
        0.U,
        // This should really sign extend for byte and halfword loads
        Seq(
          (DResp.size === MemSize.Byte) -> 0.U(24.W) ## data(7, 0),
          (DResp.size === MemSize.HalfWord) -> 0.U(16.W) ## data(15, 0),
          (DResp.size === MemSize.Word) -> data
        )
      )
    }
  }.otherwise {
    io.dataPort.dataRead := 0.U
  }

  when(DResp.valid && DResp.write) {
    when(DResp.region === MemoryRegions.ProgramMemory) {
      // calculate the data based on memsize
      val offset = DResp.respAddr(1, 0)
      // put data into lastRead based on size and offset
      val data = Wire(UInt(32.W))
      data := MuxCase(
        lastRead,
        Seq(
          (DResp.size === MemSize.Byte) -> Mux1H(
            Seq(
              (offset === 0.U) -> DResp.data(7, 0) ## lastRead(
                23,
                0
              ),
              (offset === 1.U) -> lastRead(31, 24) ## DResp
                .data(7, 0) ## lastRead(15, 0),
              (offset === 2.U) -> lastRead(31, 16) ## DResp
                .data(7, 0) ## lastRead(7, 0),
              (offset === 3.U) -> lastRead(31, 8) ## DResp.data(7, 0)
            )
          ),
          (DResp.size === MemSize.HalfWord) -> Mux1H(
            Seq(
              (offset === 0.U) -> DResp.data(15, 0) ## lastRead(
                15,
                0
              ),
              (offset === 2.U) -> lastRead(31, 16) ## DResp
                .data(15, 0)
            )
          ),
          (DResp.size === MemSize.Word) -> DResp.data
        )
      )
      // calculate address by:
      val ramIndex = (DResp.respAddr - 0x00010000.U)(11, 2)
      DDat.write(ramIndex, data)
    }
  }

}
