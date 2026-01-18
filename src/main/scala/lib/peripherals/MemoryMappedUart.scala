// RX and TX modules adapted from https://github.com/freechipsproject/ip-contributions/blob/master/src/main/scala/chisel/lib/uart/Uart.scala

package lib.peripherals

import chisel3._
import chisel3.util._

object UUtils {
  def BitCount(baudRate: Int, freq: Int): Int = {
    ((freq + baudRate / 2) / baudRate - 1).toInt
  }
  def StartRxCount(baudRate: Int, freq: Int): Int = {
    ((3 * freq / 2 + baudRate / 2) / baudRate - 2).toInt
  }

}

class Rx(freq: Int, baud: Int) extends Module {
  val io = IO(new Bundle {
    val rxData = Input(Bool())
    val UartIO = DecoupledIO(UInt(8.W))
  })

  val rxReg = RegNext(RegNext(io.rxData, false.B), false.B)
  val fallingEdge = rxReg && !io.rxData

  val shiftReg = RegInit(0.U(8.W))
  val (cntReg, _) = Counter(
    0 to UUtils.BitCount(baud, freq) by 1
  ) // have some idle time before listening
  val bitsReg = RegInit(0.U(4.W))
  val valReg = RegInit(false.B)

  when(cntReg === 0.U) {
    when(bitsReg =/= 0.U) {
      cntReg := UUtils.BitCount(baud, freq).U
      shiftReg := Cat(rxReg, shiftReg >> 1)
      bitsReg := bitsReg - 1.U
      // the last shifted in
      when(bitsReg === 1.U) {
        valReg := true.B
      }
    }.elsewhen(fallingEdge) { // wait 1.5 bits after falling edge of start
      cntReg := UUtils.StartRxCount(baud, freq).U
      bitsReg := 8.U
    }
  }

  when(valReg && io.UartIO.ready) {
    valReg := false.B
  }

  io.UartIO.bits := shiftReg
  io.UartIO.valid := valReg

}

class Tx(freq: Int, baud: Int) extends Module {
  val io = IO(new Bundle {
    val txData = Output(Bool())
    val UartIO = Flipped(DecoupledIO(UInt(8.W)))
  })

  val STOPBITS = 2
  val STARTBITS = 1
  // overhead for each byte = start + stop bits
  val TOTALBITS = 8 + STOPBITS + STARTBITS

  val (counterValue, counterWrap) = Counter(
    0 until UUtils.BitCount(baud, freq) by 1
  )
  val shiftReg = RegInit("b11111111111".U(11.W)) // start + data + stop bits
  val bitsReg = RegInit(0.U(4.W))

  io.UartIO.ready := (counterValue === 0.U) && (bitsReg === 0.U)
  io.txData := shiftReg(0)

  when(counterValue === 0.U) {
    when(bitsReg =/= 0.U) {
      // shift out data
      shiftReg := 1.U(1.W) ## shiftReg(TOTALBITS - 1, 1)
      bitsReg := bitsReg - 1.U
    }.otherwise {
      when(io.UartIO.valid) {
        // load new data
        shiftReg := 3.U(2.W) ## io.UartIO.bits ## 0.U(1.W)
        bitsReg := 11.U(4.W)
      }.otherwise {
        shiftReg := "b11111111111".U(11.W) // idle state
      }
    }
  }

}

/** Memory-mapped I/O UART peripheral. This implementation is two-ported to
  * allow simultaneous access from instruction and data buses. Although the UART
  * only has one connection, this simplifies cache controlling a lot.
  *
  * Memory layout: 0x00: Data register (read/write) 0x04: Status register (read
  * only)
  *
  * Status register bits: Bit 0: TX Ready (1 = ready to accept new data) Bit 1:
  * RX Valid (1 = data available to read)
  *
  * @param Freq
  * @param BaudRate
  * @param baseAddr
  */
class MMIOUart(Freq: Int, BaudRate: Int, baseAddr: BigInt = 0x10000000)
    extends Module {
  val io = IO(new Bundle {
    val dbus = lib.Bus.RespondPort()
    val rx = Input(Bool())
    val tx = Output(Bool())
  })

  io.dbus.init()

  // actual logic for Uart
  val txMod = Module(new Tx(Freq, BaudRate))
  val rxMod = Module(new Rx(Freq, BaudRate))

  // Uart is slow, so we just buffer everything
  val txBuffer = Module(new Queue(UInt(8.W), 32))
  val rxBuffer = Module(new Queue(UInt(8.W), 32))

  txMod.io.UartIO <> txBuffer.io.deq
  rxBuffer.io.enq <> rxMod.io.UartIO

  val dataRead = RegNext(io.dbus.hasReadRequestAt(baseAddr.U), false.B)
  val statusRead = RegNext(io.dbus.hasReadRequestAt((baseAddr + 4).U), false.B)
  val dataWrite = io.dbus.hasWriteRequestAt(baseAddr.U)

  txBuffer.io.enq.valid := dataWrite
  txBuffer.io.enq.bits := io.dbus.wrData(7, 0)

  rxBuffer.io.deq.ready := dataRead

  io.tx := txMod.io.txData
  rxMod.io.rxData := io.rx

  io.dbus.rdData := Mux(
    dataRead,
    rxBuffer.io.deq.bits,
    0.U(30.W) ## rxBuffer.io.deq.valid ## txBuffer.io.enq.ready
  )
  io.dbus.stall := false.B
  io.dbus.rdValid := false.B

}
