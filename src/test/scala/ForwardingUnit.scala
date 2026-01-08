package riscv

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import chisel3._
import chisel3.simulator.scalatest.ChiselSim

class ForwardingUnitSpec extends AnyFunSpec with ChiselSim with Matchers {
  describe("ForwardingUnit") {

    it("If the instruction does not use any forwarded data, no forwarding occurs") {
      simulate(new ForwardingUnit()) { dut =>
        dut.io.dec.rs1.poke(1.U)
        dut.io.dec.rs2.poke(2.U)
        dut.io.dec.uses.aluRs1.poke(false.B)
        dut.io.dec.uses.aluRs2.poke(false.B)
        dut.io.dec.uses.bjRs1.poke(false.B)
        dut.io.dec.uses.bjRs2.poke(false.B)
        dut.io.dec.uses.idRs1.poke(false.B)
        dut.io.dec.uses.idRs2.poke(false.B)
        dut.io.dec.uses.storeDataRs2.poke(false.B)

        dut.io.exe.rd.poke(0.U); 
        dut.io.exe.regWrite.poke(false.B); 
        dut.io.exe.isLoad.poke(false.B)
        dut.io.mem.rd.poke(0.U); 
        dut.io.mem.regWrite.poke(false.B); 
        dut.io.mem.isLoad.poke(false.B)
        dut.io.wb.rd.poke(0.U);  
        dut.io.wb.regWrite.poke(false.B);  
        dut.io.wb.isLoad.poke(false.B)

        dut.clock.step()

        dut.io.sel.data1ALUSel.expect(ForwardingUnit.Sel.none)
        dut.io.sel.data2ALUSel.expect(ForwardingUnit.Sel.none)
        dut.io.sel.data1BJSel.expect(ForwardingUnit.Sel.none)
        dut.io.sel.data2BJSel.expect(ForwardingUnit.Sel.none)
        dut.io.sel.dataMemSel.expect(ForwardingUnit.Sel.none)
        dut.io.sel.data1IDSel.expect(false.B)
        dut.io.sel.data2IDSel.expect(false.B)
      }
    }

    it("If ALU needs rs1 forwarded and the EXE stages just wrote to it, forward from EXE") {
      simulate(new ForwardingUnit()) { dut =>
        dut.io.dec.rs1.poke(5.U)
        dut.io.dec.rs2.poke(0.U)
        dut.io.dec.uses.aluRs1.poke(true.B)
        dut.io.dec.uses.aluRs2.poke(false.B)
        dut.io.dec.uses.bjRs1.poke(false.B)
        dut.io.dec.uses.bjRs2.poke(false.B)
        dut.io.dec.uses.idRs1.poke(false.B)
        dut.io.dec.uses.idRs2.poke(false.B)
        dut.io.dec.uses.storeDataRs2.poke(false.B)

        dut.io.exe.rd.poke(5.U); 
        dut.io.exe.regWrite.poke(true.B); 
        dut.io.exe.isLoad.poke(false.B)
        dut.io.mem.rd.poke(5.U); 
        dut.io.mem.regWrite.poke(true.B); 
        dut.io.mem.isLoad.poke(false.B)
        dut.io.wb.rd.poke(5.U);  
        dut.io.wb.regWrite.poke(true.B);  
        dut.io.wb.isLoad.poke(false.B)

        dut.clock.step()

        // priority: EXE > MEM > WB
        dut.io.sel.data1ALUSel.expect(ForwardingUnit.Sel.exe)
      }
    }

    it("If EXE is a load then forward from MEM instead") {
      simulate(new ForwardingUnit()) { dut =>
        dut.io.dec.rs1.poke(10.U)
        dut.io.dec.rs2.poke(0.U)
        dut.io.dec.uses.aluRs1.poke(true.B)
        dut.io.dec.uses.aluRs2.poke(false.B)
        dut.io.dec.uses.bjRs1.poke(false.B)
        dut.io.dec.uses.bjRs2.poke(false.B)
        dut.io.dec.uses.idRs1.poke(false.B)
        dut.io.dec.uses.idRs2.poke(false.B)
        dut.io.dec.uses.storeDataRs2.poke(false.B)

        // EXE matches but is a load -> should NOT pick EXE
        dut.io.exe.rd.poke(10.U); 
        dut.io.exe.regWrite.poke(true.B); 
        dut.io.exe.isLoad.poke(true.B)

        // MEM matches -> should pick MEM
        dut.io.mem.rd.poke(10.U); 
        dut.io.mem.regWrite.poke(true.B); 
        dut.io.mem.isLoad.poke(false.B)

        // WB also matches, but lower priority
        dut.io.wb.rd.poke(10.U);  
        dut.io.wb.regWrite.poke(true.B);  
        dut.io.wb.isLoad.poke(false.B)

        dut.clock.step()

        dut.io.sel.data1ALUSel.expect(ForwardingUnit.Sel.mem)
      }
    }

    it("ignores forwarding when destination register is x0") {
      simulate(new ForwardingUnit()) { dut =>
        dut.io.dec.rs1.poke(0.U)
        dut.io.dec.rs2.poke(8.U)

        dut.io.dec.uses.aluRs1.poke(false.B)
        dut.io.dec.uses.aluRs2.poke(true.B)
        dut.io.dec.uses.bjRs1.poke(false.B)
        dut.io.dec.uses.bjRs2.poke(false.B)
        dut.io.dec.uses.idRs1.poke(false.B)
        dut.io.dec.uses.idRs2.poke(false.B)
        dut.io.dec.uses.storeDataRs2.poke(false.B)

        // EXE "matches" rs2 only if rd==rs2. Set rd=0 (should be ignored even if regWrite)
        dut.io.exe.rd.poke(0.U); 
        dut.io.exe.regWrite.poke(true.B); 
        dut.io.exe.isLoad.poke(false.B)

        // WB matches properly
        dut.io.mem.rd.poke(0.U); 
        dut.io.mem.regWrite.poke(false.B); 
        dut.io.mem.isLoad.poke(false.B)
        dut.io.wb.rd.poke(8.U);  
        dut.io.wb.regWrite.poke(true.B);  
        dut.io.wb.isLoad.poke(false.B)
        dut.clock.step()

        dut.io.sel.data2ALUSel.expect(ForwardingUnit.Sel.wb)
      }
    }

    it("Store data forwarding chooses the newest match (EXE > MEM > WB), skipping EXE if it is a load") {
      simulate(new ForwardingUnit()) { dut =>
        dut.io.dec.rs1.poke(1.U)
        dut.io.dec.rs2.poke(7.U)

        dut.io.dec.uses.aluRs1.poke(false.B)
        dut.io.dec.uses.aluRs2.poke(false.B)
        dut.io.dec.uses.bjRs1.poke(false.B)
        dut.io.dec.uses.bjRs2.poke(false.B)
        dut.io.dec.uses.idRs1.poke(false.B)
        dut.io.dec.uses.idRs2.poke(false.B)
        dut.io.dec.uses.storeDataRs2.poke(true.B)

        // EXE match but is load -> should not use EXE
        dut.io.exe.rd.poke(7.U); 
        dut.io.exe.regWrite.poke(true.B); 
        dut.io.exe.isLoad.poke(true.B)

        // MEM match -> should use MEM
        dut.io.mem.rd.poke(7.U); 
        dut.io.mem.regWrite.poke(true.B); 
        dut.io.mem.isLoad.poke(false.B)

        // WB match -> lower priority
        dut.io.wb.rd.poke(7.U); 
        dut.io.wb.regWrite.poke(true.B); 
        dut.io.wb.isLoad.poke(false.B)

        dut.clock.step()

        dut.io.sel.dataMemSel.expect(ForwardingUnit.Sel.mem)
      }
    }

    it("ID bypass triggers only from WB (not EXE/MEM)") {
      simulate(new ForwardingUnit()) { dut =>
        dut.io.dec.rs1.poke(3.U)
        dut.io.dec.rs2.poke(0.U)

        dut.io.dec.uses.aluRs1.poke(false.B)
        dut.io.dec.uses.aluRs2.poke(false.B)
        dut.io.dec.uses.bjRs1.poke(false.B)
        dut.io.dec.uses.bjRs2.poke(false.B)
        dut.io.dec.uses.idRs1.poke(true.B)
        dut.io.dec.uses.idRs2.poke(false.B)
        dut.io.dec.uses.storeDataRs2.poke(false.B)

        // MEM matches, but ID bypass should ignore it
        dut.io.exe.rd.poke(0.U); 
        dut.io.exe.regWrite.poke(false.B); 
        dut.io.exe.isLoad.poke(false.B)
        dut.io.mem.rd.poke(3.U); 
        dut.io.mem.regWrite.poke(true.B);  
        dut.io.mem.isLoad.poke(false.B)
        dut.io.wb.rd.poke(0.U);  
        dut.io.wb.regWrite.poke(false.B);  
        dut.io.wb.isLoad.poke(false.B)

        dut.clock.step()
        dut.io.sel.data1IDSel.expect(false.B)

        // Now make WB match -> should assert
        dut.io.wb.rd.poke(3.U)
        dut.io.wb.regWrite.poke(true.B)
        dut.clock.step()

        dut.io.sel.data1IDSel.expect(true.B)
      }
    }

    it("forwards branch operand 2 when requested") {
      simulate(new ForwardingUnit()) { dut =>
        dut.io.dec.rs1.poke(0.U)
        dut.io.dec.rs2.poke(12.U)

        dut.io.dec.uses.aluRs1.poke(false.B)
        dut.io.dec.uses.aluRs2.poke(false.B)
        dut.io.dec.uses.bjRs1.poke(false.B)
        dut.io.dec.uses.bjRs2.poke(true.B)
        dut.io.dec.uses.idRs1.poke(false.B)
        dut.io.dec.uses.idRs2.poke(false.B)
        dut.io.dec.uses.storeDataRs2.poke(false.B)

        // MEM matches rs2 -> expect MEM select
        dut.io.exe.rd.poke(0.U);  
        dut.io.exe.regWrite.poke(false.B); 
        dut.io.exe.isLoad.poke(false.B)
        dut.io.mem.rd.poke(12.U); 
        dut.io.mem.regWrite.poke(true.B);  
        dut.io.mem.isLoad.poke(false.B)
        dut.io.wb.rd.poke(12.U);  
        dut.io.wb.regWrite.poke(true.B);   
        dut.io.wb.isLoad.poke(false.B)

        dut.clock.step()
        dut.io.sel.data2BJSel.expect(ForwardingUnit.Sel.mem)
      }
    }
  }
}
