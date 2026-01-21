package riscv.memory

import com.carlosedp.riscvassembler.RISCVAssembler

object AssemblerCompat {

  private val NOP_HEX = "00000013"

  private val csrMap = Map(
    "mstatus" -> 0x300,
    "mtvec" -> 0x305,
    "mscratch" -> 0x340,
    "mepc" -> 0x341,
    "mcause" -> 0x342,
    "mtval" -> 0x343
  )

  private val regMap = Map(
    "x0" -> 0,
    "zero" -> 0,
    "x1" -> 1,
    "ra" -> 1,
    "x2" -> 2,
    "sp" -> 2,
    "x3" -> 3,
    "gp" -> 3,
    "x4" -> 4,
    "tp" -> 4,
    "x5" -> 5,
    "t0" -> 5,
    "x6" -> 6,
    "t1" -> 6,
    "x7" -> 7,
    "t2" -> 7,
    "x8" -> 8,
    "s0" -> 8,
    "fp" -> 8,
    "x9" -> 9,
    "s1" -> 9,
    "x10" -> 10,
    "a0" -> 10,
    "x11" -> 11,
    "a1" -> 11,
    "x12" -> 12,
    "a2" -> 12,
    "x13" -> 13,
    "a3" -> 13,
    "x14" -> 14,
    "a4" -> 14,
    "x15" -> 15,
    "a5" -> 15,
    "x16" -> 16,
    "a6" -> 16,
    "x17" -> 17,
    "a7" -> 17,
    "x28" -> 28,
    "t3" -> 28,
    "x29" -> 29,
    "t4" -> 29,
    "x30" -> 30,
    "t5" -> 30,
    "x31" -> 31,
    "t6" -> 31
  )

  private def stripComments(line: String): String = {
    val noHash = line.split("#")(0)
    val noDbl = noHash.split("//")(0)
    // your existing code also uses `'` as comment in bootloader strings
    val noTick = noDbl.split("'")(0)
    noTick.trim
  }

  private def parseImm(s: String): Int = {
    val t = s.trim.toLowerCase
    if (t.startsWith("0x")) Integer.parseUnsignedInt(t.drop(2), 16)
    else t.toInt
  }

  private def parseReg(s: String): Int =
    regMap.getOrElse(
      s.trim.toLowerCase,
      throw new Exception(s"Unknown reg: $s")
    )

  private def parseCsr(s: String): Int = {
    val t = s.trim.toLowerCase
    csrMap.getOrElse(t, parseImm(t))
  }

  private def encI(opcode: Int, funct3: Int, rd: Int, rs1: Int, imm: Int): Int =
    ((imm & 0xfff) << 20) | (rs1 << 15) | (funct3 << 12) | (rd << 7) | (opcode & 0x7f)

  private def toHexWord(w: Int): String = f"$w%08X"

  private def encJ(opcode: Int, rd: Int, imm: Int): Int = {
    // imm is byte offset (signed)
    val off = imm
    val bit20 = (off >> 20) & 0x1
    val bits10_1 = (off >> 1) & 0x3ff
    val bit11 = (off >> 11) & 0x1
    val bits19_12 = (off >> 12) & 0xff
    (bit20 << 31) |
      (bits19_12 << 12) |
      (bit11 << 20) |
      (bits10_1 << 21) |
      (rd << 7) |
      (opcode & 0x7f)
  }

  private def encB(
      opcode: Int,
      funct3: Int,
      rs1: Int,
      rs2: Int,
      imm: Int
  ): Int = {
    // imm is byte offset (signed), must be multiple of 2
    val off = imm
    val bit12 = (off >> 12) & 0x1
    val bits10_5 = (off >> 5) & 0x3f
    val bits4_1 = (off >> 1) & 0xf
    val bit11 = (off >> 11) & 0x1
    (bit12 << 31) |
      (bits10_5 << 25) |
      (rs2 << 20) |
      (rs1 << 15) |
      (funct3 << 12) |
      (bits4_1 << 8) |
      (bit11 << 7) |
      (opcode & 0x7f)
  }

  private def isLabel(
      s: String,
      labels: scala.collection.mutable.Map[String, Int]
  ): Boolean =
    labels.contains(s)

  private def resolvePcRelativeOffset(
      token: String,
      pc: Int,
      labels: scala.collection.mutable.Map[String, Int]
  ): Int = {
    if (isLabel(token, labels))
      labels(token) - pc // label -> absolute addr, convert to offset
    else parseImm(token) // numeric -> already an offset in bytes
  }

  /** Assemble using external RISCVAssembler when possible, but patch in support
    * for:
    *   - .org (by inserting nops)
    *   - csrw/csrr (via csrrw/csrrs encodings)
    *   - mret (fixed encoding)
    *
    * Returns: hex text, one 32-bit word per line (no 0x prefix), matching
    * Bootloader.assemble expectations.
    */
  def fromString(program: String): String = {
    val lines0 =
      program.split("\n").toList.map(stripComments).filter(_.nonEmpty)

    // First pass: labels -> address (pc in bytes)
    var pc = 0
    val labels = scala.collection.mutable.Map.empty[String, Int]
    lines0.foreach { line =>
      val l = line.trim
      if (l.toLowerCase.startsWith(".org")) {
        val parts = l.split("\\s+")
        pc = parseImm(parts(1))
      } else if (l.contains(":")) {
        val parts = l.split(":", 2)
        labels(parts(0).trim) = pc
        val rest = parts.lift(1).map(_.trim).getOrElse("")
        if (rest.nonEmpty) pc += 4
      } else {
        pc += 4
      }
    }

    // Second pass: emit final hex lines
    val out = new StringBuilder
    pc = 0

    def emit(wordHex: String): Unit = {
      out.append(wordHex).append("\n"); pc += 4
    }
    def emitNopsUntil(targetPc: Int): Unit = while (pc < targetPc) emit(NOP_HEX)

    lines0.foreach { line0 =>
      var line = line0.trim

      // label split
      if (line.contains(":")) {
        val parts = line.split(":", 2)
        line = parts.lift(1).map(_.trim).getOrElse("")
        if (line.isEmpty) { /* label only */ }
      }

      if (line.nonEmpty) {
        val lower = line.toLowerCase
        if (lower.startsWith(".org")) {
          val parts = lower.split("\\s+")
          val target = parseImm(parts(1))
          emitNopsUntil(target)
          pc = target
        } else {
          val toks = lower.split("[\\s,\\(\\)]+").filter(_.nonEmpty)

          toks(0) match {
            case "mret" =>
              emit(toHexWord(0x30200073))

            case "ecall" =>
              // external assembler already supports it, but safe to hardcode:
              emit(toHexWord(0x00000073))

            case "csrw" =>
              // csrw csr, rs1 == csrrw x0, csr, rs1
              // encoding: opcode=0x73 funct3=001 rd=0 rs1=reg imm=csr
              val csr = parseCsr(toks(1))
              val rs1 = parseReg(toks(2))
              emit(toHexWord(encI(0x73, 0x1, 0, rs1, csr)))

            case "csrr" =>
              // csrr rd, csr == csrrs rd, csr, x0
              // encoding: opcode=0x73 funct3=010 rd=reg rs1=0 imm=csr
              val rd = parseReg(toks(1))
              val csr = parseCsr(toks(2))
              emit(toHexWord(encI(0x73, 0x2, rd, 0, csr)))

            case "csrrw" =>
              // csrrw rd, csr, rs1
              val rd = parseReg(toks(1))
              val csr = parseCsr(toks(2))
              val rs1 = parseReg(toks(3))
              emit(toHexWord(encI(0x73, 0x1, rd, rs1, csr))) // funct3=001

            case "csrrs" =>
              // csrrs rd, csr, rs1
              val rd = parseReg(toks(1))
              val csr = parseCsr(toks(2))
              val rs1 = parseReg(toks(3))
              emit(toHexWord(encI(0x73, 0x2, rd, rs1, csr))) // funct3=010

            case "csrrc" =>
              // csrrc rd, csr, rs1
              val rd = parseReg(toks(1))
              val csr = parseCsr(toks(2))
              val rs1 = parseReg(toks(3))
              emit(toHexWord(encI(0x73, 0x3, rd, rs1, csr))) // funct3=011

            case "csrrwi" =>
              val rd = parseReg(toks(1))
              val csr = parseCsr(toks(2))
              val zimm = parseImm(toks(3)) & 0x1f
              emit(
                toHexWord(encI(0x73, 0x5, rd, zimm, csr))
              ) // funct3=101, rs1=zimm

            case "csrrsi" =>
              val rd = parseReg(toks(1))
              val csr = parseCsr(toks(2))
              val zimm = parseImm(toks(3)) & 0x1f
              emit(toHexWord(encI(0x73, 0x6, rd, zimm, csr))) // funct3=110

            case "csrrci" =>
              val rd = parseReg(toks(1))
              val csr = parseCsr(toks(2))
              val zimm = parseImm(toks(3)) & 0x1f
              emit(toHexWord(encI(0x73, 0x7, rd, zimm, csr))) // funct3=111

            case "beq" | "bne" | "blt" | "bge" | "bltu" | "bgeu" =>
              val rs1 = parseReg(toks(1))
              val rs2 = parseReg(toks(2))
              val off = resolvePcRelativeOffset(toks(3), pc, labels)

              val funct3 = toks(0) match {
                case "beq"  => 0x0
                case "bne"  => 0x1
                case "blt"  => 0x4
                case "bge"  => 0x5
                case "bltu" => 0x6
                case "bgeu" => 0x7
              }

              emit(toHexWord(encB(0x63, funct3, rs1, rs2, off)))

            case "jal" =>
              // jal rd, target
              // target can be a label (pc-relative) or numeric offset (already pc-relative)
              val rd = parseReg(toks(1))
              val off = resolvePcRelativeOffset(toks(2), pc, labels)
              emit(toHexWord(encJ(0x6f, rd, off)))

            case "jalr" =>
              // Support BOTH:
              //   jalr rd, imm(rs1)   -> toks: jalr rd imm rs1
              //   jalr rd, rs1, imm   -> toks: jalr rd rs1 imm
              val rd = parseReg(toks(1))

              val (rs1, imm) =
                if (line.contains("(") && toks.length >= 4) {
                  // jalr rd, imm(rs1)
                  (parseReg(toks(3)), parseImm(toks(2)))
                } else {
                  // jalr rd, rs1, imm
                  (parseReg(toks(2)), parseImm(toks(3)))
                }

              // JALR encoding: opcode=0x67 funct3=000
              emit(toHexWord(encI(0x67, 0x0, rd, rs1, imm)))

            case other =>
              // For everything else, delegate to external assembler (single-line)
              val hex = RISCVAssembler
                .fromString(line)
                .split("\\R")
                .map(_.trim)
                .filter(_.nonEmpty)

              if (hex.isEmpty) {
                // If external assembler can't parse it, fail loudly instead of silently emitting 0s
                throw new Exception(
                  s"Unsupported instruction for assembler: '$line'"
                )
              } else {
                // external outputs like "XXXXXXXX" or maybe "0xXXXXXXXX"; your Bootloader expects no "0x"
                val w = hex.head.stripPrefix("0x").stripPrefix("0X")
                emit(w)
              }
          }
        }
      }
    }

    out.toString()
  }
}
