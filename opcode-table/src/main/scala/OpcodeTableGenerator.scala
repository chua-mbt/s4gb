import org.akaii.s4gb.emulator.cpu.instructions.{Instruction, OpCode}
import spire.math.UByte

import java.io.PrintWriter
import scala.util.Try

object OpcodeTableGenerator {

  def main(args: Array[String]): Unit = {
    val outputDir = args.headOption.getOrElse("docs")
    val outputPath = s"$outputDir/opcode-table.html"
    val file = java.io.File(outputPath)
    file.getParentFile.mkdirs()
    val pw = PrintWriter(file)
    pw.write(html)
    pw.close()
    println(s"Generated opcode table: $outputPath")
  }

  private def html: String = {
    val baseGrid = generateGrid(None)
    val cbGrid = generateGrid(Some(OpCode.PREFIXED))

    val headerCols = (0 until 16).map(i => s"+${i.toHexString.toUpperCase}").mkString("<th>", "</th><th>", "</th>")

    s"""<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>s4gb Opcode Table</title>
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body { font-family: monospace; padding: 10px; overflow-x: auto; background: #1a1a2e; }
    h1 { margin-bottom: 10px; font-size: 18px; color: #eee; }
    h2 { margin: 20px 0 10px; font-size: 14px; color: #ccc; }
    table { border-collapse: collapse; width: 100%; table-layout: fixed; }
    th, td { border: 1px solid #444; padding: 0; }
    td { width: calc(100% / 17); min-width: 70px; height: 50px; }
    thead th:not(:first-child) { background: #3d3d5c; color: #ccc; font-weight: normal; font-size: 11px; height: 20px; }
    thead th:first-child { width: 40px; min-width: 40px; background: #3d3d5c; color: #ccc; }
    tbody th { width: 40px; min-width: 40px; background: #3d3d5c; color: #ccc; font-weight: normal; font-size: 11px; }
    td.instr { background: #2a2a40; }
    td.hole { background: #1e1e2a; }
    td.prefix { background: #3d3d5c; }
    .cell-content { display: flex; flex-direction: column; height: 100%; justify-content: center; }
    .instr-name { text-align: center; font-weight: bold; font-size: 10px; padding: 2px; color: #b8b8d0; }
    .meta { display: flex; justify-content: space-between; font-size: 8px; color: #b8b8d0; padding: 0 4px; }
  </style>
</head>
<body>
  <h1>s4gb Opcode Table</h1>
  <h2>Base</h2>
  <table>
    <thead>
      <tr>
        <th></th>
        $headerCols
      </tr>
    </thead>
    <tbody>
      ${renderGrid(baseGrid)}
    </tbody>
  </table>
  <h2>0xCB Extensions</h2>
  <table>
    <thead>
      <tr>
        <th></th>
        $headerCols
      </tr>
    </thead>
    <tbody>
      ${renderGrid(cbGrid)}
    </tbody>
  </table>
</body>
</html>"""
  }

  private def generateGrid(prefix: Option[UByte]): Array[Array[String]] = {
    val grid = Array.ofDim[String](16, 16)

    for {
      row <- 0 until 16
      col <- 0 until 16
    } do {
      val opcode = UByte((row << 4) | col)
      val input = prefix match
        case Some(p) => Array(p, opcode)
        case None => Array(opcode)
      
      val cell = (prefix, opcode) match
        case (None, op) if op == OpCode.PREFIXED => 
          """<td class="prefix"><div class="cell-content"><span class="instr-name">EXTENSIONS<br>PREFIX</span></div></td>"""
        case _ =>
          Try(Instruction.decode(input))
            .map(instr => renderCell(opcode, instr))
            .getOrElse(s"""<td class="hole"></td>""")
      
      grid(row)(col) = cell
    }

    grid
  }

  private def renderCell(opcode: UByte, instruction: Instruction): String = {
    val name = instruction.productPrefix.replace("_", " ")
    val bytes = instruction.bytes
    val cycles = instruction.cycles match
      case Instruction.MCycle.Fixed(c) => c.toString
      case Instruction.MCycle.Varying(r) => s"${r.min}-${r.max}"
      case Instruction.MCycle.Undefined => "?"
    s"""<td class="instr">
        <div class="cell-content">
          <span class="instr-name">$name</span>
          <div class="meta">
            <span>${bytes}</span>
            <span>${cycles}m</span>
          </div>
        </div>
       </td>"""
  }

  private def renderGrid(grid: Array[Array[String]]): String = {
    grid.zipWithIndex.map { case (row, rowIdx) =>
      val rowHex = s"${rowIdx.toHexString.toUpperCase}0+"
      s"""<tr>
        <th>$rowHex</th>
        ${row.mkString}
      </tr>"""
    }.mkString("\n")
  }
}
