package game

import game.Grid
import game.pieces.{King, Piece, Soldier, Player}
import gamelib.*

import java.awt.Graphics2D
import scala.jdk.CollectionConverters.*


object HiveGame {
  // Changeable
  val cellSize: Int = 100
  val gameColumns: Int = 7
  val gameRows: Int = 7

  // NOT changeable
  val uiRows = 1 // power up row
  val windowWidth: Int = this.cellSize * this.gameColumns
  val windowHeight: Int = this.cellSize * (this.gameRows + this.uiRows)
  val numRows: Int = this.gameRows + this.uiRows
  val numColumns: Int = this.gameColumns
  val cellWidth: Int = windowWidth / numColumns
  val cellHeight: Int = windowHeight / numRows


  def main(args: Array[String]): Unit =

    // GUI and panel from gamelib
    val gui = new GUI(this.windowWidth, this.windowHeight, this.numRows, this.numColumns, 0)
    val panel = gui.getGridPanel()

    // logicGrid that stores information
    val logicGrid = new Grid[Piece](this.numRows, this.numColumns)
    println(s"An empty ${logicGrid.rows}x${logicGrid.columns} grid is made.")
    // connect logicGrid to visual panel
    val gameLogic = new GameLogic(logicGrid, panel, this.gameRows)


    // All start positions of pieces
    // defence (yellow)
    val king = new King(3, 3)
    val defenderSoldiers = List(
      new Soldier(2, 3, Player.Defender),
      new Soldier(3, 2, Player.Defender),
      new Soldier(3, 4, Player.Defender),
      new Soldier(4, 3, Player.Defender)
    )

    // attacker (brown -> red)
    val attackerSoldiers = List(
      new Soldier(0, 3, Player.Attacker),
      new Soldier(1, 3, Player.Attacker),
      new Soldier(3, 0, Player.Attacker),
      new Soldier(3, 1, Player.Attacker),
      new Soldier(3, 5, Player.Attacker),
      new Soldier(3, 6, Player.Attacker),
      new Soldier(5, 3, Player.Attacker),
      new Soldier(6, 3, Player.Attacker)
    )

    // Add pieces in a list to add them to the logicGrid
    val allPieces: List[Piece] = List(king) ++ defenderSoldiers ++ attackerSoldiers
    allPieces.foreach { piece =>
      logicGrid.addPiece(piece, piece.row, piece.column)
    }
    // Draw all pieces on the board
    panel.addDrawables(allPieces.asJava) // Visual panel (asJava gamelib)


    // Mouseclick
    def onClick(x: Int, y: Int): Unit = {
      // From pixel coordinates (x, y) to grid coordinates (row, column).
      val row = y / this.cellHeight
      val column = x / this.cellWidth
      gameLogic.selectSquare(row, column) // Process gameLogic with click
    }

    // Connect onClick to panel
    panel.setPressListener(onClick)


}//obj