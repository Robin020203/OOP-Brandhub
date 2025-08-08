package game

import gamelib.GridPanel
import game.pieces.*
import game.Grid
import game.pieces.Player.{Attacker, Defender}
import game.effects.CaptureEffect
import scala.jdk.CollectionConverters.*



class GameLogic(val logicGrid: Grid[Piece], val panel: GridPanel, val gameRows: Int, val powerUpSelector: UI.PowerUpSelector) {
  var currentPlayer: Player = Player.Attacker // Will change between Attacker - Defender
  var selectedPiece: Option[Piece] = None // Can change to Some(piece) or None
  var captureEffects: List[CaptureEffect] = List() // Can change to List of effects
  var isGameOver: Boolean = false // Can change to true


  // Corners set
  private val corners: Set[(Int, Int)] = Set(
    (0, 0), // upper-left
    (0, this.logicGrid.columns - 1), // upper-right
    (this.gameRows - 1, 0), // under-left
    (this.gameRows - 1, this.logicGrid.columns - 1) // under-right
  )
  /** Check for corner */
  private def isCorner(row: Int, col: Int): Boolean =
    corners.contains((row, col))
  /** Check if a piece is at a corner */
  private def isAtCorner(piece: Piece): Boolean =
    this.isCorner(piece.row, piece.column)

  // Center square
  private val centerSquare: (Int, Int) =
    (this.gameRows / 2, this.logicGrid.columns / 2)

  /** Check for center square */
  private def isCenter(row: Int, col: Int): Boolean =
    (row, col) == centerSquare
  /** Check if piece is at center */
  private def isAtCenter(piece: Piece): Boolean =
    this.isCenter(piece.row, piece.column)


  /** Player clicks on a field */

  def selectSquare(row: Int, column: Int): Unit =
    // Remove previous effect
    this.captureEffects.foreach {
      effect =>
        this.panel.removeDrawable(effect)
    }
    this.captureEffects = List()

    // GAME OVER
    if (this.isGameOver) {
      return
    }

    val clickedSquare = this.logicGrid.getPiece(row, column)

    this.selectedPiece match {
      // No piece selected yet
      case None =>
        // Click on a piece field
        clickedSquare match {
          // Click on a valid piece (select piece)
          case Some(piece) =>
            if (piece.player == this.currentPlayer) {
              this.selectedPiece = Some(piece)
              piece.isSelected = true
              this.updatePowerUpStatus(Some(piece))
              this.panel.repaint()
              println(s"Player ${this.currentPlayer} selected piece on [${piece.row}, ${piece.column}]")
            }
            // Click on an invalid piece (don't select piece)
            else {
              this.selectedPiece = None
              println(s"Player ${this.currentPlayer} selected WRONG piece on [${piece.row}, ${piece.column}], it's player ${this.currentPlayer}'s turn!")
            }
          // Select an empty field (no piece)
          case None =>
            this.selectedPiece = None
            println(s"Player ${this.currentPlayer} selected an empty field")

        }

      // A piece is selected
      case Some(s_piece) =>
        // After action, deactivate all power ups
        this.updatePowerUpStatus(None)
        // Click on a square
        clickedSquare match {

          // Click on an other piece
          case Some(piece) =>
            this.selectedPiece = None
            s_piece.isSelected = false
            this.panel.repaint()
            println(s"Selection canceled: Player ${this.currentPlayer} selected a non-empty field")
          // Click on an empty cell
          case None =>
            // Only if valid move, move the piece
            if (this.isValidMove(s_piece, row, column)) {
              println(s"Moved to[$row, $column]")
              this.logicGrid.movePiece(s_piece.row, s_piece.column, row, column)
              s_piece.row = row
              s_piece.column = column

              // CAPTURES? A piece is moved, so check if there are captures
              this.checkCaptures(s_piece)

              // GAME OVER?
              this.checkGameOver()
              // TODO: WHAT IF GAME OVER?

              if (!this.isGameOver) {
                // Switch turns
                this.currentPlayer = if (this.currentPlayer == Player.Attacker) Player.Defender else Player.Attacker
                println(s"Next turn: Player ${this.currentPlayer}")
              }

            }
            // Not a valid move so don't move
            else {
              println("Invalid move")
            }
            // We always deselect the piece and repaint (whether it's a valid move or not)
            s_piece.isSelected = false
            this.panel.repaint() // redraw board from java

        }
        // Whatever we do after a piece is selected, deselect the piece after action
        // (still in selectedPiece match)
        this.selectedPiece = None

    }
    // after selectedPiece match (print status after every click)
    println(s"clicked on field: [$row , $column]. Current player: ${this.currentPlayer}")


  /** Check if a move is valid */

  def isValidMove(piece: Piece, toRow: Int, toColumn: Int): Boolean = {
    // 1) UI is not part of the game squares
    if (toRow >= this.gameRows) {
      println("Invalid: no one can move to the UI-Row")
      return false
    }

    // 2) If location is not on the board = false
    if (!this.logicGrid.isValidPosition(toRow, toColumn)) {
      println("Invalid position outside board")
      return false
    }

    // 3) No one can move to King's start position (middle of the board)
    if (isCenter(toRow, toColumn)) {
      println("Invalid position: No one can move to the King's throne")
      return false
    }

    // 4) Only the king can move to a corner square
    if (isCorner(toRow, toColumn) && !piece.isInstanceOf[King]) {
      println("Invalid position: Only the king can move to a corner square")
      return false
    }
    // If all rules are followed, isValidMove logic is decided by pieces
    piece.isValidMove(this.logicGrid, toRow, toColumn)
  }


  /** Check if there are captures */

  private def checkCaptures(movedPiece: Piece): Unit = {
    // Coordinates of all neighbours
    val neighbourPositions = List(
      (movedPiece.row + 1, movedPiece.column), // Upper neighbour
      (movedPiece.row - 1, movedPiece.column), // Lower neighbour
      (movedPiece.row, movedPiece.column + 1), // Right neighbour
      (movedPiece.row, movedPiece.column - 1) // Left neighbour
    )
    // For every neighbour position
    neighbourPositions.foreach {
      case (row, column) =>
        // Check what is on that position
        this.logicGrid.getPiece(row, column) match {
          // If it's a soldier and it's one from the opponent
          case Some(neighbour: Soldier) if neighbour.player != this.currentPlayer =>
            // Other side of neighbour
            val toRow = row + (row - movedPiece.row) // Row of neighbour + row difference
            val toColumn = column + (column - movedPiece.column) // Column of neighbour + column difference

            this.logicGrid.getPiece(toRow, toColumn) match {
              // CAPTURE: If other side of neighbour is our piece
              case Some(toPiece) if toPiece.player == this.currentPlayer =>
                // Capture between 2 pieces
                this.captureSoldier(row, column, neighbour)
              case _ if this.isCorner(toRow, toColumn) || this.isCenter(toRow, toColumn) =>
                this.captureSoldier(row, column, neighbour)

              // Other side of neighbour is their piece or empty field (≠ corner/center)
              case _ => ()
            }
          // Neighbour is the king and moved piece is the attacker (other team than king)
          case Some(neighbour: King) if neighbour.player != this.currentPlayer =>
            // Other side of king
            val toRow = row + (row - movedPiece.row) // Row of neighbour + row difference
            val toColumn = column + (column - movedPiece.column) // Column of neighbour + column difference

            this.logicGrid.getPiece(toRow, toColumn) match {
              // GAME OVER: If other side of king is also an attacker
              case Some(toPiece) if toPiece.player == this.currentPlayer =>
                println("GAME OVER: ATTACKER WON")
                this.isGameOver = true
              // Neighbour is not a soldier of the enemy
              case _ => ()
            }
          // Neighbour is not a soldier of the enemy or the king
          case _ => ()
        }
    }
  }


  /** Check if the defender won (if the king got to a corner) */

  private def checkGameOver(): Unit = {
    val kingPos = this.logicGrid.allPieces().find(piece => piece.isInstanceOf[King])


    // DEFENDER WINS
    kingPos.foreach {
      king =>
        if (isAtCorner(king)) {
          println("GAME OVER: DEFENDER WON")
          this.isGameOver = true
        }
    }
  }
  private def captureSoldier(row: Int, column: Int, piece: Soldier): Unit = {
    this.logicGrid.removePiece(row, column)
    this.panel.removeDrawable(piece)
    println(s"CAPTURE: Soldier at ($row, $column) has been captured")

    val effect = new CaptureEffect(row, column)
    this.panel.addDrawables(List(effect).asJava) // List Java ≠ List Scala
    // Cons effect to list
    this.captureEffects = effect :: this.captureEffects
  }

  private def updatePowerUpStatus(selectedPiece: Option[Piece]): Unit = {
    var activePowerUps = Set[powerups.PowerUpType]()

    selectedPiece.foreach { piece =>
      // TODO
      activePowerUps = powerups.PowerUpType.values.toSet
    }
    this.powerUpSelector.activePowerUps = activePowerUps

  }

}