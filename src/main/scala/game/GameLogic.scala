package game

import gamelib.GridPanel
import game.pieces.*
import game.Grid
import game.pieces.Player.{Attacker, Defender}
import game.effects.CaptureEffect
import game.powerups.{DiagonalMove, Leap}

import scala.jdk.CollectionConverters.*



class GameLogic(val logicGrid: Grid[Piece], val panel: GridPanel, val gameRows: Int, val powerUpSelector: UI.PowerUpSelector) {
  var currentPlayer: Player = Player.Attacker // Will change between Attacker - Defender
  var selectedPiece: Option[Piece] = None // Can change to Some(piece) or None
  var captureEffects: List[CaptureEffect] = List() // Can change to List of effects
  var isGameOver: Boolean = false // Can change to true

  // Stores remaining power up counts in a map
  var powerUpCounts: Map[(Player, powerups.PowerUpType), Int] = Map(
    (Player.Defender, powerups.PowerUpType.DiagonalMove) -> 3,
    (Player.Attacker, powerups.PowerUpType.DiagonalMove) -> 3,
    (Player.Defender, powerups.PowerUpType.Leap) -> 2,
    (Player.Attacker, powerups.PowerUpType.Leap) -> 2,
    (Player.Defender, powerups.PowerUpType.Exterminate) -> 2,
    (Player.Attacker, powerups.PowerUpType.Exterminate) -> 2,
    (Player.Defender, powerups.PowerUpType.TemporarySoldier) -> 1,
    (Player.Attacker, powerups.PowerUpType.TemporarySoldier) -> 1,
  )

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
  /** Check if a piece is at a corner*/
  private def isAtCorner(piece: Piece): Boolean =
    this.isCorner(piece.row, piece.column)

  // Center square coordinates
  private val centerSquare: (Int, Int) =
    (this.gameRows / 2, this.logicGrid.columns / 2)

  /** Check for center square */
  private def isCenter(row: Int, col: Int): Boolean =
    (row, col) == centerSquare
  /** Check if piece is at center */
  private def isAtCenter(piece: Piece): Boolean =
    this.isCenter(piece.row, piece.column)


  /** Player clicks on a square (main game logic) */

  def selectSquare(row: Int, column: Int): Unit =
    // Remove previous effects (from UI)
    this.captureEffects.foreach {
      effect =>
        this.panel.removeDrawable(effect)
    }
    this.captureEffects = List() // Empty again

    // GAME OVER, selectSquare logic isn't relevant anymore
    if (this.isGameOver) {
      return
    }

    val clickedSquare = this.logicGrid.getPiece(row, column)

    this.selectedPiece match {
      // No piece selected yet, new click as selection attempt
      case None =>
        // Clicked on a piece field
        clickedSquare match {
          // Clicked on a valid piece from own team (select piece)
          case Some(piece) =>
            if (piece.player == this.currentPlayer) {
              this.selectedPiece = Some(piece)
              piece.isSelected = true
              this.updatePowerUpStatus(Some(piece))
              this.panel.repaint()
              println(s"Player ${this.currentPlayer} selected piece on [${piece.row}, ${piece.column}]")
            }
            // Clicked on an invalid piece from opponent (don't select piece)
            else {
              this.selectedPiece = None
              println(s"Player ${this.currentPlayer} selected WRONG piece on [${piece.row}, ${piece.column}], it's player ${this.currentPlayer}'s turn!")
            }
          // Clicked on an empty field (no piece) => do nothing
          case None =>
            this.selectedPiece = None
            println(s"Player ${this.currentPlayer} selected an empty field")

        }

      // A piece is already selected, new click as selection attempt
      case Some(s_piece) =>
        // deactivate all power ups (choise phase over)
        this.updatePowerUpStatus(None)

        // 1) UI bar => power up bar
        if (row >= this.gameRows) {
          // King can not get a power up
          if (!s_piece.canReceivePowerUp) {
            println("Only Soldiers can receive power ups")
            s_piece.isSelected = false
            this.selectedPiece = None
            this.panel.repaint()
            return
          }
          val allPowerUpTypes = powerups.PowerUpType.values
          if (column < allPowerUpTypes.length) {
            val powerUpType = allPowerUpTypes(column)

            powerUpType match {
              case powerups.PowerUpType.DiagonalMove =>
                val count = this.powerUpCounts.getOrElse((this.currentPlayer, powerUpType), 0)
                if (count > 0) {
                  println(s"Power up ${powerUpType.displayName} activated")
                  val poweredPiece = new DiagonalMove(s_piece)
                  this.logicGrid.addPiece(poweredPiece, s_piece.row, s_piece.column)
                  this.panel.removeDrawable(s_piece)
                  this.panel.addDrawables(List(poweredPiece).asJava)
                  poweredPiece.isSelected = true
                  this.selectedPiece = Some(poweredPiece)
                  this.panel.repaint()
                  return

                } else {
                  println(s"No power up ${powerUpType.displayName} left")
                }
                // ASSIGN Diagonal Move power up
                //println(s"Power up '${powerUpType.displayName}' assigned to piece at [${s_piece.row},${s_piece.column}]!.")
                //val poweredPiece = new DiagonalMove(s_piece)
                //poweredPiece.isSelected = false
                //this.logicGrid.addPiece(poweredPiece, s_piece.row, s_piece.column)
                //panel.removeDrawable(s_piece)
                //panel.addDrawables(List(poweredPiece).asJava)
                // Switch turns
                //this.currentPlayer = if (this.currentPlayer == Player.Attacker) Player.Defender else Player.Attacker
                //println(s"Next turn: Player ${this.currentPlayer}")

              case powerups.PowerUpType.Leap =>
                val count = this.powerUpCounts.getOrElse((this.currentPlayer, powerUpType), 0)
                if (count > 0) {
                  println(s"Power up ${powerUpType.displayName} activated")
                  val poweredPiece = new Leap(s_piece)
                  this.logicGrid.addPiece(poweredPiece, s_piece.row, s_piece.column)
                  this.panel.removeDrawable(s_piece)
                  this.panel.addDrawables(List(poweredPiece).asJava)
                  poweredPiece.isSelected = true
                  this.selectedPiece = Some(poweredPiece)
                  this.panel.repaint()
                  return

                } else {
                  println(s"No power up ${powerUpType.displayName} left")
                }

              case _ =>
                println(s"Action for '${powerUpType.displayName}' not implemented yet.")
            }
          }
        } else { // Not in the UI bar, so on the game board


          clickedSquare match {
            // Clicked on an other piece (cancel selection)
            case Some(piece) =>
              println(s"Selection canceled: Player ${this.currentPlayer} selected a non-empty field")
              val pieceToReset: Piece = s_piece match {
                // It was a powered up piece, so reset power
                case powered: powerups.PowerUp =>
                  val originalPiece = powered.power // origninal piece
                  this.logicGrid.addPiece(originalPiece, powered.row, powered.column)
                  this.panel.removeDrawable(powered)
                  this.panel.addDrawables(List(originalPiece).asJava)

                  originalPiece

                // Normal selection, so nothing to change
                case notPowered =>
                  notPowered
              }
              // In both cases, the piece gets deselected
              this.selectedPiece = None
              pieceToReset.isSelected = false
              this.panel.repaint()

            // Click on an empty cell
            case None =>
              // Only if valid move, move the piece
              if (this.isValidMove(s_piece, row, column)) {
                println(s"Moved to[$row, $column]")

                // Store what we have to keep
                val pieceToKeep: Piece = s_piece match {
                  // 1) if s_piece is a powered piece
                  case powered: powerups.PowerUp =>
                    val powerUpType = powered.powerUpType
                    val count = this.powerUpCounts.getOrElse((this.currentPlayer, powerUpType),0)
                    this.powerUpCounts = this.powerUpCounts.updated((this.currentPlayer, powerUpType), count - 1)
                    println(s"Power up ${powerUpType.displayName} used. ${count - 1} left.")

                    powered.power // Original piece

                  case notPowered =>
                    notPowered // Normal move, so keep normal piece without changes

                }

                // Move piece on the board
                this.logicGrid.movePiece(s_piece.row, s_piece.column, row, column)
                pieceToKeep.updatePosition(row, column) // Updates position of piece

                this.logicGrid.addPiece(pieceToKeep, row, column)
                this.panel.removeDrawable(s_piece)
                this.panel.addDrawables(List(pieceToKeep).asJava)
                pieceToKeep.isSelected = false

                // CAPTURES? A piece is moved, so check if there are captures
                this.checkCaptures(pieceToKeep)

                // GAME OVER?
                this.checkGameOver()
                // TODO: WHAT IF GAME OVER?

                // If not game over
                if (!this.isGameOver) {
                  // Switch turns
                  this.removePowerUps(this.currentPlayer)
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

          } // end 2nd click
        } // Some(s_piece) else finished (if it was not in UI)

        // Always deselect after any action
        // (still in selectedPiece match)
         s_piece.isSelected = false
         this.panel.repaint()
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


  /** Removes a captured soldier from the board and adds capture effect */

  private def captureSoldier(row: Int, column: Int, piece: Soldier): Unit = {
    this.logicGrid.removePiece(row, column)
    this.panel.removeDrawable(piece)
    println(s"CAPTURE: Soldier at ($row, $column) has been captured")

    val effect = new CaptureEffect(row, column)
    this.panel.addDrawables(List(effect).asJava) // List Java ≠ List Scala
    // Cons effect to list
    this.captureEffects = effect :: this.captureEffects
  }


  /** Updates the available power ups in the UI based on the selected piece */

  private def updatePowerUpStatus(selectedPiece: Option[Piece]): Unit = {
    var activePowerUps = Set[powerups.PowerUpType]()

    selectedPiece.foreach { piece =>
      // Only if piece is a soldier or already has a power up
      if (piece.isInstanceOf[Soldier] || piece.isInstanceOf[powerups.PowerUp]) {
        activePowerUps = powerups.PowerUpType.values.toSet
      } else { // None or King
        activePowerUps = Set.empty
      }
      // TODO (currently activates all powerups)
    }
    this.powerUpSelector.activePowerUps = activePowerUps
  }

  /** Removes active power ups from player*/

  private def removePowerUps(player: Player): Unit = {
    this.logicGrid.allPieces().foreach {
      case powered: powerups.PowerUp =>
        // if it's from current player
        if (powered.player == player) {
          val originalPiece = powered.power
          // Add original piece and remove power up
          this.logicGrid.addPiece(originalPiece, powered.row, powered.column)
          this.panel.removeDrawable(powered)
          this.panel.addDrawables(List(originalPiece).asJava)
        }
      case _ => () // all other pieces
    }
  }


}