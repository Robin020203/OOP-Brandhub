package game

import gamelib.GridPanel
import game.pieces.*
import game.Grid
import game.pieces.Player.{Attacker, Defender}
import game.effects.CaptureEffect
import game.powerups.{DiagonalMove, Exterminate, Leap}
import scala.jdk.CollectionConverters.*



class GameLogic(val logicGrid: Grid[Piece], val panel: GridPanel, val gameRows: Int, val powerUpSelector: UI.PowerUpSelector) {
  var currentPlayer: Player = Player.Attacker // Will change between Attacker - Defender
  var selectedPiece: Option[Piece] = None // Can change to Some(piece) or None
  var captureEffects: List[CaptureEffect] = List() // Can change to List of effects
  var isGameOver: Boolean = false // Can change to true

  // To keep track if someone is placing an extra soldier
  var placementForPowerUp: Option[powerups.PowerUpType] = None
  // To keep track if someone is choosing a target for exterminate
  var targetingPowerUp: Option[powerups.PowerUpType] = None
  // Active timers (for power ups)
  var activeTimers: Map[Piece, (Int, Player)] = Map()

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
    this.captureEffects.foreach { effect => this.panel.removeDrawable(effect)}
    this.captureEffects = List() // Empty again

    // GAME OVER, selectSquare logic isn't relevant anymore
    if (this.isGameOver) {return}

    // What is on the selected square
    val clickedSquare = this.logicGrid.getPiece(row, column)

    // Someone is choosing a target (to exterminate)
    if (this.targetingPowerUp.isDefined) {
      this.executeTargetingPowerUp(clickedSquare)
      return
    }
    // Someone is choosing a place (to add an extra soldier)
    if (this.placementForPowerUp.isDefined) {
      this.executePlacementForPowerUp(row, column, clickedSquare)
      return
    }

    this.selectedPiece match {
      // No piece selected yet, new click as selection attempt
      case None =>
        this.executeNoSelection(clickedSquare)
      // A piece was already selected (s_piece), new click as selection attempt
      case Some(s_piece) =>
        this.executeWithSelection(s_piece, row, column, clickedSquare)
    }
    // after selectedPiece match (print status after every click)
    println(s"clicked on field: [$row , $column]. Current player: ${this.currentPlayer}")


  /** Executes whole action when a piece is already selected (s_piece)
   * row, column and clickedSquare are from the new clicked Square */

  private def executeWithSelection(s_piece: Piece, row: Int, column: Int, clickedSquare: Option[Piece]): Unit = {
    // deactivate all power ups (choise phase over)
    this.updatePowerUpStatus(None)

    // 1) UI bar => power up bar
    if (row >= this.gameRows) {
      // King can not get a power up
      if (!s_piece.canReceivePowerUp) {
        println("Only Soldiers can receive power ups")
        this.deselectPieceAndRepaint(s_piece)
        return
      }
      val allPowerUpTypes = powerups.PowerUpType.values
      if (column < allPowerUpTypes.length) {
        val powerUpType = allPowerUpTypes(column)

        powerUpType match {
          // Diagonal Move power up
          case powerups.PowerUpType.DiagonalMove =>
            this.tryActivatePowerUp(s_piece, powerups.PowerUpType.DiagonalMove)
            return
          // Leap power up
          case powerups.PowerUpType.Leap =>
            this.tryActivatePowerUp(s_piece, powerups.PowerUpType.Leap)
            return
          // Exterminate power up (tryActivatePowerUp not applicable)
          case powerups.PowerUpType.Exterminate =>
            val count = this.powerUpCounts.getOrElse((this.currentPlayer, powerUpType), 0)
            if (count > 0) {
              println("Exterminate activated, select an enemy Soldier")
              this.targetingPowerUp = Some(powerups.PowerUpType.Exterminate)
            } else {
              println("No Exterminate power up left")
            }
            return
          // Extra soldier power up (tryActivatePowerUp not applicable)
          case powerups.PowerUpType.TemporarySoldier =>
            val count = this.powerUpCounts.getOrElse((this.currentPlayer, powerUpType), 0)
            if (count > 0) {
              println("Extra soldier activated, select an empty square to place")
              this.placementForPowerUp = Some(powerups.PowerUpType.TemporarySoldier)
            } else {
              println("No Exterminate power up left")
            }
            return
          // Other power ups (if this game gets expanded)
          case _ =>
            println(s"Action for '${powerUpType.displayName}' not implemented yet.")
        }
      }
    } else { // 2) Not in the UI bar, so on the game board


      clickedSquare match {
        // Clicked on an other piece (cancel selection)
        case Some(piece) =>
          println(s"Selection canceled: Player ${this.currentPlayer} selected a non-empty field")
          val pieceToReset: Piece = s_piece match {
            // It was a powered up piece, so reset power
            case powered: powerups.PowerUp =>
              this.revertPowerUp(powered)
            // Normal selection, so nothing to change
            case notPowered =>
              notPowered
          }
          // In both cases, the piece gets deselected
          this.deselectPieceAndRepaint(pieceToReset)

        // Click on an empty cell
        case None =>
          // Only if valid move, move the piece
          if (this.isValidMove(s_piece, row, column)) {
            println(s"Moved to[$row, $column]")

            // Store what we have to keep
            val pieceToKeep: Piece = s_piece match {
              // if s_piece is a powered piece
              case powered: powerups.PowerUp =>
                powered.powerUpType match {
                  // Only in case of Exterminate, keep the effect
                  case powerups.PowerUpType.Exterminate => powered
                  case _ => this.consumeAndRevertPowerUp(powered)
                }
              case notPowered =>
                notPowered // Normal move, so keep normal piece without changes
            }

            // Move piece on the board
            this.executeMove(s_piece, pieceToKeep, row, column)
            pieceToKeep.isSelected = false

            // A Piece is moved so check CAPTURES and GAME OVER
            this.checkCaptures(pieceToKeep)
            this.checkGameOver()
            // TODO: WHAT IF GAME OVER?

            // If not game over switch turns, deselect the piece and repaint panel
            if (!this.isGameOver) {this.endTurn()}
            this.deselectPieceAndRepaint(pieceToKeep)

          }
          // Not a valid move so don't move
          else {
            println("Invalid move")
            val pieceToReset: Piece = s_piece match {
              case powered: powerups.PowerUp =>
                powered.powerUpType match {
                  case powerups.PowerUpType.DiagonalMove | powerups.PowerUpType.Leap =>
                    this.consumeAndRevertPowerUp(powered)
                  case _ => powered
                }
              case notPowered =>
                notPowered
            }
            // We always deselect the piece and repaint (whether it's a valid move or not)
            this.deselectPieceAndRepaint(pieceToReset)
          }
      } // end 2nd click
    } // Some(s_piece) else finished (if it was not in UI)
  }


  /** Executes whole action when there is no piece already selected
   * clickedSquare is the new clicked Square */

  private def executeNoSelection(clickedSquare: Option[Piece]): Unit = {
    // Clicked on a piece field
    clickedSquare match {
      // SELECT PIECE: Clicked on a valid piece from own team
      case Some(piece) =>
        if (piece.player == this.currentPlayer) {
          this.selectedPiece = Some(piece)
          piece.isSelected = true
          this.updatePowerUpStatus(Some(piece))
          this.panel.repaint()
          println(s"Player ${this.currentPlayer} selected piece on [${piece.row}, ${piece.column}]")
        }
        // DON'T SELECT PIECE: Clicked on an invalid piece from opponent
        else {
          this.selectedPiece = None
          println(s"Player ${this.currentPlayer} selected WRONG piece on [${piece.row}, ${piece.column}], it's player ${this.currentPlayer}'s turn!")
        }
      // DON'T SELECT: Clicked on an empty field (no piece)
      case None =>
        this.selectedPiece = None
        println(s"Player ${this.currentPlayer} selected an empty field")

    }
  }


  /** Executes action when a player targets an enemy with a power up
   * clickedSquare is the new clicked Square */

  private def executeTargetingPowerUp(clickedSquare: Option[Piece]): Unit = {
    val powerUpType = this.targetingPowerUp.get

    clickedSquare match {
      // Player clicks on valid target
      case Some(targetPiece: Soldier) =>
        if (targetPiece.player != this.currentPlayer) {
          println("Valid target selected")
          targetPiece.isMarkedForExtermination = true

          // TODO: if-test needed if there are more targeting power ups
          // New powered piece
          val exterminatedPiece = new Exterminate(targetPiece)
          // Replace original piece by powered piece
          this.replacePiece(targetPiece, exterminatedPiece)

          // Add to timer map
          this.activeTimers = this.activeTimers + (exterminatedPiece -> (3, this.currentPlayer))

          // Use power up
          this.consumePowerUp(powerUpType)

          // Reset targeting mode and selection, and switch turn
          this.targetingPowerUp = None
          this.deselectPieceAndRepaint(this.selectedPiece.get)
          this.endTurn()
        }

      // Invalid target
      case _ =>
        this.cancelPowerUpAction("Invalid target")
        this.targetingPowerUp = None
        this.deselectPieceAndRepaint(this.selectedPiece.get) // selected piece is defined so .get works
    }
  }


  /** Executes action to place a new piece on the board for a power up
   * row, column and clickedSquare are from the new clicked Square */

  private def executePlacementForPowerUp(row: Int, column: Int, clickedSquare: Option[Piece]): Unit = {
    val powerUpType = this.placementForPowerUp.get

    clickedSquare match {
      case None =>
        if (!isCenter(row, column) && !isCorner(row, column)) {
          println("New soldier is placed")
          val newSoldier = new Soldier(row, column, this.currentPlayer)

          // Add to board
          this.logicGrid.addPiece(newSoldier, row, column)
          this.panel.addDrawables(List(newSoldier).asJava)
          this.panel.repaint()

          // Start timer
          this.activeTimers += (newSoldier -> (3, this.currentPlayer))

          // Use charge (power up)
          this.consumePowerUp(powerUpType)

          // Reset and switch turn
          this.placementForPowerUp = None
          this.deselectPieceAndRepaint(this.selectedPiece.get) // selected piece is defined so .get works
          this.endTurn()
        }

      case _ =>
        this.cancelPowerUpAction("Invalid location")
        this.placementForPowerUp = None
        this.deselectPieceAndRepaint(this.selectedPiece.get)
    }
  }


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
      // Only if piece is a soldier or already has a power up, and there are power ups left
      //if (piece.isInstanceOf[Soldier] || piece.isInstanceOf[powerups.PowerUp]) {
      if (piece.canReceivePowerUp) {
        val allPowerUpTypes = powerups.PowerUpType.values
        //activePowerUps = powerups.PowerUpType.values.toSet
        activePowerUps = allPowerUpTypes.filter {
          powerUpType =>
            val count = this.powerUpCounts.getOrElse((this.currentPlayer, powerUpType), 0)
            count > 0
        }.toSet
      }
      // Else Set stays empty
    }
    // Filtered set
    this.powerUpSelector.activePowerUps = activePowerUps
    this.panel.repaint()
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


  /** Deselects pieces and repaints panel*/

  private def deselectPieceAndRepaint(pieceToDeselect: Piece): Unit = {
    pieceToDeselect.isSelected = false
    this.selectedPiece = None
    this.panel.repaint()
  }

  /** Selects pieces and repaints panel*/

  private def selectPieceAndRepaint(pieceToDeselect: Piece): Unit = {
    pieceToDeselect.isSelected = true
    this.selectedPiece = Some(pieceToDeselect)
    this.panel.repaint()
  }

  /** Replaces piece by other piece*/

  private def replacePiece(oldPiece: Piece, newPiece: Piece): Unit = {
    this.logicGrid.addPiece(newPiece, oldPiece.row, oldPiece.column)
    this.panel.removeDrawable(oldPiece)
    this.panel.addDrawables(List(newPiece).asJava)
    this.panel.repaint()
  }


  /** Can activate a power up if there are >0 left
   * ONLY if it is a direct power up and there is no extra action needed */

  private def tryActivatePowerUp(piece: Piece, powerUpType: powerups.PowerUpType): Unit = {
    val count = this.powerUpCounts.getOrElse((this.currentPlayer, powerUpType), 0)
    if (count > 0) {
      println(s"Power up ${powerUpType.displayName} activated")

      val poweredPiece: powerups.PowerUp = powerUpType match {
        case powerups.PowerUpType.DiagonalMove => new DiagonalMove(piece)
        case powerups.PowerUpType.Leap => new Leap(piece)
        case powerups.PowerUpType.Exterminate => new Exterminate(piece)
        //case powerups.PowerUpType.TemporarySoldier => new TemporarySoldier(piece)
      }
      // MOVE TIMER!
      if (this.activeTimers.contains(piece)) {
        val timerInfo = this.activeTimers(piece)

        this.activeTimers = this.activeTimers - piece
        this.activeTimers = this.activeTimers + (poweredPiece -> timerInfo)
      }


      this.replacePiece(piece, poweredPiece)
      this.selectPieceAndRepaint(poweredPiece)

    } else {
      println(s"No power up ${powerUpType.displayName} left")
    }
  }


  /** Reverts a powered piece to its original form without power up */

  private def revertPowerUp(poweredPiece: powerups.PowerUp): Piece = {
    val originalPiece = poweredPiece.power // origninal piece

    // GIVES TIMER BACK
    if (this.activeTimers.contains(poweredPiece)) {
      val timerInfo = this.activeTimers(poweredPiece)

      this.activeTimers = this.activeTimers - poweredPiece
      this.activeTimers = this.activeTimers + (originalPiece -> timerInfo)
    }

    // Put original piece on the board
    this.logicGrid.addPiece(originalPiece, poweredPiece.row, poweredPiece.column)
    this.panel.removeDrawable(poweredPiece)
    this.panel.addDrawables(List(originalPiece).asJava)

    originalPiece // Return original piece
  }


  /** Uses a power up and lowering the count */

  private def consumePowerUp(powerUpType: powerups.PowerUpType): Unit = {
    val count = this.powerUpCounts.getOrElse((this.currentPlayer, powerUpType), 0)
    if (count > 0) {
      this.powerUpCounts = this.powerUpCounts.updated((this.currentPlayer, powerUpType), count - 1)
      println(s"Power-up ${powerUpType.displayName} used. ${count - 1} left.")
    }
  }


  /** Reverts a powered piece to its original form without power up */

  private def consumeAndRevertPowerUp(poweredPiece: powerups.PowerUp): Piece = {
    // Use power up and lower counter
    this.consumePowerUp(poweredPiece.powerUpType)

    // Reset piece with other helper function
    this.revertPowerUp(poweredPiece)
  }


  /** Makes a move on the Grid and in the UI
   * It replaces a start piece (can be powered piece) by the end piece (pieceToKeep) */

  private def executeMove(startPiece: Piece, pieceToKeep: Piece, toRow: Int, toColumn: Int): Unit = {
    // Remove piece from start position
    this.logicGrid.removePiece(startPiece.row, startPiece.column)
    //this.logicGrid.movePiece(startPiece.row, startPiece.column, toRow, toColumn)
    // Change coordinates of end piece and add to grid
    pieceToKeep.updatePosition(toRow, toColumn)
    this.logicGrid.addPiece(pieceToKeep, toRow, toColumn)
    // Visual
    this.panel.removeDrawable(startPiece)
    this.panel.addDrawables(List(pieceToKeep).asJava)
  }


  /** Updates all active timers */

  private def updateTimers(): Unit = {
    // Map to keep track of timers of next round (to update)
    var nextTurnTimers: Map[Piece, (Int, Player)] = Map()
    var expiredPieces: List[Piece] = List()
    val nextPlayer = if (this.currentPlayer == Player.Attacker) Player.Defender else Player.Attacker

    // For each timer that is active
    this.activeTimers.foreach {
      case (piece, (turnsLeft, initPlayer)) =>
        // Timer only counts down if it's not the player's turn who started the timer
        if (nextPlayer != initPlayer) {
          if (turnsLeft > 0) {
            nextTurnTimers = nextTurnTimers + (piece -> (turnsLeft - 1, initPlayer))
          } else {
            // Timer ran out (turnsLeft <= 0)
            expiredPieces = piece :: expiredPieces
          }

        } else {
          // Turn of player who started the timer, remains stays the same
          nextTurnTimers = nextTurnTimers + (piece -> (turnsLeft, initPlayer))
        }
    }

    // Change timers with updated timers
    this.activeTimers = nextTurnTimers

    // Remove all expired pieces
    expiredPieces.foreach {
      piece =>
        println(s"Timer ran out, piece at [${piece.row}, ${piece.column}] removed")
        //this.logicGrid.removePiece(piece.row, piece.column)
        //this.panel.removeDrawable(piece)
        this.logicGrid.getPiece(piece.row, piece.column) match {
          // If there is a piece, remove it
          case Some(currentPiece) =>
            this.logicGrid.removePiece(piece.row, piece.column)
            this.panel.removeDrawable(currentPiece)
          case None =>
            // If nothing left in grid, still try to remove
            this.panel.removeDrawable(piece)
        }

        // TODO: Other effect if timer runs out?
        // Same capture-effect as normal death
        val effect = new CaptureEffect(piece.row, piece.column)
        this.panel.addDrawables(List(effect).asJava)
        this.captureEffects = effect :: this.captureEffects
    }
    // If there was a change, repaint panel
    if (expiredPieces.nonEmpty) this.panel.repaint()
  }


  /** End a player's turn */

  private def endTurn(): Unit = {
    // Update timers
    this.updateTimers()
    // Switch turns
    this.currentPlayer = if (this.currentPlayer == Player.Attacker) Player.Defender else Player.Attacker
    println(s"Next turn: Player ${this.currentPlayer}")
  }


  /** cancels actions (used for invalid actions) */

  private def cancelPowerUpAction(message: String): Unit = {
    println(s"$message, action canceled.")
    // Reset both (now this function works for multiple power ups)
    this.targetingPowerUp = None
    this.placementForPowerUp = None
    // Deselect piece that power up tried to use
    if (this.selectedPiece.isDefined) {
      deselectPieceAndRepaint(this.selectedPiece.get)
    }
  }

}