package game.powerups

import game.pieces.Piece
import game.Grid

class DiagonalMove(power: Piece) extends PowerUp(power, PowerUpType.DiagonalMove) {

  override def canReceivePowerUp: Boolean = true

  override def isValidMove(grid: Grid[Piece], toRow: Int, toColumn: Int): Boolean = {
    // Old rules apply
    val wasValidBefore = this.power.isValidMove(grid, toRow, toColumn)

    // New diagonal rule
    val isDiagonal = math.abs(this.row - toRow) == math.abs(this.column - toColumn) // abs(row diff) = abs(column diff)

    // Path clear?
    def isDiagonalPathClear(): Boolean = {
      val distance = math.abs(this.row - toRow)
      // Check direction + define a step (.sign gives +1 if pos and -1 if neg)
      val rowStep = (toRow - this.row).sign // +1 or -1?
      val columnStep = (toColumn - this.column).sign // +1 or -1?
      val range = 1 until distance
      range.forall {
        i =>
          val checkRow = this.row + i * rowStep
          val checkColumn = this.column + i * columnStep
          grid.getPiece(checkRow, checkColumn).isEmpty
      }
    }


    // Old rule OR Diagonal rule with clear path needs to apply
    wasValidBefore || (isDiagonal && isDiagonalPathClear())
  }
}