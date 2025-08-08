package game.powerups

import game.pieces.Piece
import game.Grid

class DiagonalMove(power: Piece) extends PowerUp(power) {

  override def isValidMove(grid: Grid[Piece], toRow: Int, toColumn: Int): Boolean = {
    // Old rules apply
    val wasValidBefore = super.isValidMove(grid, toRow, toColumn)

    // New diagonal rule
    val isDiagonal = math.abs(this.row - toRow) == math.abs(this.column - toColumn) // abs(row diff) = abs(column diff)
    // TODO: Check path

    // 1 of the 2 rules needs to apply
    wasValidBefore || isDiagonal
  }
}
