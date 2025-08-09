package game.powerups

import game.Grid
import game.pieces.Piece

class Leap(power: Piece) extends PowerUp(power, PowerUpType.Leap) {

  override def isValidMove(grid: Grid[Piece], toRow: Int, toColumn: Int): Boolean = {
    // Old rules apply
    val wasValidBefore = this.power.isValidMove(grid, toRow, toColumn)

    // only horizontal or vertical move that is not the same place
    val isHorizontal = this.row == toRow
    val isVertical = this.column == toColumn
    val hasMoved = !(isHorizontal && isVertical)
    val straightLine = (isHorizontal || isVertical)
    val straightMove = hasMoved && straightLine

    // Old rule OR Straight rule without restrictions to apply
    wasValidBefore || straightMove
  }
}

