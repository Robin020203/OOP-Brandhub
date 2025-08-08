package game.powerups

import game.pieces.Piece

abstract class PowerUp(val power: Piece) extends Piece(power.row, power.column, power.isSelected) {

  override def updatePosition(newRow: Int, newColumn: Int): Unit = {
    this.row = newRow
    this.column = newColumn
  }

  // Piece's player and image
  override val player = power.player
  override def image: String = power.image

  var canMoveDiagonally: Boolean = false
  var canJump: Boolean = false
  var canLeap: Boolean = false
  var canGetExtraSoldier: Boolean = false


}
