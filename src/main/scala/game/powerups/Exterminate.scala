package game.powerups

import game.pieces.{Piece, Player}

class Exterminate(power: Piece) extends PowerUp(power, PowerUpType.Exterminate) {
  override val player: Player = power.player
}
