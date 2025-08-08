package game.pieces

import java.awt.Graphics2D
import gamelib.AssetsLoader


class King(row: Int, column: Int) extends Piece(row, column) {
  val player: Player = Player.Defender
  def image: String = "king.png" 

}