package game.pieces

import java.awt.Graphics2D
import gamelib.AssetsLoader


class Soldier(row: Int, column: Int, val player: Player) extends Piece(row, column) {
  
  def image: String = "soldier.png"

  override def canReceivePowerUp: Boolean = true
}

//class Soldaat extends Piece:
//  override def draw(g: Graphics2D): Unit =
//    g.drawImage(
//      AssetsLoader.loadImage("soldier.png"), // afbeelding
//      HiveGame.cellWidth*3,   // kolom (derde kolom)
//      HiveGame.cellHeight*4,  // rij (vierde rij)
//      HiveGame.cellWidth,     // breedte
//      HiveGame.cellHeight,    // hoogte
//      null                    // te negeren
//    )