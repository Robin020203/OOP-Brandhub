package game.effects

import gamelib.Drawable

import java.awt.{BasicStroke, Color, Graphics2D}
import game.HiveGame

class CaptureEffect(var row: Int, var column: Int) extends Drawable {
  def image: String = "cross.png"
  def draw(g: Graphics2D): Unit =
    // Load image
    val loadedImage = gamelib.AssetsLoader.loadImage(this.image)

    // Basic info from Main
    val cellWidth = game.HiveGame.cellWidth
    val cellHeight = game.HiveGame.cellHeight
    
    // Measures for drawing
    val marge = 4
    val size = cellWidth - (marge * 2) // size of my drawings (horizontal/vertical)
    val x = (this.column * cellWidth) + marge // horizontal start position
    val y = (this.row * cellHeight) + marge //vertical start position

    g.drawImage(loadedImage, x, y, size, size, null)

}
