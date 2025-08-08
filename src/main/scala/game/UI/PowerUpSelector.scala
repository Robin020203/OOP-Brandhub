package game.UI

import game.HiveGame
import game.powerups.PowerUpType
import gamelib.Drawable
import java.awt.Graphics2D


class PowerUpSelector extends Drawable {
  var activePowerUps: Set[PowerUpType] = Set() // default empty
  private val allPowerUpTypes = PowerUpType.values

  override def draw(g: Graphics2D): Unit = {
    // Loop over every power up and draw image on right place
    this.allPowerUpTypes.zipWithIndex.foreach { (powerUpType, index) =>

      val isActive = this.activePowerUps.contains(powerUpType)
      val imageName = if (isActive) {
        powerUpType.activeImage
      } else {
        powerUpType.inactiveImage
      }
      val image = gamelib.AssetsLoader.loadImage(imageName)
      
      val cellWidth = HiveGame.cellWidth
      val cellHeight = HiveGame.cellHeight
      val row = HiveGame.gameRows // UI row
      val column = index

      val marge = 4
      val size = math.min(cellWidth, cellHeight) - (marge * 2)
      val xOffset = (cellWidth - size) / 2
      val yOffset = (cellHeight - size) / 2
      val x = (column * cellWidth) + xOffset
      val y = (row * cellHeight) + yOffset

      g.drawImage(image, x, y, size, size, null)

    }
  }
}