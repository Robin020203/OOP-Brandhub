package game.pieces

import gamelib.Drawable
import java.awt.{BasicStroke, Color, Graphics2D}
import game.HiveGame

import game.Grid

// Defines 2 possible players
enum Player:
  case Attacker, Defender


abstract class Piece(var row: Int, var column: Int, var isSelected: Boolean = false) extends Drawable {

  val player: Player // override
  def image: String // override
  var isMarkedForExtermination: Boolean = false

  // for PowerUp Class
  def updatePosition(newRow: Int, newColumn: Int): Unit = {
    this.row = newRow
    this.column = newColumn
  }

  // for Soldiers to override
  def canReceivePowerUp: Boolean = false


  /** Draw a piece */

  def draw(g: Graphics2D): Unit = {
    // Load image
    val loadedImage = gamelib.AssetsLoader.loadImage(this.image)
    // Basic info from Main
    val cellWidth = game.HiveGame.cellWidth
    val cellHeight = game.HiveGame.cellHeight
    // Measures for drawing
    val minDimension = math.min(cellWidth, cellHeight)
    val marge = 4
    val size = minDimension - (marge * 2) // size of my drawings (horizontal/vertical)
    // start positions drawings
    val xOffset = (cellWidth - size) / 2
    val yOffset = (cellHeight - size) / 2
    val x = (this.column * cellWidth) + xOffset // horizontal start position
    val y = (this.row * cellHeight) + yOffset//vertical start position

    // Power up piece border
    val hasPowerUp = this.isInstanceOf[game.powerups.PowerUp]

    // Player base color
    val baseColor = this.player match {
      case Player.Attacker => Color.RED.darker()
      case Player.Defender => Color.YELLOW.darker()
    }

    //
    val playerColor: Color = this match {
      case _: game.powerups.Exterminate =>
        baseColor.darker()
      case _: game.powerups.PowerUp =>
        baseColor.brighter()
      case _ =>
        baseColor
    }
    val finalColor = if (isMarkedForExtermination) {
      playerColor.darker()
    } else {
      playerColor
    }

    // Border width and color (other color if it's selected)
    g.setStroke(new BasicStroke(8))

    val borderColor = if (this.isSelected) Color.CYAN else Color.DARK_GRAY

    // Draw border
    g.setColor(borderColor)
    g.drawOval(x, y, size, size)

    // Fill circle and draw image
    g.setColor(finalColor)
    g.fillOval(x, y, size, size)
    g.drawImage(loadedImage, x, y, size, size, null)
  }


  /** Check if a move is valid, moved here to override by power ups
   * It has to be a straight move with a clear path*/

  def isValidMove(grid: Grid[Piece], toRow: Int, toColumn: Int): Boolean = {

    def isPathClear(fromRow: Int, fromColumn: Int, toRow: Int, toColumn: Int): Boolean = {
      // Horizontal move
      if (fromRow == toRow) {
        val min = math.min(fromColumn, toColumn)
        val max = math.max(fromColumn, toColumn)
        val range = (min + 1) until max
        // Returns true if all columns in between are empty
        range.forall(column => grid.getPiece(fromRow, column).isEmpty) // isEmpty = true if None (option[T])

      } else { // Vertical move
        val min = math.min(fromRow, toRow)
        val max = math.max(fromRow, toRow)
        val range = (min + 1) until max
        // Returns true if all rows in between are empty
        range.forall(row => grid.getPiece(row, fromColumn).isEmpty) // isEmpty = true if None (option[T])

      }
    }

    // only horizontal or vertical move that is not the same place
    val isHorizontal = this.row == toRow
    val isVertical = this.column == toColumn
    val hasMoved = !(isHorizontal && isVertical)
    val straightLine = (isHorizontal || isVertical)
    val straightMove = hasMoved && straightLine

    straightMove && isPathClear(this.row, this.column, toRow, toColumn)
  }


}
