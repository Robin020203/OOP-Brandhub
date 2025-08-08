package game
import game.pieces.Piece

class Grid[T](val rows: Int, val columns: Int) {
 // Board (with vector of vectors) filled with Some[T] or None
 private var board: Vector[Vector[Option[T]]] = Vector.fill(this.rows, this.columns)(None)

 /** Adds an object[T] to a location */
 def addPiece(piece: T, row: Int, column: Int): Unit =
  this.board = this.board.updated(row, this.board(row).updated(column, Some(piece))) //Some(T)
  
 /** Removes an object[T] from a location */
 def removePiece(row: Int, col: Int): Unit =
  this.board = this.board.updated(row, this.board(row).updated(col, None)) //None

 /** Check if position is on the board */
 def isValidPosition(row: Int, col: Int): Boolean =
  row >= 0 && row < this.rows && col >= 0 && col < this.columns

 /** Get a piece of a from a location */
 def getPiece(row: Int, column: Int): Option[T] =
  if isValidPosition(row, column) then
   this.board(row)(column) // Some[T]
  else
   None

 /** Move a piece from a location to a location.
  * combination of 'getPiece', 'removePiece' en 'addPiece' */
 def movePiece(fromRow: Int, fromColumn: Int, toRow: Int, toColumn: Int): Unit =
  this.getPiece(fromRow, fromColumn) match
   case Some(piece) =>
    this.removePiece(fromRow, fromColumn)
    this.addPiece(piece, toRow, toColumn)
   case None =>
    println(s"No valid piece on ($fromRow, $fromColumn) to move.")


 /** Make a copy of the grid */
 def copy(): Grid[T] = // from WPO4
  val newGrid: Grid[T] = new Grid[T](this.rows, this.columns)
  for row <- 0 until this.rows do
   for column <- 0 until this.columns do
    this.getPiece(row, column) match
     case Some(piece) => newGrid.addPiece(piece, row, column) //match? add Piece to new grid
     case None => () // do nothing
  newGrid


 /** List of all Pieces that are on the board */
 def allPieces(): List[T] = // from WPO4 (allCells)
  var listOfPieces: List[T] = List()
  for row <- 0 until this.rows do
   for column <- 0 until this.columns do
    this.getPiece(row, column) match
     case Some(piece) => listOfPieces = piece :: listOfPieces //match? cons piece to list
     case None    => () // do nothing

  listOfPieces
}