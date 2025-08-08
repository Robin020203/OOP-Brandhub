package game.powerups

// Defines 4 possible power ups with 'parameterized enum'-feature
enum PowerUpType(val displayName: String, val activeImage: String, val inactiveImage: String):
  case Exterminate extends PowerUpType("Exterminate", "exterminate.png", "exterminate_bw.png") // Verdelgen
  case DiagonalMove extends PowerUpType("Diagonal Move", "diagonal_move.png", "diagonal_move_bw.png") // Diagonaal bewegen
  case Leap extends PowerUpType("Jump", "jump.png", "jump_bw.png") // Springen
  case TemporarySoldier extends PowerUpType("Temporary Soldier", "temporary_soldier.png", "temporary_soldier_bw.png") // Extra soldaat