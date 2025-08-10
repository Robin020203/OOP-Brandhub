# Project: Hive Defender Game

A strategy game based on Brandhub, expanded with a power up system.

## Known Issue: Visuals of 'Timed Pieces' are not removed correctly when they use an other power up

There is a recurring visual bug concerning pieces that are given a temporary lifespan via a power-up.
When a temporary piece uses an other power up, the bug occurs.
While the pieces are correctly removed from the game logic when their timer expires, 
their visual representation wrongfully remains "frozen" on the game board.

This issue can be triggered with both the 'Exterminate' and 'Extra Soldier' power-ups.

### Steps to Reproduce

**The bug can be triggered via this scenario:**

1.  Activate the 'Exterminate' power up on an enemy piece, or the 'Extra Soldier' power up and place the new soldier on the board.
2.  Before the timer runs out, select the temporary piece (Marked with a darker color) and use an other power up (DiagonalMove or Leap)
3.  Wait the required number of turns (3) for the piece's timer to expire.


### Expected vs. Actual Result

* **Expected Result:** The piece should disappear completely from the game board, and a "capture" effect should appear.
* **Actual Result:** The piece can no longer be selected (which is correct, as it has been removed from the logic), but its icon remains visible on the board.
