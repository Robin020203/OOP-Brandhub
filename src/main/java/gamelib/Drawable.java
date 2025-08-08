package gamelib;

import java.awt.*;

/**
 * Een <code>Drawable</code> stelt een object
 * voor dat getekend kan worden. De Java code
 * roept herhaaldelijk de <code>draw</code>
 * methode op waarin je dan je code kan
 * zetten waarin het tekenen effectief gebeurt.
 */
public interface Drawable {
    /**
     * @param g the grafische context waarin het object getekend moet worden
     */
    public void draw(Graphics2D g);
}
