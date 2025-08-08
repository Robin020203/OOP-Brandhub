package gamelib;

/**
 * Een simpele interface om events van de muis op te vangen
 */
@FunctionalInterface
public interface SimpleMouseListener {
    /**
     * Gets called when the mouse has done something (click, move, released, ...)
     * @param x the x coordinate of the mouse
     * @param y the y coordinate of the mouse
     */
    public void onEvent(int x, int y);
}
