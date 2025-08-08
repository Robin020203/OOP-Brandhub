package gamelib;

import java.awt.*;
import java.io.IOException;
import javax.imageio.ImageIO;

/**
 * Deze klassen maakt het mogelijk om een afbeelding in te laden die je in de <code>src/main/resources</code>
 * map geplaatst hebt.
 *
 * Bijvoorbeeld om een afbeelding genaamd "voorbeeld.png" in te laden kan je de volgende code gebruiken:
 * <code>
 * {@code
 * AssetsLoader.loadImage("voorbeeld.png")
 * }
 * </code>
 */
public class AssetsLoader {
    public static Image loadImage(String name) throws IOException {
        Image img = ImageIO.read(AssetsLoader.class.getResourceAsStream("/"+name));
        return img;
    }
}
