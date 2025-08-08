package gamelib;

import javax.swing.*;
import java.awt.*;

/**
 * Maakt een venster aan bij constructie waarin ook een {@link GridPanel} in aanwezig is.
 */
public class GUI extends JFrame {
    private GridPanel panel;

    /**
     * Maak een nieuw venster met de gegeven hoogte en breedte.
     * @param width de breedte van het venster
     * @param height de hoogte van het venster
     * @param rows het aantal rijen in het grid
     * @param columns het aantal kolommen in het grid
     * @param padding de witruimte, in pixels, tussen het grid en de rand van het venster, wordt
     *                toegevoegd boven, onder en links en rechts van het grid.
     */
    public GUI(int width, int height, int rows, int columns, int padding) {
        super();
        this.setSize(new Dimension(width, height+40));
        this.setResizable(false);
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
        panel = new GridPanel(width, height, rows, columns);
        panel.setPadding(padding);
        this.setTitle("Cell Game");
        this.setContentPane(panel);
        this.setVisible(true);
    }

    public GridPanel getGridPanel() {
        return panel;
    }
}
