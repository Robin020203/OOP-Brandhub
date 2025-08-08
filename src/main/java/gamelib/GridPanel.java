package gamelib;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * GridPanel is de grafische representatie van een grid,
 * je zal ook nog een logische representatie hiervoor moeten maken.
 *
 * Het voorziet de volgende methodes:
 * - repaint: zorgt ervoor dat het scherm wordt leeggemaakt en dat alles wordt getekend
 * - addDrawables: geef een lijst van drawables die getekend moeten worden
 * - removeDrawable: verwijdert een specifieke drawable van het tekenopervlak en hertekent het tekenopervlak
 * - clear verwijdert alle drawables
 */
public class GridPanel extends JPanel {
    /**
     * De lijst van cellen (intern)
     */
    private CopyOnWriteArrayList<Drawable> drawables;

    /**
     * Een referentie naar de renderer waarmee we interessante figuren kunnen tekenen (intern)
     */
    private Graphics2D mGraphics;

    /**
     * De breedte van 1 cell (intern)
     */
    private int cellWidth;

    /**
     * De hoogte van 1 cell (intern)
     */
    private int cellHeight;

    /**
     * De breedte van het venster (intern)
     */
    private int windowWidth;

    /**
     * De hoogte van het venster (intern)
     */
    private int windowHeight;

    /**
     * Het aantal rijen (intern)
    */
    private int rows;

    /**
     * Het aantal kolommen (intern)
     */
    private int columns;

    /**
     * De padding (witruimte langst alle kanten) van het grid (intern)
     */
    private int padding;

    /**
     * Specifeert wat er moet gebeuren wanneer je klikt in het venster (intern)
     */
    private SimpleMouseListener pressListener = new EmptyMouseListener();
    /**
     * Specifieert wat er moet gebeuren wanneer je met je muis over het venster beweegt (intern)
     */
    private SimpleMouseListener moveListener = new EmptyMouseListener();
    /**
     * Specifieert wat er moet gebeuren wanneer je eerst met je muis klikt en daarna
     * je linkermuisknop terug loslaat (intern)
     */
    private SimpleMouseListener releaseListener = new EmptyMouseListener();

    /**
     * Vraag de padding op
     *
     * @return de huidige padding van het venster
     */
    public int getPadding() {
        return padding;
    }


    /**
     * Stel in wat er moet gebeuren wanneer je in het venster klikt
     */
    public void setPressListener(SimpleMouseListener clickListener) {
        this.pressListener = clickListener;
    }

    /**
     * Stel in wat er moet gebeuren wanneer je met je muis over het venster heen beweegt
     */
    public void setMoveListener(SimpleMouseListener moveListener) {
        this.moveListener = moveListener;
    }

    /**
     * Stel in wat er moet gebeuren wanneer je de linkermuisknop loslaat
     */
    public void setReleaseListener(SimpleMouseListener releaseListener) {
        this.releaseListener = releaseListener;
    }

    /**
     * Stel de padding in
     */
    public void setPadding(int padding) {
        this.padding = padding;
        cellWidth  = (windowWidth  - padding*2) / columns;
        cellHeight = (windowHeight - padding*2) / rows;
    }

    /**
     * Een constructor voor het <code>GridPanel</code>, verwacht de gewenste breedte
     * en grootte van het grid en diens aantal rijen en kolommen.
     * @param width de breedte van het grid
     * @param height de hoogte van het grid
     * @param rows het aantal rijen van het grid
     * @param columns het aantal kolommen van het grid
     */
    public GridPanel(int width, int height, int rows, int columns) {
        super();
        cellWidth  = (width  - padding*2) / columns;
        cellHeight = (height - padding*2) / rows;
        windowWidth = width;
        windowHeight = height;
        this.rows = rows;
        this.columns = columns;
        drawables = new CopyOnWriteArrayList<>();
        this.addMouseMotionListener(new MouseMotionListener() {
            @Override
            public void mouseDragged(MouseEvent e) {}

            @Override
            public void mouseMoved(MouseEvent e) {
                moveListener.onEvent(e.getX(), e.getY());
            }
        });
        this.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {}

            @Override
            public void mousePressed(MouseEvent e) {
                pressListener.onEvent(e.getX(), e.getY());
            }

            @Override
            public void mouseReleased(MouseEvent e) {
                releaseListener.onEvent(e.getX(), e.getY());
            }

            @Override
            public void mouseEntered(MouseEvent e) {

            }

            @Override
            public void mouseExited(MouseEvent e) {

            }
        });
    }

    /** Wordt intern gebruikt om het grid te tekenen */
    private void drawGrid() {
        for (int i = 0; i < columns; i++) {
            for (int j = 0; j < rows; j++) {
                Rectangle rect = new Rectangle(padding + i*cellWidth, padding + j*cellHeight, cellWidth, cellHeight);
                mGraphics.draw(rect);
            }
        }
    }

    /** Wordt intern aangeroepen om het grid en alle drawables te tekenen */
    @Override
    public void paint(Graphics g) {
        super.paintComponent(g);
        mGraphics = (Graphics2D) g.create();
        this.repaintDrawables();
        mGraphics.dispose();
    }

    /**
     * Hertekent het volledig venster gegeven de cellen
     */
    public void repaintDrawables() {
        mGraphics.clearRect(0, 0, windowWidth, windowHeight);
        this.drawGrid();
        for (Drawable drawable : drawables) {
            drawable.draw(mGraphics);
        }

        this.repaint();
    }

    /**
     * Voeg nieuwe drawables toe, elke drawable dat je toevoegt blijft permanent in het venster totdat
     * je het verwijdert met {@link #removeDrawable}.
     */
    public void addDrawables(Iterable<Drawable> cells) {
        cells.forEach(this.drawables::add);
    }

    /** Verwijder een drawable */
    public void removeDrawable(Drawable drawable) {
        this.drawables.remove(drawable);
    }

    /** Verwijder alle drawables */
    public void clear() {
        this.drawables.clear();
    }

}
