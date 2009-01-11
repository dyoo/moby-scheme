package plt.gui;
import javax.microedition.lcdui.*;

public class NwRectanglePicture extends RectanglePicture {
    public NwRectanglePicture(int width, int height, 
			      String style,
			      Color color) {
	super(width, height, style, color);
	setPinhole(0, 0);
    }
}
