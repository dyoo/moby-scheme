import javax.microedition.lcdui.*;
import javax.microedition.midlet.*;

import org.plt.guiworld.*;
import org.plt.world.WorldTransformer;


public class GuiWorldTest extends MIDlet {
    private Object world;

    public GuiWorldTest() {
	this.world = "no world yet.";
    }

    public void startApp() throws MIDletStateChangeException {
	Display display = Display.getDisplay(this);

	Gui view = new Row(new Gui[] {
	    new Message(new WorldTransformer() {
		    public Object transform(Object world) {
			return "Hello world";
		    }
		}),


	    new Message(new WorldTransformer() {
		    public Object transform(Object world) {
			return "Goodbye world";
		    }
		}),
	});


	GuiRenderer guiRender = new GuiRenderer("title", world, view);
	Form form = guiRender.getForm();
	display.setCurrent(form);
    }

    public void pauseApp() {
    }

    public void destroyApp(boolean unconditional) {
	notifyDestroyed();
    }
}
