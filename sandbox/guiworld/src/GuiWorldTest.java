import javax.microedition.lcdui.*;
import javax.microedition.midlet.*;

import org.plt.guiworld.*;
import org.plt.world.WorldTransformer;


public class GuiWorldTest extends MIDlet {
    private Object world;

    public GuiWorldTest() {
	this.world = new Integer(1);
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
		    // label
		    public Object transform(Object world) {
			return "The World says: " + world.toString();
		    }
		}),

	    // Button has not yet been implemented.
	    new Button(new WorldTransformer() {
		    // label
		    public Object transform(Object world) {
			return "The World says: " + world.toString();
		    }},
		       
		       // callback
		       new WorldTransformer() {
			   public Object transform(Object world) {
			       return new Integer(((Integer)world).intValue() + 1);
			   }})});


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
