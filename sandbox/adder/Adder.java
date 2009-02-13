import javax.microedition.lcdui.*;
import javax.microedition.midlet.*;

public class Adder extends MIDlet implements CommandListener {
	// program state
	Integer val;

	// program ui
	Display display;
	Form form;
	StringItem label;
	static final Command pressCommand = new Command("Press me!", Command.OK, 2);

	// constructor.
	public Adder() {
	}

	/**
	 * Start the MIDlet by creating a list of items and associating the exit
	 * command with it.
	 */
	public void startApp() throws MIDletStateChangeException {
		val = new Integer(0);
		display = Display.getDisplay(this);
		label = new StringItem(null, val.toString());
		form = new Form("Adder");
		form.append(label);
		form.addCommand(pressCommand);
		form.setCommandListener(this);
		display.setCurrent(form);
	}

	public void pauseApp() {
		display = null;
		form = null;
		label = null;
	}

	public void destroyApp(boolean unconditional) {
		notifyDestroyed();
	}

	/**
	 * Handle events.
	 */
	public void commandAction(Command c, Displayable d) {
		val = new Integer(val.intValue() + 1);
		label.setText(val.toString());
	}
}
