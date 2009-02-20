import javax.microedition.lcdui.*;
import javax.microedition.midlet.*;

public class CircleApp extends MIDlet implements CommandListener {

	// program state
	private Integer radius;
	private static final Integer max = new Integer(50);
	private static final Integer min = new Integer(20);

	// program ui
	private Display display;
	private Canvas cir;

	private static final Command smallCommand = new Command("smaller",
			Command.BACK, 0);
	private static final Command largeCommand = new Command("larger",
			Command.OK, 0);

	public CircleApp() {
	}

	protected void destroyApp(boolean arg0) throws MIDletStateChangeException {
		notifyDestroyed();
	}

	protected void pauseApp() {

	}

	protected void startApp() throws MIDletStateChangeException {
		display = Display.getDisplay(this);

		radius = min;
		cir = new Canvas() {
			protected void paint(Graphics g) {
				g.setGrayScale(255);
				g.fillRect(0, 0, getWidth(), getHeight());
				g.setColor(255, 0, 0);
				g.fillArc(getWidth() / 2 - radius.intValue(), getHeight() / 2
						- radius.intValue(), radius.intValue() * 2, radius
						.intValue() * 2, 0, 360);
			}
		};

		cir.addCommand(smallCommand);
		cir.addCommand(largeCommand);
		cir.setCommandListener(this);

		display.setCurrent(cir);
	}

	public void commandAction(Command c, Displayable d) {
		if (c.getLabel().equals("larger")) {
			radius = radius.intValue() == max.intValue() ? max : new Integer(
					radius.intValue() + 1);
		} else if (c.getLabel().equals("smaller")) {
			radius = radius.intValue() == min.intValue() ? min : new Integer(
					radius.intValue() - 1);
		}
		cir.repaint();
	}

}
