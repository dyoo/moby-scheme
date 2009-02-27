import javax.microedition.midlet.*;
import javax.microedition.lcdui.*;

public class MovingBall extends MIDlet {

	private World world;

	protected void destroyApp(boolean arg0) throws MIDletStateChangeException {

	}

	protected void pauseApp() {
		world = null;
	}

	protected void startApp() throws MIDletStateChangeException {
		MyCanvas canvas = new MyCanvas(new World(new Integer(50),
				new Integer(0)), Display.getDisplay(this));
		Display.getDisplay(this).setCurrent(canvas);
		new Thread(canvas).start();
	}
}

class MyCanvas extends Canvas implements Runnable {
	private World world;
	private Display g;

	public MyCanvas(World w, Display g) {
		world = w;
		this.g = g;
	}

	public void run() {
		while (true) {
			world = world.move(new Integer(getWidth()));
			repaint();
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}

	public void paint(Graphics g) {
		g.setGrayScale(255);
		g.fillRect(0, 0, getWidth(), getHeight());
		g.setColor(0x00ff0000);

		g.fillArc(world.getX().intValue(), getHeight() / 2
				- world.getRadius().intValue(),
				world.getRadius().intValue() * 2,
				world.getRadius().intValue() * 2, 0, 360);
	}
}

class World {
	private final Integer radius;
	private final Integer x;

	public World(Integer radius, Integer x) {
		this.radius = radius;
		this.x = x;
	}

	public World move(Integer width) {
		Integer newX = new Integer((x.intValue() + 3) % width.intValue());
		return new World(radius, newX);
	}

	public Integer getRadius() {
		return radius;
	}

	public Integer getX() {
		return x;
	}
}