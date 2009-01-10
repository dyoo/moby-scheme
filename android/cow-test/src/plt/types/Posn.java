package plt.types;

public class Posn implements Struct {
    private Object x, y;
    public Posn(Object x, Object y) {
	this.x = x;
	this.y = y;
    }
    public Object getX() {
	return this.x;
    }
    public Object getY() {
	return this.y;
    }
}
