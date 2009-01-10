package plt.types;
public class Pair implements List{
    private Object f;
    private List r;

    public Pair(Object first, List rest) {
	this.f = first;
	this.r = rest;
    }

    public Object first() {
	return this.f;
    }
    public List rest() {
	return this.r;
    }
    public boolean isEmpty() { return false; }
}
