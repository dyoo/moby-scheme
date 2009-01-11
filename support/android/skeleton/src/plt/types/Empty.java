package plt.types;
public class Empty implements List {
    private Empty() {}

    static public Empty EMPTY = new Empty();

    public Object first() {
	throw new RuntimeException();
    }
    public List rest() {
	throw new RuntimeException();
    }

    public boolean isEmpty() { return true; }
}
