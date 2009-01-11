package plt.types;
public class Logic {
    boolean val;
    public static Logic TRUE = new Logic(true);    
    public static Logic FALSE = new Logic(false);

    private Logic(boolean val) {
	this.val = val;
    }

    public boolean isTrue() {
	return val;
    }

    public boolean isFalse() {
	return !val;
    }

    public Logic negate() {
	if (this == TRUE) {
	    return FALSE;
	} else {
	    return TRUE;
	}
    }
}