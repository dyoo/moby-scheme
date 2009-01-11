package plt.types;

// Fixme: Symbols should be kept in a hashtable so we can do quick
// pointer equality.

public class Symbol {

    private String content;

    private Symbol(String content) {
	this.content = content;
    }

    static public Symbol makeInstance(String content) {
	return new Symbol(content);
    }

    public String toString() {
 	return content;
    }

    public boolean equals(Object _other) {
	if (_other instanceof Symbol) {
	    Symbol other = (Symbol) _other;
	    return this.content.equals(other.content);
	} else {
	    return false;
	}
    }
}
