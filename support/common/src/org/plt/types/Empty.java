package org.plt.types;

import org.plt.SchemeException;

public class Empty implements List {
    private Empty() {}

    static public Empty EMPTY = new Empty();

    public Object first() {
	throw new SchemeException("Can't take first on empty");
    }
    public List rest() {
	throw new SchemeException("Cant' take rest on empty");
    }

    public boolean isEmpty() { return true; }
}
