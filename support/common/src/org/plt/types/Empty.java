package org.plt.types;

import org.plt.checker.SchemeException;

public class Empty implements List {
	private Empty() {
	}

	static public Empty EMPTY = new Empty();

	public Object first() {
		throw new SchemeException("Can't take first on empty");
	}

	public List rest() {
		throw new SchemeException("Cant' take rest on empty");
	}

	public boolean isEmpty() {
		return true;
	}

	public boolean equals(Object other) {
		return (other instanceof org.plt.types.Empty);
	}
    public String toString() {
	return "()";
    }
}
