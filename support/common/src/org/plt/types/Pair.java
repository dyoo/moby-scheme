package org.plt.types;

public class Pair implements List {
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

	public boolean isEmpty() {
		return false;
	}

	public boolean equals(Object other) {
		if ((other instanceof org.plt.types.List) == false)
			return false;

		if (this.isEmpty() && ((org.plt.types.List) other).isEmpty())
			return true;

		if (this.isEmpty() || ((org.plt.types.List) other).isEmpty())
			return false;

		if (this.first().equals(((org.plt.types.List) other).first()) == false)
			return false;

		return (this.rest().equals(((org.plt.types.List) other).rest()));
	}
}
