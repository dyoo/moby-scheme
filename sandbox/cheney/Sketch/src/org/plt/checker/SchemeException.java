package org.plt.checker;

public class SchemeException extends RuntimeException {
	public SchemeException(String msg) {
		super(msg);
	}

	public SchemeException(Throwable t) {
		super(t.toString());
	}
}