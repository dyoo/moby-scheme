package org.plt;

public class SchemeException extends RuntimeException {
	public SchemeException(String msg) {
		super(msg);
	}

	public SchemeException(Throwable t) {
		super(t.toString());
	}
}