package com.phonegap.demo;

class NoSuchToneException extends RuntimeException {

//	public NoSuchToneException() {
//		super();
//	}

	public NoSuchToneException(int tone) {
		super("The tone " + tone + " does not exist.");
	}
}
