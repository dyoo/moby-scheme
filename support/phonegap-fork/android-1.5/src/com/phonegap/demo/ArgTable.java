package com.phonegap.demo;

import java.util.HashMap;

public class ArgTable {

	private HashMap<String, Object> args;

	public ArgTable() {
		args = new HashMap<String, Object>();
	}

	public Object get(String key) {
		return args.remove(key);
	}

	public void put(String key, Object val) {
		args.put(key, val);
	}

	public void clearCache() {
		args.clear();
	}
}

