package org.plt.types;

public class VoidObject {
    static public VoidObject VOID = new VoidObject();
    private VoidObject() {}
    public String toString() {
	return "";
    }
}