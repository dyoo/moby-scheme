package org.plt.platform;

public class J2MEPlatform implements PlatformI {
    // debug
    static {
	System.out.println("J2ME Platform");
    }

    public String getName() {
	return "J2ME";
    }
}