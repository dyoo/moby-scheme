package org.plt.platform;

public class AndroidPlatform implements PlatformI {
    // debug
    static {
	System.out.println("Android Platform");
    }



    public String getName() {
	return "Android";
    }
}