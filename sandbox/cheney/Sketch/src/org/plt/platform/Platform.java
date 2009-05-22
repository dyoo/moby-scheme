package org.plt.platform;

import java.io.InputStream;
import java.io.IOException;


public class Platform {
    
    private static PlatformI instance;

    public static PlatformI getInstance() {
	return instance;
    }

    static {
    	try {
	    InputStream ins = 
		Class.forName("org.plt.platform.Platform").
		getResourceAsStream("/platform.implementation");
	    System.out.println("Reading in a line from " + ins);
 	    String platformImpl = readLine(ins);
 	    instance = (PlatformI) 
		Class.forName(platformImpl).newInstance();
	} catch(Exception ex) {
	    throw new RuntimeException
		("error loading platform.properties: " +
		 ex.toString());
	}
    }

    private static String readLine(InputStream in) {
	StringBuffer buf = new StringBuffer();
	while(true) {
	    try {
		int ch = in.read();
		if (ch == -1 || ch == '\n')
		    break;
		buf.append((char)ch);
	    } catch (IOException e) {
		break;
	    }
	}
	return buf.toString();
    }
}
