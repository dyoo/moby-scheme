package org.plt.lib;

import org.plt.types.*;
import org.plt.platform.Platform;

public class Sms {
    // Fixme: consuming the world is not quite right, but 
    // we're working under Beginner Scheme Level, where every
    // function must return a good value.
    public static Object sendTextMessage(Object address,
					 Object message,
					 Object world) {
	SmsService service = 
	    Platform.getInstance().getSmsService();
	service.sendTextMessage(address.toString(), message.toString());
	return world;
    }
}
