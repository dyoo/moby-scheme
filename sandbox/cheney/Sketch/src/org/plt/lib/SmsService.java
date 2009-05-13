package org.plt.lib;

public interface SmsService {
    void sendTextMessage(String destinationString,
			 String msg);
}