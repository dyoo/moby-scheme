package com.phonegap.demo;


// Class for listening to SMS-receive intents and sending such events
// to other listeners.


// NOTE: you must have the right permissions to use this class.
// You need the android.permission.RECEIVE_SMS permission.


import java.util.List;
import java.util.ArrayList;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;

// FIXME: when we move to more recent Android, use android.telephony.SmsMessage
// since the other class is deprecated.
import android.telephony.gsm.SmsMessage;


public class SmsListener extends BroadcastReceiver implements LifecycleService
{

    // An interface for people who want to be notified when a message comes in.
    public interface OnSmsMessageReceive {
	void onSmsMessageReceive(String sender, String msg);
    }

    private static String ACTION = "android.provider.Telephony.SMS_RECEIVED"; 


    //////////////////////////////////////////////////////////////////////
    private Context ctx;
    private List<OnSmsMessageReceive> listeners;
    
    //////////////////////////////////////////////////////////////////////



    public SmsListener(Context ctx) {
	this.ctx = ctx;
	this.listeners = new ArrayList<OnSmsMessageReceive>();
    }


    /**
     * Add a new listener.
     */
    public void addListener(OnSmsMessageReceive listener) {
	this.listeners.add(listener);
    }


    /** Given a message, extract it and notify all our listeners that
     *  we got a new message.
     */
    @Override public void onReceive(Context ctx, Intent intent) {
	if (intent.getAction().equals(ACTION)) {
	    Bundle bundle = intent.getExtras();
	    if (bundle != null) {
		Object[] pdus = (Object[]) bundle.get("pdus");
		for(int i = 0; i < pdus.length; i++) {
		    SmsMessage msg = SmsMessage.createFromPdu((byte[])pdus[i]);
		    // maybe use getDisplayOriginationAddress instead?
		    String sender = msg.getOriginatingAddress();
		    String message = msg.getMessageBody();
		    for (OnSmsMessageReceive listener: this.listeners) {
			listener.onSmsMessageReceive(sender, message);
		    }
		}
	    }
	}
	
    }


    //////////////////////////////////////////////////////////////////////

    /**
     * Start listening.
     */
    public void onStart() {
	ctx.registerReceiver(this, new IntentFilter(ACTION));;
    }

    public void onResume() {
	ctx.registerReceiver(this, new IntentFilter(ACTION));;
    }

    public void onPause() {
	ctx.unregisterReceiver(this);
    }

    public void onStop() {
	ctx.unregisterReceiver(this);
    }

    public void onRestart() {
	ctx.registerReceiver(this, new IntentFilter(ACTION));;
    }

    public void onDestroy() {
	ctx.unregisterReceiver(this);
    }
}
