package com.phonegap.demo;

import android.content.Context;
import android.webkit.WebView;
import android.util.Log;

/**
 * class designed to allow phonegap to print to the console
 * using the print and println methods.
 */
public class ConsoleOutput {
	
	WebView mAppView;
	Context mCtx;
	
	public ConsoleOutput(Context ctx, WebView appView)
	{
		mCtx = ctx;
		mAppView = appView;
	}

	public void print(String msg) {
		System.out.print(msg);
	}

	public void println(String msg) {
		System.out.println(msg);
	}

	public void logd(String tag, String msg) {
		Log.d(tag, msg);
	}
}
