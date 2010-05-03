package com.phonegap.demo;
/* License (MIT)
 * Copyright (c) 2008 Nitobi
 * website: http://phonegap.com
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * Software), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

import java.lang.reflect.Field;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.res.Configuration;
import android.os.Bundle;
import android.os.Handler;
import android.util.Log;
import android.view.Window;
import android.view.WindowManager;
import android.webkit.JsResult;
import android.webkit.WebChromeClient;
import android.webkit.WebView;
import android.webkit.WebSettings;
import android.content.Intent;


import plt.playlist.PlaylistRecord;
import plt.playlist.PlaylistPlayer;


public class DroidGap extends Activity {
    static final int PLAYLIST_PICKED = 8024;


	
    private static final String LOG_TAG = "DroidGap";
    private WebView appView;
    private String uri;

    private PhoneGap gap;
    private GeoBroker geo;
    private AccelListener accel;
    private ConsoleOutput console;

    private ArgTable arguments;
    private PlaylistPicker playlistPicker;
	

    private Handler handler = new Handler();


    /** Called when the activity is first created. */
    @Override
	public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        getWindow().requestFeature(Window.FEATURE_NO_TITLE); 
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FORCE_NOT_FULLSCREEN,
			     WindowManager.LayoutParams.FLAG_FORCE_NOT_FULLSCREEN); 
        setContentView(R.layout.main);        
         
        appView = (WebView) findViewById(R.id.appView);
        
        /* This changes the setWebChromeClient to log alerts to LogCat!  Important for Javascript Debugging */
        
        appView.setWebChromeClient(new GapClient(this));
	appView.getSettings().setCacheMode(WebSettings.LOAD_CACHE_ELSE_NETWORK);
	appView.getSettings().setJavaScriptEnabled(true);
        appView.getSettings().setJavaScriptCanOpenWindowsAutomatically(true);        
	appView.getSettings().setBlockNetworkImage(false);
	appView.getSettings().setLoadsImagesAutomatically(true);
	
        
        /* Bind the appView object to the gap class methods */
        bindBrowser(appView);
        
        /* Load a URI from the strings.xml file */
        Class<R.string> c = R.string.class;
        Field f;
        
        int i = 0;
        
        try {
	    f = c.getField("url");
	    i = f.getInt(f);
	    this.uri = this.getResources().getString(i);
        } catch (Exception e)
	    {
		this.uri = "http://www.phonegap.com";
	    }
	Log.d(LOG_TAG, "Loading the uri " + this.uri);

        appView.loadUrl(this.uri);        
	Log.d(LOG_TAG, "uri loaded");
    }
	

    @Override
	public void onConfigurationChanged(Configuration newConfig) {
	//don't reload the current page when the orientation is changed
	super.onConfigurationChanged(newConfig);
    } 
    
    private void bindBrowser(WebView appView)
    {
	Log.d(LOG_TAG, "Binding the browser");

    	// The PhoneGap class handles the Notification and Android Specific crap
	arguments = new ArgTable();
	Log.d(LOG_TAG, "Instantiating gap");
    	gap = new PhoneGap(this, this.handler, appView, getAssets(), arguments);
	Log.d(LOG_TAG, "Instantiating geo");
    	geo = new GeoBroker(this, appView, arguments);
	Log.d(LOG_TAG, "Instantiating accel");
    	accel = new AccelListener(this, appView, arguments);
	Log.d(LOG_TAG, "Instantiating console");
    	console = new ConsoleOutput(this, appView);
	Log.d(LOG_TAG, "Instantiating playlistpicker");
	playlistPicker = new PlaylistPicker();

	Log.d(LOG_TAG, "Adding interfaces");
    	// This creates the new javascript interfaces for PhoneGap
    	appView.addJavascriptInterface(gap, "Device");
    	appView.addJavascriptInterface(geo, "Geo");
    	appView.addJavascriptInterface(accel, "Accel");
	appView.addJavascriptInterface(console, "Console");
	appView.addJavascriptInterface(arguments, "Args");
	appView.addJavascriptInterface(playlistPicker, "PlaylistPicker");
	Log.d(LOG_TAG, "Bound the browser");
    }


    public void onPause() {
	System.out.println("Being paused");
	gap.onPause();
	geo.onPause();
	accel.onPause();
	super.onPause();
    }


    public void onResume() {
	System.out.println("Being resumed");
	gap.onResume();
	geo.onResume();
	accel.onResume();
	super.onResume();
    }



    public void onStop() {
    	System.out.println("Being stopping");
    	gap.onStop();
    	geo.onStop();
    	accel.onStop();
    	super.onStop();
    }


    public void onRestart() {
	System.out.println("Being restarted");
	gap.onRestart();
	geo.onRestart();
	accel.onRestart();
	super.onRestart();
    }




    public void onDestroy() {
	System.out.println("Being destroyed");
	appView.destroy();

	gap.onDestroy();
	geo.onDestroy();
	accel.onDestroy();
	super.onDestroy();
    }


    public Handler getHandler() {
	return this.handler;
    }

        
    /**
     * Provides a hook for calling "alert" from javascript. Useful for
     * debugging your javascript.
     */
    final class GapClient extends WebChromeClient {
    	
    	Context mCtx;
    	GapClient(Context ctx)
    	{
	    mCtx = ctx;
    	}
    	
	void onConsoleMessage(String message, int lineNumber, String sourceId) {
	    String singleMessage = ("javascript: " + 
				    sourceId + " : " +
				    lineNumber + " : " + 
				    message);
            AlertDialog.Builder alertBldr = new AlertDialog.Builder(mCtx);
            alertBldr.setMessage(singleMessage);
            alertBldr.setTitle("Javascript Error");
            alertBldr.show();
	    System.out.println(singleMessage);
	}

    	@Override
	    public boolean onJsAlert(WebView view, String url, String message, JsResult result) {
            Log.d(LOG_TAG, message);
            // This shows the dialog box.  This can be commented out for dev
            AlertDialog.Builder alertBldr = new AlertDialog.Builder(mCtx);
            alertBldr.setMessage(message);
            alertBldr.setTitle("Alert");
            alertBldr.show();
            result.confirm();
            return true;
        }
    }




    public class PlaylistPicker {
	private PlaylistRecord record;
	public PlaylistPicker() {}

	public void requestPickPlaylist() {
	    bindPlaylistPicker();
	}
	
	public PlaylistRecord getPlaylistRecord() {
	    return this.record;
	}

	public void setPlaylistRecord(PlaylistRecord record) {
	    this.record = record;
	}

    }


    private void bindPlaylistPicker() {
	Intent intent = new Intent();
	intent.setClass(this, plt.playlist.PickPlaylist.class);
	this.startActivityForResult(intent, PLAYLIST_PICKED);
    }



    protected void onActivityResult(int requestCode, 
				    int resultCode,
				    Intent intent) {
	if (resultCode == RESULT_OK) {
	    switch (requestCode) {
	    case PLAYLIST_PICKED:
		System.out.println("I got a playlist selected.");
		plt.playlist.PlaylistRecord record = 
		    (plt.playlist.PlaylistRecord) intent.getSerializableExtra("value");
		playlistPicker.setPlaylistRecord(record);
		appView.loadUrl("javascript:navigator.dialogPickers.notifyPlaylistPicked()");

		// Just to see that we can actually play.
		// 		PlaylistPlayer player = new PlaylistPlayer(this, this.handler, record);
		// 		player.play();


		break;
	    default:
		appView.loadUrl("javascript:navigator.dialogPickers.notifyPlaylistCanceled()");
	    };
	} else {
	}
    }
}
