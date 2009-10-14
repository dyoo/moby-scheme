package com.phonegap.demo;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.BufferedInputStream;
import java.io.FileOutputStream;
import java.io.BufferedOutputStream;
import java.net.URLConnection;
import java.net.URL;
import java.util.Map;
import java.util.HashMap;

import android.os.Handler;


import android.app.Activity;
import android.content.Context;
import android.content.res.AssetManager;
import android.content.res.AssetFileDescriptor;
import android.media.AudioManager;
import android.media.MediaPlayer;
import android.media.MediaPlayer.OnErrorListener;
import android.media.MediaRecorder;
import android.media.MediaPlayer.OnBufferingUpdateListener;
import android.media.MediaPlayer.OnCompletionListener;
import android.media.MediaPlayer.OnPreparedListener;
import android.util.Log;
import android.webkit.WebView;

import plt.playlist.PlaylistRecord;
import plt.playlist.PlaylistPlayer;

public class AudioHandler implements OnCompletionListener, 
				     OnPreparedListener,
				     OnErrorListener {
    private MediaRecorder recorder;
    private boolean isRecording = false;
    //	MediaPlayer mPlayer;
    private boolean isPlaying = false;
    private String recording;
    private String saveFile;
    private Activity mCtx;
    private Handler handler;
    private WebView mAppView;
    private ArgTable arguments;

    private HashMap<String, MPlayerStatus> mPlayers_file;
    private HashMap<MediaPlayer, MPlayerStatus> mPlayers_player;


    private Map<Long, PlaylistPlayer> playlistPlayers;

    //	private boolean isPaused = false;
    private AssetManager assets;
    //	private String curPlaying = null;
    private AudioManager volumeControl;
	
    private static final int MUSIC_STREAM = AudioManager.STREAM_MUSIC;

    private class MPlayerStatus {
	public String file;
	public MediaPlayer player;
	public boolean isPaused = false;
	public boolean isPlaying = false;

	public MPlayerStatus(String theFile, MediaPlayer thePlayer) {
	    file = theFile;
	    player = thePlayer;
	}
    }
	
    public AudioHandler(String file, 
			Activity ctx,
			Handler handler,
			WebView appView,
			AssetManager assets,
			ArgTable args) {
	//		this.recording = file;
	this.mCtx = ctx;
	this.handler = handler;
	this.mAppView = appView;
	this.assets = assets;
	this.arguments = args;
	volumeControl = (AudioManager) mCtx.getSystemService(Context.AUDIO_SERVICE);
	mPlayers_file = new HashMap<String, MPlayerStatus>();
	mPlayers_player = new HashMap<MediaPlayer, MPlayerStatus>();

	this.playlistPlayers = new HashMap<Long, PlaylistPlayer>();
    }
	
    /*
      protected void startRecording(String file){
      if (!isRecording){
      saveFile=file;
      recorder = new MediaRecorder();
      recorder.setAudioSource(MediaRecorder.AudioSource.MIC);
      recorder.setOutputFormat(MediaRecorder.OutputFormat.THREE_GPP);
      recorder.setAudioEncoder(MediaRecorder.AudioEncoder.AMR_NB);
      recorder.setOutputFile(this.recording);
      try {
      recorder.prepare();
      } catch (IllegalStateException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
      //			} catch (IOException e) {
      //				// TODO Auto-generated catch block
      //				e.printStackTrace();
      }
      isRecording = true;
      recorder.start();
      }
      }
	
      private void moveFile(String file) {
      // this is a hack to save the file as the specified name
      File f = new File (this.recording);
      f.renameTo(new File("/sdcard" + file));
      }
	
      protected void stopRecording(){
      try{
      if((recorder != null)&&(isRecording)) {
      isRecording = false;
      recorder.stop();
      recorder.release(); 
      }
      moveFile(saveFile);
      }catch (Exception e){e.printStackTrace();}
      }
    */	
	
    private File readStreamedUrl(String url) {
	try {
	    URLConnection cn = new URL(url).openConnection();
	    cn.connect();
	    BufferedInputStream in = new BufferedInputStream(cn.getInputStream());
	    // The extension actually matters to the media player.
	    File f = File.createTempFile("play", url.substring(url.length()-4));
	    BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(f));
	    int ch;
	    while ((ch = in.read()) != -1) {
		out.write(ch);
	    }
	    in.close();
	    out.close();
	    return f;
	} catch (IOException e) {
	    throw new RuntimeException(e);
	}
    }


    public void playPlaylistRecord(PlaylistRecord record) {
	if (!playlistPlayers.containsKey(record.getId())) {
	    PlaylistPlayer player = new PlaylistPlayer(this.mCtx, 
						       this.handler,
						       record);
	    playlistPlayers.put(record.getId(), player);
	}

	PlaylistPlayer player = playlistPlayers.get(record.getId());
	player.play();
	
    }

    public void pausePlaylistRecord(PlaylistRecord record) {
	if (playlistPlayers.containsKey(record.getId())) {
	    PlaylistPlayer player = playlistPlayers.get(record.getId());
	    player.pause();
	}

    }

    public void stopPlaylistRecord(PlaylistRecord record) {
	if (playlistPlayers.containsKey(record.getId())) {
	    PlaylistPlayer player = playlistPlayers.get(record.getId());
	    player.stop();
	}
    }


    


    protected void startPlaying(String file) {
	MPlayerStatus status = mPlayers_file.get(file);
	if ( status == null ) {
	    try {
		AssetFileDescriptor fileAsset = getAssetFileDesc(file);
				
		MediaPlayer mPlayer = new MediaPlayer();
		mPlayer.setOnPreparedListener(this);
		status = new MPlayerStatus(file, mPlayer);
		mPlayers_file.put(file, status);
		mPlayers_player.put(mPlayer, status);
		Log.d("Audio startPlaying", "audio: " + file);

		if (isStreaming(file))
		    {
			File f = readStreamedUrl(file);
			Log.d("AudioStartPlaying", "Streaming");
			// Streaming prepare async
			mPlayer.setDataSource(f.getAbsolutePath());
			mPlayer.setAudioStreamType(MUSIC_STREAM);  
			mPlayer.prepare();
		    } else {
		    Log.d("AudioStartPlaying", "File");
		    // Not streaming prepare synchronous, abstract base directory
		    if (fileAsset == null) {
			mPlayer.setDataSource(file);
		    }
		    else {
			mPlayer.setDataSource(fileAsset.getFileDescriptor(),
					      fileAsset.getStartOffset(),
					      fileAsset.getLength());
		    }
		    mPlayer.prepare();
		}

		status.isPlaying = true;
	    } catch (Exception e) { e.printStackTrace(); }
			
	}
	else if ( !status.isPlaying ) {
	    try {
			    
		status.player.prepare();
		status.player.start();
		status.isPlaying = true;
		status.isPaused = false;
	    } catch (Exception e) { e.printStackTrace(); }
	}
	// Otherwise check to see if it's paused, if it is, resume
	else if ( status.isPaused ) {
	    status.player.start();
	    status.isPaused = false;
	}
    }

    private AssetFileDescriptor getAssetFileDesc(String file) {
	if ( !file.startsWith("file:///android_asset/") ) {
	    return null;
	}
		
	try {
	    String filePath = file.substring(22);
	    return assets.openFd(filePath);
	} catch (IOException e) {
	    e.printStackTrace();
	    return null;
	}
    }
	
    protected void pausePlaying(String file) {
	MPlayerStatus status = mPlayers_file.get(file);
	if ( status != null && status.isPlaying && !status.isPaused ) {
	    status.player.pause();
	    status.isPaused = true;
	}
    }

    protected void resumePlaying(String file) {
	MPlayerStatus status = mPlayers_file.get(file);
	if ( status != null && status.isPaused ) {
	    status.player.start();
	    status.isPaused = false;
	}
    }

    protected void stopPlaying(String file) {
	//		System.out.println("stopPlaying called");

	MPlayerStatus status = mPlayers_file.get(file);
	if ( status != null ) {
	    status.player.stop();
	    status.isPlaying = false;
	    status.isPaused = false;
	}
    }
	
    public void onCompletion(MediaPlayer mPlayer) {
	//		System.out.println("onCompletion called");
	MPlayerStatus status = mPlayers_player.get(mPlayer);

	mPlayer.stop();
	status.isPlaying = false;
	status.isPaused = false;

	arguments.put("finishedMusicFile", status.file);
		
	//		String escapedFile = status.file.replaceAll("'", "\\\\'");
	mAppView.loadUrl("javascript:navigator.audio.musicFinished()");

	//		System.out.println("Called musicFinished on " + escapedFile);
    }

    public void stopAllPlaying() {
	//		System.out.println("stopAllPlaying called");

	for ( MPlayerStatus status : mPlayers_file.values() ) {
	    status.player.stop();
	    status.isPlaying = false;
	    status.isPaused = false;
	}

	for (Long id : playlistPlayers.keySet()) {
	    playlistPlayers.get(id).stop();
	}
    }

    public void clearCache() {
	//		System.out.println("clearCache called");

	for (MediaPlayer player : mPlayers_player.keySet()) {
	    player.stop();
	    player.release();
	}
	mPlayers_file.clear();
	mPlayers_player.clear();
    }

    public void increaseVolume(int flags) {
	volumeControl.adjustStreamVolume(MUSIC_STREAM,
					 AudioManager.ADJUST_RAISE,
					 flags);
    }

    public void decreaseVolume(int flags) {
	volumeControl.adjustStreamVolume(MUSIC_STREAM,
					 AudioManager.ADJUST_LOWER,
					 flags);
    }

    public boolean setVolume(int percent, int flags) {
	if (percent < 0 || percent > 100)
	    return false;

	int volIndex = percent * volumeControl.getStreamMaxVolume(MUSIC_STREAM) / 100;
	volumeControl.setStreamVolume(MUSIC_STREAM, volIndex, flags);
	return true;
    }
	
    protected long getCurrentPosition(String file) {
	MediaPlayer mPlayer = mPlayers_file.get(file).player;
	if (mPlayer != null) 
	    {
		return(mPlayer.getCurrentPosition());
	    } else { return(-1); }
    }
	
    private boolean isStreaming(String file) 
    {
	if (file.contains("http://")) {
	    return true;
	} else {
	    return false;
	}
    }
	
    protected long getDuration(String file) {
	long duration = -2;
	if (!mPlayers_file.containsKey(file) & !isStreaming(file)) {
	    try {
		AssetFileDescriptor fileAsset = getAssetFileDesc(file);
		MediaPlayer mPlayer = new MediaPlayer();
		if (fileAsset == null) {
		    mPlayer.setDataSource(file);
		}
		else {
		    mPlayer.setDataSource(fileAsset.getFileDescriptor());
		}
		mPlayer.prepare();
		duration = mPlayer.getDuration();
		mPlayer.release();
	    } catch (Exception e) { e.printStackTrace(); return(-3); }
	} else if (mPlayers_file.containsKey(file)) {
	    try {
		duration = mPlayers_file.get(file).player.getDuration();
	    } catch (Exception e) { e.printStackTrace(); return(-4); }
	} else { return -1; }
	return duration;
    }

    public void onPrepared(MediaPlayer mPlayer) {
	if ( mPlayers_player.containsKey(mPlayer) ) {
	    mPlayer.setOnCompletionListener(this);
	    mPlayer.setOnBufferingUpdateListener(new OnBufferingUpdateListener()
		{
		    public void onBufferingUpdate(MediaPlayer mPlayer, int percent)
		    {
			/* TODO: call back, e.g. update outer progress bar */
			Log.d("AudioOnBufferingUpdate", "percent: " + percent); 
		    }
		});
	    mPlayer.start();
	}
    }

    public boolean onError(MediaPlayer mPlayer, int arg1, int arg2) {
	Log.e("AUDIO onError", "error " + arg1 + " " + arg2);
	return false;
    }
	
    protected void setAudioOutputDevice(int output){
	// Changes the default audio output device to speaker or earpiece 
	AudioManager audiMgr = (AudioManager) mCtx.getSystemService(Context.AUDIO_SERVICE);
	if (output == (2))
	    audiMgr.setRouting(AudioManager.MODE_NORMAL, AudioManager.ROUTE_SPEAKER, AudioManager.ROUTE_ALL);
	else if (output == (1)){
	    audiMgr.setRouting(AudioManager.MODE_NORMAL, AudioManager.ROUTE_EARPIECE, AudioManager.ROUTE_ALL);
	}else
	    Log.e("AudioHandler setAudioOutputDevice", " unknown output device");	
    }
	
    protected int getAudioOutputDevice(){
	AudioManager audiMgr = (AudioManager) mCtx.getSystemService(Context.AUDIO_SERVICE);
	if (audiMgr.getRouting(AudioManager.MODE_NORMAL) == AudioManager.ROUTE_EARPIECE)
	    return 1;
	else if (audiMgr.getRouting(AudioManager.MODE_NORMAL) == AudioManager.ROUTE_SPEAKER)
	    return 2;
	else
	    return -1;
    }
}
