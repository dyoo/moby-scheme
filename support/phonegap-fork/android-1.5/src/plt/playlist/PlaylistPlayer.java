package plt.playlist;


import android.media.MediaPlayer;
import android.media.AudioManager;
import android.media.MediaPlayer.OnPreparedListener;
import android.media.MediaPlayer.OnCompletionListener;
import android.net.Uri;
import android.os.Handler;
import android.os.SystemClock;
import android.app.Activity;

import java.io.IOException;
import java.util.List;

import android.util.Log;





// Given a playlist, plays music.

// Needs a handler to queue its stuff.  MediaPlayer is supposed to be
// finicky about running in the UI thread, so all of our operations
// must run from the handler.


// FIXME: currently restricted to play the first song in the playlist over
// and over.  We need to fix this!


public class PlaylistPlayer {
    private MediaPlayer mediaPlayer;
    private Handler handler;
    private Activity activity;

    private int currentSongIndex;
    private List<Uri> songs;

    private boolean isPlaying;

    public PlaylistPlayer(final Activity activity,
			  Handler handler,
			  final PlaylistRecord record) {

	Log.d("PlaylistPlayer", "Constructing player");

	final PlaylistPlayer that = this;
	this.activity = activity;
	this.handler = handler;
	this.handler.post(new Runnable() { public void run() { 
	    that.songs = record.getSongUris(activity);
	    that.currentSongIndex = 0;
	    that.isPlaying = false;
	}});

    }


    // The following methods will queue up a sequence of songs to play.
    public void play() {
	final PlaylistPlayer that = this;

	this.handler.post(new Runnable() {
		public void run() {
		    that.isPlaying = true;
		    try {
			if (that.mediaPlayer == null) {
			    that.mediaPlayer = new MediaPlayer();
			    that.mediaPlayer.setLooping(true);
			    // that.mediaPlayer.setOnCompletionListener
// 				(new OnCompletionListener() {
// 					public void onCompletion(final MediaPlayer mp) {
// 					    mp.release();
// 					    that.mediaPlayer = null;
// 					    that.currentSongIndex = 
// 						(that.currentSongIndex + 1) %
// 						that.songs.size();
// 					    if (that.isPlaying) {
// 						that.play();
// 					    }
// 					}
// 				    });
			    that.mediaPlayer.setDataSource
				(that.activity,
				 that.songs.get(that.currentSongIndex));
			    that.mediaPlayer.setAudioStreamType(AudioManager.STREAM_MUSIC);
			    that.mediaPlayer.prepare();
			    that.mediaPlayer.start();
			} else {
			    that.mediaPlayer.start();
			}
		    } catch (IOException e) {
			e.printStackTrace();
		    }
		}
	    });
    }
    


    public void pause() {
	final PlaylistPlayer that = this;
	this.handler.post(new Runnable() {
		public void run() {
		    that.isPlaying = false;
		    if (that.mediaPlayer != null) {
			that.mediaPlayer.pause();
		    }
		}
	    });
    }


    public void stop() {
	final PlaylistPlayer that = this;
	this.handler.post(new Runnable() {
		public void run() {
		    that.isPlaying = false;
		    if (that.mediaPlayer != null) {
			that.mediaPlayer.release();
			that.mediaPlayer = null;
		    }
		}
	    });
    }
}
