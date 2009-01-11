package javax.microedition.media.control;

import javax.microedition.media.Control;

public interface VolumeControl extends Control {

	int getLevel();
	
	boolean isMuted();
	
	void setLevel( int level );
	
	void setMute( boolean mute );
}
