package javax.microedition.media.control;

import javax.microedition.media.Control;

public interface ToneControl extends Control
{
	static byte	BLOCK_END = 0 ;
	static byte	BLOCK_START = 1; 
	static byte	C4 = 2; 
	static byte	PLAY_BLOCK = 3; 
	static byte	REPEAT = 4;
	static byte	RESOLUTION = 5;
	static byte	SET_VOLUME = 6;
	static byte	SILENCE = 7;
	static byte	TEMPO = 8;
	static byte	VERSION = 9;

	void setSequence( byte[] sequence );
}
