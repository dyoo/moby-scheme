package com.phonegap.demo;

import android.media.AudioManager;
import android.media.ToneGenerator;

public class ToneHandler {

	private ToneGenerator generator;
	private Integer nowPlaying = null;

	private static final int[] TONE_VALUES =
		{ToneGenerator.TONE_DTMF_1, ToneGenerator.TONE_DTMF_2, ToneGenerator.TONE_DTMF_3, ToneGenerator.TONE_DTMF_A,
		 ToneGenerator.TONE_DTMF_4, ToneGenerator.TONE_DTMF_5, ToneGenerator.TONE_DTMF_6, ToneGenerator.TONE_DTMF_B,
		 ToneGenerator.TONE_DTMF_7, ToneGenerator.TONE_DTMF_8, ToneGenerator.TONE_DTMF_9, ToneGenerator.TONE_DTMF_C,
		 ToneGenerator.TONE_DTMF_S, ToneGenerator.TONE_DTMF_0, ToneGenerator.TONE_DTMF_P, ToneGenerator.TONE_DTMF_D};

	public ToneHandler() {
		generator = new ToneGenerator(AudioManager.STREAM_RING, ToneGenerator.MAX_VOLUME);
	}

	public void playDTMF(int tone) {
		// if already playing the specified tone, then do nothing
		if (nowPlaying != null && tone == nowPlaying.intValue()) {
			return;
		}
	 	
		if (tone >= 0 && tone < TONE_VALUES.length) {
			generator.startTone(TONE_VALUES[tone]);
			nowPlaying = tone;
		}
		else {
			throw new NoSuchToneException(tone);
		}
	}

	public void stopDTMF() {
		nowPlaying = null;
		generator.stopTone();
	}
}
