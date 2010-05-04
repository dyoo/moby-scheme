goog.provide('plt.world.effects');


// Hooks to define each of the side-effects of each Effect defined by
// moby/runtime/effect-struct.


(function() {

    var E = plt.Kernel.invokeModule("moby/runtime/effect-struct").EXPORTS;

    var effect_colon_none =
	E['effect:none'];

    var effect_colon_beep =
	E['effect:beep'];

    var effect_colon_play_dash_dtmf_dash_tone =
	E['effect:play-dtmf-tone'];

    var effect_colon_send_dash_sms = 
	E['effect:send-sms'];

    var effect_colon_play_dash_sound = 
	E['effect:play-sound'];

    var effect_colon_pause_dash_sound = 
	E['effect:pause-sound'];

    var effect_colon_stop_dash_sound = 
	E['effect:stop-sound'];

    var effect_colon_set_dash_sound_dash_volume = 
	E['effect:set-sound-volume'];

    var effect_colon_raise_dash_sound_dash_volume = 
	E['effect:raise-sound-volume'];

    var effect_colon_lower_dash_sound_dash_volume = 
	E['effect:lower-sound-volume'];

    var effect_colon_set_dash_wake_dash_lock = 
	E['effect:set-wake-lock'];

    var effect_colon_release_dash_wake_dash_lock = 
	E['effect:release-wake-lock'];

    var effect_colon_pick_dash_playlist = 
	E['effect:pick-playlist'];

    var effect_colon_pick_dash_random = 
	E['effect:pick-random'];



    // The rest of the code here extends the structure type of each
    // effect with a run() method.  This is used in plt.world.Kernel.applyEffect.

    effect_colon_none.prototype.run = function() {
	// Do nothing.
    };

    effect_colon_beep.prototype.run = function() {
	plt.platform.Platform.getInstance().getSoundService().beep();
    };

    effect_colon_play_dash_dtmf_dash_tone.prototype.run = function() {
	plt.platform.Platform.getInstance().getSoundService().playDtmfTone(
	    plt.types.NumberTower.toFixnum(this._fields[0]),
	    plt.types.NumberTower.toFixnum(this._fields[1]));
    };

    effect_colon_send_dash_sms.prototype.run = function() {
	plt.platform.Platform.getInstance().getSmsService().send(
	    this._fields[0], this._fields[1]);
    };

    effect_colon_play_dash_sound.prototype.run = function() {
	if (plt.Kernel.isString(this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().playSoundUrl(
		this._fields[0]);
	} else if (playlist_dash_sound_question_(this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().playPlaylist(
		this._fields[0]._fields[0]);
	}
    };

    effect_colon_pause_dash_sound.prototype.run = function() {
	if (plt.Kernel.isString(this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().pauseSoundUrl(
		this._fields[0]);
	} else if (playlist_dash_sound_question_(this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().pausePlaylist(
		this._fields[0]._fields[0]);
	}
    };

    effect_colon_stop_dash_sound.prototype.run = function() {
	if (plt.Kernel.isString(this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().stopSoundUrl(
		this._fields[0]);
	} else if (playlist_dash_sound_question_(this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().stopPlaylist(
		this._fields[0]._fields[0]);
	}
    };

    effect_colon_set_dash_sound_dash_volume.prototype.run = function() {
	plt.platform.Platform.getInstance().getSoundService().setVolume(
	    plt.types.NumberTower.toFixnum(this._fields[0]));
    };

    E['effect:set-beep-volume'].prototype.run = function() {
	plt.platform.Platform.getInstance().getSoundService().setRingerVolume(
	    plt.types.NumberTower.toFixnum(this._fields[0]));
    };

    effect_colon_raise_dash_sound_dash_volume.prototype.run = function() {
	plt.platform.Platform.getInstance().getSoundService.raiseVolume();
    };

    effect_colon_lower_dash_sound_dash_volume.prototype.run = function() {
	plt.platform.Platform.getInstance().getSoundService.lowerVolume();
    };

    effect_colon_set_dash_wake_dash_lock.prototype.run = function() {
	plt.platform.Platform.getInstance().getPowerService().setWakeLock(
	    plt.types.NumberTower.toFixnum(this._fields[0]));
    };

    effect_colon_release_dash_wake_dash_lock.prototype.run = function() {
	plt.platform.Platform.getInstance().getPowerService().releaseWakeLock();
    };

    effect_colon_pick_dash_playlist.prototype.run = function() {
	var updater = this._fields[0];
	var callback = function(playlist) {
	    var playlistSound = make_dash_playlist_dash_sound(playlist);
	    setTimeout(function() {
		var changeWorld = plt.world.config.CONFIG.lookup("changeWorld");
		changeWorld(function(w) {
		    return updater([w, playlistSound]);
		});
	    }, 0);
	}
	plt.platform.Platform.getInstance().getPickPlaylistService().pickPlaylist(callback);
    };

    effect_colon_pick_dash_random.prototype.run = function() {
	var aRandomNumber =
	    plt.types.Rational.makeInstance(
		Math.floor(plt.types.NumberTower.toFixnum(this._fields[0]) * 
			   Math.random()),
		1);
	var callback = this._fields[1];
	return function(w) { return callback([w, aRandomNumber]) }
    };

}());