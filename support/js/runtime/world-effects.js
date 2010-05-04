goog.provide('plt.world.effects');


// Hooks to define each of the side-effects of each Effect defined by
// moby/runtime/effect-struct.


(function() {

    var E = plt.Kernel.invokeModule("moby/runtime/effect-struct").EXPORTS;


    // The rest of the code here extends the structure type of each
    // effect with a run() method.  This is used in plt.world.Kernel.applyEffect.

    E['effect:none'].prototype.run = function() {
	// Do nothing.
    };

    E['effect:beep'].prototype.run = function() {
	plt.platform.Platform.getInstance().getSoundService().beep();
    };

    E['effect:play-dtmf-tone'].prototype.run = function() {
	plt.platform.Platform.getInstance().getSoundService().playDtmfTone(
	    plt.types.NumberTower.toFixnum(this._fields[0]),
	    plt.types.NumberTower.toFixnum(this._fields[1]));
    };

    E['effect:send-sms'].prototype.run = function() {
	plt.platform.Platform.getInstance().getSmsService().send(
	    this._fields[0], this._fields[1]);
    };

    E['effect:play-sound'].prototype.run = function() {
	if (plt.Kernel.isString(this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().playSoundUrl(
		this._fields[0]);
	} else if (E['playlist-sound?'](this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().playPlaylist(
		this._fields[0]._fields[0]);
	}
    };

    E['effect:pause-sound'].prototype.run = function() {
	if (plt.Kernel.isString(this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().pauseSoundUrl(
		this._fields[0]);
	} else if (E['playlist-sound?'](this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().pausePlaylist(
		this._fields[0]._fields[0]);
	}
    };

    E['effect:stop-sound'].prototype.run = function() {
	if (plt.Kernel.isString(this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().stopSoundUrl(
		this._fields[0]);
	} else if (E['playlist-sound?'](this._fields[0])) {
	    plt.platform.Platform.getInstance().getSoundService().stopPlaylist(
		this._fields[0]._fields[0]);
	}
    };

    E['effect:set-sound-volume'].prototype.run = function() {
	plt.platform.Platform.getInstance().getSoundService().setVolume(
	    plt.types.NumberTower.toFixnum(this._fields[0]));
    };

    E['effect:set-beep-volume'].prototype.run = function() {
	plt.platform.Platform.getInstance().getSoundService().setRingerVolume(
	    plt.types.NumberTower.toFixnum(this._fields[0]));
    };

    E['effect:raise-sound-volume'].prototype.run = function() {
	plt.platform.Platform.getInstance().getSoundService.raiseVolume();
    };

    E['effect:lower-sound-volume'].prototype.run = function() {
	plt.platform.Platform.getInstance().getSoundService.lowerVolume();
    };

    E['effect:set-wake-lock'].prototype.run = function() {
	plt.platform.Platform.getInstance().getPowerService().setWakeLock(
	    plt.types.NumberTower.toFixnum(this._fields[0]));
    };

    E['effect:release-wake-lock'].prototype.run = function() {
	plt.platform.Platform.getInstance().getPowerService().releaseWakeLock();
    };
    
    E['effect:pick-playlist'].prototype.run = function() {
	var updater = this._fields[0];
	var callback = function(playlist) {
	    var playlistSound = E['make-playlist-sound'](playlist);
	    setTimeout(function() {
		var changeWorld = plt.world.config.CONFIG.lookup("changeWorld");
		changeWorld(function(w) {
		    return updater([w, playlistSound]);
		});
	    }, 0);
	}
	plt.platform.Platform.getInstance().getPickPlaylistService().pickPlaylist(callback);
    };

    E['effect:pick-random'].prototype.run = function() {
	var aRandomNumber =
	    plt.types.Rational.makeInstance(
		Math.floor(plt.types.NumberTower.toFixnum(this._fields[0]) * 
			   Math.random()),
		1);
	var callback = this._fields[1];
	return function(w) { return callback([w, aRandomNumber]) }
    };

}());