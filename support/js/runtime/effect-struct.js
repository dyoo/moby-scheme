// This is automatically generated by bootstrap-js-compiler.ss
// Please don't hand-edit this file.

function effect_colon_none() { plt.Kernel.Struct.call(this, "make-effect:none", []); }
                    effect_colon_none.prototype = new plt.Kernel.Struct();

function make_dash_effect_colon_none() { return new effect_colon_none(); }

function effect_colon_none_question_(obj) { 
              return obj != null && obj != undefined && obj instanceof effect_colon_none; }
function effect_colon_beep() { plt.Kernel.Struct.call(this, "make-effect:beep", []); }
                    effect_colon_beep.prototype = new plt.Kernel.Struct();

function make_dash_effect_colon_beep() { return new effect_colon_beep(); }

function effect_colon_beep_question_(obj) { 
              return obj != null && obj != undefined && obj instanceof effect_colon_beep; }
function effect_colon_play_dash_dtmf_dash_tone(tone,duration) { plt.Kernel.Struct.call(this, "make-effect:play-dtmf-tone", [tone,duration]);this.tone = tone;
this.duration = duration; }
                    effect_colon_play_dash_dtmf_dash_tone.prototype = new plt.Kernel.Struct();

function make_dash_effect_colon_play_dash_dtmf_dash_tone(id0,id1) { return new effect_colon_play_dash_dtmf_dash_tone(id0,id1); }
function effect_colon_play_dash_dtmf_dash_tone_dash_tone(obj) {
     if (effect_colon_play_dash_dtmf_dash_tone_question_ (obj)) {
        return obj.tone;
     } else {
        throw new plt.Kernel.MobyRuntimeError(            plt.Kernel.format('effect_colon_play_dash_dtmf_dash_tone_dash_tone: not a effect:play-dtmf-tone: ~s', [obj]));
     }
}

function effect_colon_play_dash_dtmf_dash_tone_dash_duration(obj) {
     if (effect_colon_play_dash_dtmf_dash_tone_question_ (obj)) {
        return obj.duration;
     } else {
        throw new plt.Kernel.MobyRuntimeError(            plt.Kernel.format('effect_colon_play_dash_dtmf_dash_tone_dash_duration: not a effect:play-dtmf-tone: ~s', [obj]));
     }
}

function effect_colon_play_dash_dtmf_dash_tone_question_(obj) { 
              return obj != null && obj != undefined && obj instanceof effect_colon_play_dash_dtmf_dash_tone; }
function effect_colon_send_dash_sms(address,msg) { plt.Kernel.Struct.call(this, "make-effect:send-sms", [address,msg]);this.address = address;
this.msg = msg; }
                    effect_colon_send_dash_sms.prototype = new plt.Kernel.Struct();

function make_dash_effect_colon_send_dash_sms(id0,id1) { return new effect_colon_send_dash_sms(id0,id1); }
function effect_colon_send_dash_sms_dash_address(obj) {
     if (effect_colon_send_dash_sms_question_ (obj)) {
        return obj.address;
     } else {
        throw new plt.Kernel.MobyRuntimeError(            plt.Kernel.format('effect_colon_send_dash_sms_dash_address: not a effect:send-sms: ~s', [obj]));
     }
}

function effect_colon_send_dash_sms_dash_msg(obj) {
     if (effect_colon_send_dash_sms_question_ (obj)) {
        return obj.msg;
     } else {
        throw new plt.Kernel.MobyRuntimeError(            plt.Kernel.format('effect_colon_send_dash_sms_dash_msg: not a effect:send-sms: ~s', [obj]));
     }
}

function effect_colon_send_dash_sms_question_(obj) { 
              return obj != null && obj != undefined && obj instanceof effect_colon_send_dash_sms; }
function playlist_dash_sound(playlist) { plt.Kernel.Struct.call(this, "make-playlist-sound", [playlist]);this.playlist = playlist; }
                    playlist_dash_sound.prototype = new plt.Kernel.Struct();

function make_dash_playlist_dash_sound(id0) { return new playlist_dash_sound(id0); }
function playlist_dash_sound_dash_playlist(obj) {
     if (playlist_dash_sound_question_ (obj)) {
        return obj.playlist;
     } else {
        throw new plt.Kernel.MobyRuntimeError(            plt.Kernel.format('playlist_dash_sound_dash_playlist: not a playlist-sound: ~s', [obj]));
     }
}

function playlist_dash_sound_question_(obj) { 
              return obj != null && obj != undefined && obj instanceof playlist_dash_sound; }
function sound_question_(x) { return ((plt.Kernel.setLastLoc("offset=0 line=0 span=0 id=\"\"")   && plt.Kernel.string_question_(x))||(plt.Kernel.setLastLoc("offset=0 line=0 span=0 id=\"\"")   && playlist_dash_sound_question_(x))); }
function effect_colon_play_dash_sound(sound) { plt.Kernel.Struct.call(this, "make-effect:play-sound", [sound]);this.sound = sound; }
                    effect_colon_play_dash_sound.prototype = new plt.Kernel.Struct();

function make_dash_effect_colon_play_dash_sound(id0) { return new effect_colon_play_dash_sound(id0); }
function effect_colon_play_dash_sound_dash_sound(obj) {
     if (effect_colon_play_dash_sound_question_ (obj)) {
        return obj.sound;
     } else {
        throw new plt.Kernel.MobyRuntimeError(            plt.Kernel.format('effect_colon_play_dash_sound_dash_sound: not a effect:play-sound: ~s', [obj]));
     }
}

function effect_colon_play_dash_sound_question_(obj) { 
              return obj != null && obj != undefined && obj instanceof effect_colon_play_dash_sound; }
function effect_colon_pause_dash_sound(sound) { plt.Kernel.Struct.call(this, "make-effect:pause-sound", [sound]);this.sound = sound; }
                    effect_colon_pause_dash_sound.prototype = new plt.Kernel.Struct();

function make_dash_effect_colon_pause_dash_sound(id0) { return new effect_colon_pause_dash_sound(id0); }
function effect_colon_pause_dash_sound_dash_sound(obj) {
     if (effect_colon_pause_dash_sound_question_ (obj)) {
        return obj.sound;
     } else {
        throw new plt.Kernel.MobyRuntimeError(            plt.Kernel.format('effect_colon_pause_dash_sound_dash_sound: not a effect:pause-sound: ~s', [obj]));
     }
}

function effect_colon_pause_dash_sound_question_(obj) { 
              return obj != null && obj != undefined && obj instanceof effect_colon_pause_dash_sound; }
function effect_colon_stop_dash_sound(sound) { plt.Kernel.Struct.call(this, "make-effect:stop-sound", [sound]);this.sound = sound; }
                    effect_colon_stop_dash_sound.prototype = new plt.Kernel.Struct();

function make_dash_effect_colon_stop_dash_sound(id0) { return new effect_colon_stop_dash_sound(id0); }
function effect_colon_stop_dash_sound_dash_sound(obj) {
     if (effect_colon_stop_dash_sound_question_ (obj)) {
        return obj.sound;
     } else {
        throw new plt.Kernel.MobyRuntimeError(            plt.Kernel.format('effect_colon_stop_dash_sound_dash_sound: not a effect:stop-sound: ~s', [obj]));
     }
}

function effect_colon_stop_dash_sound_question_(obj) { 
              return obj != null && obj != undefined && obj instanceof effect_colon_stop_dash_sound; }
function effect_colon_set_dash_sound_dash_volume(volume) { plt.Kernel.Struct.call(this, "make-effect:set-sound-volume", [volume]);this.volume = volume; }
                    effect_colon_set_dash_sound_dash_volume.prototype = new plt.Kernel.Struct();

function make_dash_effect_colon_set_dash_sound_dash_volume(id0) { return new effect_colon_set_dash_sound_dash_volume(id0); }
function effect_colon_set_dash_sound_dash_volume_dash_volume(obj) {
     if (effect_colon_set_dash_sound_dash_volume_question_ (obj)) {
        return obj.volume;
     } else {
        throw new plt.Kernel.MobyRuntimeError(            plt.Kernel.format('effect_colon_set_dash_sound_dash_volume_dash_volume: not a effect:set-sound-volume: ~s', [obj]));
     }
}

function effect_colon_set_dash_sound_dash_volume_question_(obj) { 
              return obj != null && obj != undefined && obj instanceof effect_colon_set_dash_sound_dash_volume; }
function effect_colon_raise_dash_sound_dash_volume() { plt.Kernel.Struct.call(this, "make-effect:raise-sound-volume", []); }
                    effect_colon_raise_dash_sound_dash_volume.prototype = new plt.Kernel.Struct();

function make_dash_effect_colon_raise_dash_sound_dash_volume() { return new effect_colon_raise_dash_sound_dash_volume(); }

function effect_colon_raise_dash_sound_dash_volume_question_(obj) { 
              return obj != null && obj != undefined && obj instanceof effect_colon_raise_dash_sound_dash_volume; }
function effect_colon_lower_dash_sound_dash_volume() { plt.Kernel.Struct.call(this, "make-effect:lower-sound-volume", []); }
                    effect_colon_lower_dash_sound_dash_volume.prototype = new plt.Kernel.Struct();

function make_dash_effect_colon_lower_dash_sound_dash_volume() { return new effect_colon_lower_dash_sound_dash_volume(); }

function effect_colon_lower_dash_sound_dash_volume_question_(obj) { 
              return obj != null && obj != undefined && obj instanceof effect_colon_lower_dash_sound_dash_volume; }
function effect_colon_set_dash_wake_dash_lock(locks) { plt.Kernel.Struct.call(this, "make-effect:set-wake-lock", [locks]);this.locks = locks; }
                    effect_colon_set_dash_wake_dash_lock.prototype = new plt.Kernel.Struct();

function make_dash_effect_colon_set_dash_wake_dash_lock(id0) { return new effect_colon_set_dash_wake_dash_lock(id0); }
function effect_colon_set_dash_wake_dash_lock_dash_locks(obj) {
     if (effect_colon_set_dash_wake_dash_lock_question_ (obj)) {
        return obj.locks;
     } else {
        throw new plt.Kernel.MobyRuntimeError(            plt.Kernel.format('effect_colon_set_dash_wake_dash_lock_dash_locks: not a effect:set-wake-lock: ~s', [obj]));
     }
}

function effect_colon_set_dash_wake_dash_lock_question_(obj) { 
              return obj != null && obj != undefined && obj instanceof effect_colon_set_dash_wake_dash_lock; }
function effect_colon_release_dash_wake_dash_lock() { plt.Kernel.Struct.call(this, "make-effect:release-wake-lock", []); }
                    effect_colon_release_dash_wake_dash_lock.prototype = new plt.Kernel.Struct();

function make_dash_effect_colon_release_dash_wake_dash_lock() { return new effect_colon_release_dash_wake_dash_lock(); }

function effect_colon_release_dash_wake_dash_lock_question_(obj) { 
              return obj != null && obj != undefined && obj instanceof effect_colon_release_dash_wake_dash_lock; }
function effect_colon_pick_dash_playlist(update_dash_f) { plt.Kernel.Struct.call(this, "make-effect:pick-playlist", [update_dash_f]);this.update_dash_f = update_dash_f; }
                    effect_colon_pick_dash_playlist.prototype = new plt.Kernel.Struct();

function make_dash_effect_colon_pick_dash_playlist(id0) { return new effect_colon_pick_dash_playlist(id0); }
function effect_colon_pick_dash_playlist_dash_update_dash_f(obj) {
     if (effect_colon_pick_dash_playlist_question_ (obj)) {
        return obj.update_dash_f;
     } else {
        throw new plt.Kernel.MobyRuntimeError(            plt.Kernel.format('effect_colon_pick_dash_playlist_dash_update_dash_f: not a effect:pick-playlist: ~s', [obj]));
     }
}

function effect_colon_pick_dash_playlist_question_(obj) { 
              return obj != null && obj != undefined && obj instanceof effect_colon_pick_dash_playlist; }
function effect_colon_pick_dash_random(n,update_dash_f) { plt.Kernel.Struct.call(this, "make-effect:pick-random", [n,update_dash_f]);this.n = n;
this.update_dash_f = update_dash_f; }
                    effect_colon_pick_dash_random.prototype = new plt.Kernel.Struct();

function make_dash_effect_colon_pick_dash_random(id0,id1) { return new effect_colon_pick_dash_random(id0,id1); }
function effect_colon_pick_dash_random_dash_n(obj) {
     if (effect_colon_pick_dash_random_question_ (obj)) {
        return obj.n;
     } else {
        throw new plt.Kernel.MobyRuntimeError(            plt.Kernel.format('effect_colon_pick_dash_random_dash_n: not a effect:pick-random: ~s', [obj]));
     }
}

function effect_colon_pick_dash_random_dash_update_dash_f(obj) {
     if (effect_colon_pick_dash_random_question_ (obj)) {
        return obj.update_dash_f;
     } else {
        throw new plt.Kernel.MobyRuntimeError(            plt.Kernel.format('effect_colon_pick_dash_random_dash_update_dash_f: not a effect:pick-random: ~s', [obj]));
     }
}

function effect_colon_pick_dash_random_question_(obj) { 
              return obj != null && obj != undefined && obj instanceof effect_colon_pick_dash_random; }
function effect_question_(thing) { return ((plt.Kernel.setLastLoc("offset=0 line=0 span=0 id=\"\"")   && effect_colon_none_question_(thing))||(plt.Kernel.setLastLoc("offset=0 line=0 span=0 id=\"\"")   && effect_colon_beep_question_(thing))||(plt.Kernel.setLastLoc("offset=0 line=0 span=0 id=\"\"")   && effect_colon_play_dash_dtmf_dash_tone_question_(thing))||(plt.Kernel.setLastLoc("offset=0 line=0 span=0 id=\"\"")   && effect_colon_send_dash_sms_question_(thing))||(plt.Kernel.setLastLoc("offset=0 line=0 span=0 id=\"\"")   && effect_colon_play_dash_sound_question_(thing))||(plt.Kernel.setLastLoc("offset=0 line=0 span=0 id=\"\"")   && effect_colon_pause_dash_sound_question_(thing))||(plt.Kernel.setLastLoc("offset=0 line=0 span=0 id=\"\"")   && effect_colon_stop_dash_sound_question_(thing))||(plt.Kernel.setLastLoc("offset=0 line=0 span=0 id=\"\"")   && effect_colon_set_dash_sound_dash_volume_question_(thing))||(plt.Kernel.setLastLoc("offset=0 line=0 span=0 id=\"\"")   && effect_colon_raise_dash_sound_dash_volume_question_(thing))||(plt.Kernel.setLastLoc("offset=0 line=0 span=0 id=\"\"")   && effect_colon_lower_dash_sound_dash_volume_question_(thing))||(plt.Kernel.setLastLoc("offset=0 line=0 span=0 id=\"\"")   && effect_colon_set_dash_wake_dash_lock_question_(thing))||(plt.Kernel.setLastLoc("offset=0 line=0 span=0 id=\"\"")   && effect_colon_release_dash_wake_dash_lock_question_(thing))||(plt.Kernel.setLastLoc("offset=0 line=0 span=0 id=\"\"")   && effect_colon_pick_dash_playlist_question_(thing))||(plt.Kernel.setLastLoc("offset=0 line=0 span=0 id=\"\"")   && effect_colon_pick_dash_random_question_(thing))); }
(function() { 
  ((function (toplevel_dash_expression_dash_show0) { 
















 })  )(arguments[0] || plt.Kernel.identity);
})();