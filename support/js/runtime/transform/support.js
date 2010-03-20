// The code below is an implementation of the support code described
// in http://www.ccs.neu.edu/scheme/pubs/stackhack4.html

//////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////

ContinuationFrame = function(){
    this.continuation = null;
};

ContinuationFrame.prototype.Reload = function(frames_above, restart_value){
    				  
    var continue_value;
    if (frames_above === null) {
        continue_value = restart_value;
    } else {
        continue_value = frames_above.first.Reload(frames_above.rest, restart_value);
    }

    try {
	return this.Invoke(continue_value);
    } catch(sce) {
        if (! (sce instanceof SaveContinuationException)) { throw sce; }
	sce.Append(this.continuation);
	throw sce;
    }
};
ContinuationFrame.prototype.Invoke = function(return_value) {
    throw new Error("Unimplemented!");
};

ContinuationFrame.prototype.toString = function() { return "[ContinuationFrame]"; };


//////////////////////////////////////////////////////////////////////

FrameList = function(_first, _rest){
    this.first = _first;
    this.rest = _rest;
};

FrameList.reverse = function(originalFrameList){
    var result = null;
    while(originalFrameList!=null){
	result = new FrameList(originalFrameList.first, result);
	originalFrameList = originalFrameList.rest;
    }
    return result;
};


FrameList.prototype.toString = function() { return "[FrameList]"; }

//////////////////////////////////////////////////////////////////////

SaveContinuationException = function(){
    this.new_frames = null;
    this.old_frames = null;
};

SaveContinuationException.prototype.Extend = function(extension) {
    this.new_frames = new FrameList(extension, this.new_frames);
};

SaveContinuationException.prototype.Append = function(old_frames) {
    this.old_frames = old_frames;
};

SaveContinuationException.prototype.toContinuation = function() {
    return new Continuation(this.new_frames, this.old_frames);
};

SaveContinuationException.prototype.toString = function() { return "[SaveContinuationException]"; }


ReplaceContinuationException = function(k, v) {
    this.k = k;
    this.v = v;
}

//////////////////////////////////////////////////////////////////////


var Continuation = function(new_frames, old_frames){
    this.frames = old_frames;
    while(new_frames !== null){
	// head_frame is a ContinuationFrame 
	var head_frame = new_frames.first;
	new_frames = new_frames.rest;
	if(head_frame.continuation !== null)
	    throw "Continuation not empty?";
	head_frame.continuation = this.frames;
	this.frames = new FrameList(head_frame, this.frames);
    }
};

Continuation.prototype.popTopFrame = function() {
    return new Continuation(null, this.frames.rest);
}


Continuation.prototype.reload = function(restart_value){
    var rev = FrameList.reverse(this.frames);
    return rev.first.Reload(rev.rest, restart_value);
};

Continuation.prototype.toString = function() { return "[Continuation]"; };

Continuation.BeginUnwind = function(){
    throw new SaveContinuationException();
};

Continuation.CWCC = function(receiver){
    try {
        Continuation.BeginUnwind();
    } catch(sce) {
        if (! (sce instanceof SaveContinuationException)) { throw sce; }
	sce.Extend(new CWCC_frame0(receiver));
	throw sce;
    }
    return null;
};

Continuation.apply = function(k, v) {
    throw new ReplaceContinuationException(k, v);
};


// thunk -> object
Continuation.EstablishInitialContinuation = function(thunk){
    while (true){
	try {
	    return Continuation.InitialContinuationAux(thunk);
	} catch(wic) {
	    if (! (wic instanceof WithinInitialContinuationException)) { throw wic; }
	    thunk = wic.thunk;
	}
    }
};

// thunk -> object
Continuation.InitialContinuationAux = function(thunk){
    try {
	return thunk();
    } catch(sce) {
        if (sce instanceof SaveContinuationException) { 
	    k = sce.toContinuation();
	    throw new WithinInitialContinuationException(makeWICThunk(k));
        } else if (sce instanceof ReplaceContinuationException)  {
            // NOTE: maybe this code belongs in the trampoline established in EstablishInitialContinuation.
            return sce.k.reload(sce.v);
        } else {
	    throw sce;
        }
    }
};

var makeWICThunk = function(k) {
    return function() {
        var adjustedContinuation = k.popTopFrame();
	return k.reload(adjustedContinuation);
    }
};


//////////////////////////////////////////////////////////////////////

CWCC_frame0 = function(receiver){
    ContinuationFrame.call(this);
    this.receiver = receiver;
};
CWCC_frame0.prototype = new ContinuationFrame();
CWCC_frame0.prototype.Invoke = function(return_value){
    return this.receiver(return_value);
};
CWCC_frame0.prototype.toString = function() { return "[CWCC_frame0]"; };


//////////////////////////////////////////////////////////////////////
WithinInitialContinuationException = function(thunk){
    this.thunk = thunk;
};


WithinInitialContinuationException.prototype.toString = function() { return "[WithinInitialContinuationException]"; };

//////////////////////////////////////////////////////////////////////
