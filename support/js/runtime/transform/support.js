// The code below is an implementation of the support code described
// in http://www.ccs.neu.edu/scheme/pubs/stackhack4.html

//////////////////////////////////////////////////////////////////////

// GLOBALS

verbose = false;
loopverbose = false;
debug = false;
printContinuation = false;


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
	if(verbose)
	    print("-- ContinuationFrame : caught a SCE!");
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

FrameList.prototype.print = function(){
};

FrameList.prototype.toString = function() { return "[FrameList]"; }

//////////////////////////////////////////////////////////////////////

SaveContinuationException = function(){
    if(verbose)
	print("-- SCE: a new SCE raised");
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
    if(verbose)
	print("-- SCE: toContinuation started");
    return new Continuation(this.new_frames,this.old_frames);
};

SaveContinuationException.prototype.toString = function() { return "[SaveContinuationException]"; }

//////////////////////////////////////////////////////////////////////


var Continuation = function(new_frames, old_frames){
    if(verbose)
	print("here's another continuation");
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

Continuation.prototype.Reload = function(restart_value){
    var rev = FrameList.reverse(this.frames);
    return rev.first.Reload(rev.rest, restart_value);
};

Continuation.prototype.print = function(){
    print("--- PRINTING THE CONTINUATION ---");
    var _frames = this.frames;
    while(_frames!=null){
	_frames.first.print();
	_frames = _frames.rest;
    }
    print("--- PRINTING DONE. ---");
};

Continuation.prototype.toString = function() { return "[Continuation]"; };

Continuation.BeginUnwind = function(){
    if(verbose)
	print("-- Beginning to unwind");
    throw new SaveContinuationException();
};

Continuation.CWCC = function(receiver){
    if(verbose){
		print("-- Call/cc");
    }
    try
    {
		Continuation.BeginUnwind();
    }catch(sce)
    {
        if (! (sce instanceof SaveContinuationException)) { throw sce; }
	if(verbose)
	    print("-- Call/cc : caught a SCE!");
	sce.Extend(new CWCC_frame0(receiver));
	throw sce;
    }
    if(verbose) 
	print("!!! Should never get here, something's wrong !!!");
    return null;
};



Continuation.EstablishInitialContinuation = function(thunk){
    if(verbose){
	print("-- EIC: ");
	//print(thunk);
    }
    while(true){
	try
	{
	    if(loopverbose)
		print("-- EIC: loop-try");
	    return Continuation.InitialContinuationAux(thunk);
	}catch(wic)
	{
	    if (! (wic instanceof WithinInitialContinuationException)) { throw wic; }
	    if(loopverbose)
		print("-- EIC: loop-caught");
	    thunk = wic.thunk;
	}
    }
};

Continuation.InitialContinuationAux = function(thunk){
    try
    {
	return thunk();
    } catch(sce) {
        if (! (sce instanceof SaveContinuationException)) { throw sce; }
	k = sce.toContinuation();
	if(printContinuation)
	    k.print();
	if(verbose)
	    print("-- Init aux: SCE.toContinuation ok");
	throw new WithinInitialContinuationException(k);
    }
};


//////////////////////////////////////////////////////////////////////

CWCC_frame0 = function(receiver){
    this.receiver = receiver;
};

CWCC_frame0.prototype = new ContinuationFrame();
CWCC_frame0.prototype.Invoke = function(return_value){
    return this.receiver(return_value);
};

CWCC_frame0.prototype.print = function(){
    print("CWCC Frame0");
};

CWCC_frame0.prototype.toString = function() { return "[CWCC_frame0]"; };


//////////////////////////////////////////////////////////////////////
WithinInitialContinuationException = function(k){
    if(verbose)
	print("-- WIC: a new WIC raised");
    this.k = k;
};

WithinInitialContinuationException.prototype.thunk = function() {
    return this.k.Reload(this.k);
};

WithinInitialContinuationException.prototype.toString = function() { return "[WithinInitialContinuationException]"; };

//////////////////////////////////////////////////////////////////////
