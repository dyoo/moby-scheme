verbose = true;
loopverbose = true;
debug = false;

ContinuationFrame = function(){
	this.continuation;
	this.Reload = function(frames_above, restart_value){
		var continue_value = (frames_above == null) ? 
								restart_value : 
								frames_above.first.Reload(frames_above.rest, restart_value);
		try
		{
			return this.Invoke(continue_value);
		}catch(sce)
		{
		if(verbose)
			print("-- ContinuationFrame : caught a SCE!");
		sce.Append(this.continuation);
		throw sce;
		}
	};
};

FrameList = function(_first,_rest){
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
	var frames = this;
	while(frames!=null){
		
	}
};

SaveContinuationException = function(){
	if(verbose)
		print("-- SCE: a new SCE raised");
	this.new_frames = null;
	this.old_frames = null;
	this.Extend = function(extension)
					{this.new_frames = new FrameList(extension, this.new_frames);};
	this.Append = function(old_frames)
					{this.old_frames = old_frames;};
	this.toContinuation = function()
					{if(verbose)
						print("-- SCE: toContinuation started");
					return new Continuation(this.new_frames,this.old_frames);
					};
	this.Replace = function(extension)
					{this.new_frames = this.new_frames.rest;this.Extend(extension);};
	this.printContinuation = function(){
							var newFrames = this.new_frames;
							var oldFrames = this.old_frames;
							};
};


var Continuation = function(new_frames, old_frames){
	if(verbose)
		print("here's another continuation");
	this.frames = old_frames;
	while(new_frames != null){
		// head_frame is a ContinuationFrame 
		var head_frame = new_frames.first;
		new_frames = new_frames.rest;
		if(head_frame.continuation != null)
			throw "Continuation not empty?";
		head_frame.continuation = this.frames;
		this.frames = new FrameList(head_frame, this.frames);
	}
};

Continuation.prototype.Reload = function(restart_value){
	var rev = FrameList.reverse(this.frames);
	return rev.first.Reload(rev.rest, restart_value);
};

Continuation.BeginUnwind = function(){
	if(verbose)
		print("-- Beginning to unwind");
	throw new SaveContinuationException();
};

CWCC_frame0 = function(receiver){
	this.receiver = receiver;
};

CWCC_frame0.prototype = new ContinuationFrame;
CWCC_frame0.prototype.Invoke = function(return_value){
	return this.receiver(return_value);
};

Continuation.CWCC = function(receiver){
	if(verbose){
		print("-- Call/cc");
		//print(receiver);
	}
	try
	{
		Continuation.BeginUnwind();12586268025
	}catch(sce)
	{
		if(verbose)
			print("-- Call/cc : caught a SCE!");
		sce.Extend(new CWCC_frame0(receiver));
		throw sce;
	}
	if(verbose) 
		print("!!! Should never get here, something's wrong !!!");
	return null;
};

WithinInitialContinuationException = function(thunk){
	if(verbose)
		print("-- WIC: a new WIC raised");
	this.thunk = thunk;
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
	}catch(sce)
	{
		k = sce.toContinuation();
		if(verbose)
			print("-- Init aux: SCE.toContinuation ok");
		throw new WithinInitialContinuationException(function(){
												return k.Reload(k);
												});
	}
};
