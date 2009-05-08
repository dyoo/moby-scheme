// Test of possible Cheney on the MTA for tail calls.

var trampolineThreshold = 3;
var currentDepth = 0;
var currentContinuation;
var currentArg;

// startTrampoline: continuation arg ->  void
// If we come out with a value, return it.  Otherwise, bounce off the
// trampoline and continue working.
//
// WARNING: startTrampoline is NOT reentrant!
function startTrampoline(aContinuation, arg) {
    currentDepth = 0; 
    applyContinuation(aContinuation, arg);
}


// Apply a continuation.
function applyContinuation(aContinuation, arg) {
    // If the depth goes beyond the threshold, set up a timeout
    // Otherwise, just apply and continue.
    currentDepth = currentDepth + 1;
    if (currentDepth < trampolineThreshold) {
	aContinuation.restart(arg);
    } else {
	currentContinuation = aContinuation;
	currentArg = arg;
	setTimeout(continueApplication, 0);
    }
}

function continueApplication() {
    currentDepth = 0;
    currentContinuation.restart(currentArg);
}



function Continuation(f, env) {
    this.f = f;
    this.env = env;
}


// Continuation.restart: X -> void
Continuation.prototype.restart = function(arg) {
    this.f.apply(null, [arg].concat([this.env]));
}


// makeContinuation: (X env -> void) env -> Continuation
function makeContinuation(f, env) {
    return new Continuation(f, env);
}
