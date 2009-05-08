// Test of possible Cheney on the MTA for tail calls.

var trampolineThreshold = 3;
var currentDepth = 0;


function Bounce(continuation, arg) {
    this.continuation = continuation;
    this.arg = arg;
}

    
    
// startTrampoline: continuation arg ->  a continuation around.
// If we come out with a value, return it.  Otherwise, bounce off the
// trampoline and continue working.
//
// WARNING: startTrampoline is NOT reentrant!
function startTrampoline(aContinuation, arg) {
    var currentBouncing = aContinuation;
    var currentArg = arg;
    currentDepth = 0;
    while(true) {
        try {
            var lastValue = applyContinuation(currentBouncing, currentArg);
            return lastValue;
        } catch (e) {
            if (e instanceof Bounce) {
		currentBouncing = e.continuation;
		currentArg = e.arg;
                currentDepth = 0;
	    } else {
		throw e;
	    }
        }
    }
}

// Apply a continuation.
function applyContinuation(aContinuation, arg) {
    // If the depth goes beyond the threshold, throw an exception.
    // Otherwise, just apply and continue.
    currentDepth = currentDepth + 1;
    if (currentDepth < trampolineThreshold) {
	console.debug("not bouncing");
	return aContinuation.apply(null, [arg]);
    } else {
	console.log("bouncing");
	throw new Bounce(aContinuation, arg);
    }
}


function makeContinuation(f) {
  // TODO: we may want a separate continuation object.
  return f;
}
