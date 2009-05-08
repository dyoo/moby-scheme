// Test of possible Cheney on the MTA for tail calls.

var trampolineThreshold = 3;
var currentDepth = 0;


// startTrampoline: continuation arg ->  void
// If we come out with a value, return it.  Otherwise, bounce off the
// trampoline and continue working.
//
// WARNING: startTrampoline is NOT reentrant!
function startTrampoline(aContinuation, arg) {
    setTimeout(function() { currentDepth = 0; applyContinuation(aContinuation, arg); }, 
               0);
}


// Apply a continuation.
function applyContinuation(aContinuation, arg) {
    // If the depth goes beyond the threshold, set up a timeout
    // Otherwise, just apply and continue.
    currentDepth = currentDepth + 1;
    if (currentDepth < trampolineThreshold) {
//	console.debug("not bouncing");
	aContinuation.apply(null, [arg]);
    } else {
//	console.log("bouncing");
	setTimeout(function() { currentDepth = 0; aContinuation.apply(null, [arg]) },
                   0);
    }
}


function makeContinuation(f) {
  // TODO: we may want a separate continuation object.
  return f;
}
