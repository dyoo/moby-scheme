// Test of possible Cheney on the MTA for tail calls.

var usingException = {};
(function() {
    // Test of possible Cheney on the MTA for tail calls.

    var trampolineThreshold = 3;
    var currentDepth = 0;


    function Bounce(continuation, arg) {
	this.continuation = continuation;
	this.arg = arg;
    }

    
    
    // startTrampoline: continuation arg ->  void
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
		applyContinuation(currentBouncing, currentArg);
		break;
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
	    //	console.debug("not bouncing");
	    aContinuation.restart(arg);
	} else {
	    //	console.log("bouncing");
	    throw new Bounce(aContinuation, arg);
	}
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


    usingException.startTrampoline = startTrampoline;
    usingException.applyContinuation = applyContinuation;
    usingException.makeContinuation = makeContinuation;
    usingException.setThreshold = function(n) {
	trampolineThreshold = n;
    };
    
})();    
    
    
    //////////////////////////////////////////////////////////////////////
    var usingTimeout = {};
    
    (function() {

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

    usingTimeout.startTrampoline = startTrampoline;
    usingTimeout.applyContinuation = applyContinuation;
    usingTimeout.makeContinuation = makeContinuation;
    usingTimeout.setThreshold = function(n) {
	trampolineThreshold = n;
    };
})();









//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////


var makeContinuation;
var applyContinuation;
var startTrampoline;

// User functions should be in CPS form, and all functions should be
// lifted to the toplevel.

//////////////////////////////////////////////////////////////////////
function sum(n, k) {
  if (n == 0) {
      applyContinuation(k, 0);
  } else {
      var k1 = makeContinuation(sum_lift_1, { k: k, n: n });
      var k2 = makeContinuation(sum_lift_2, { k1: k1 });
      applyContinuation(k2, n-1);
  }
}

function sum_lift_1(val, env) {
  applyContinuation(env.k, val + env.n);
}

function sum_lift_2(val, env) {
  sum(val, env.k1);
}

//////////////////////////////////////////////////////////////////////
function sumIter(n, k) {
    sumIter(n, 0, k);
}

function _sumIter(n, acc, k) {
    if (n == 0) {
	applyContinuation(k, acc);
    } else {
	applyContinuation(
	    makeContinuation(sumIter_lift_2, {n:n, acc:acc, k:k}));
    }
}

function sumIter_lift_2(val, env) {
    _sumIter(env.n-1, env.acc+env.n, env.k)
}

    
//////////////////////////////////////////////////////////////////////
function sumLoop(n, k) {
  var result = 0;
  for(var i = 1; i <= n; i++) {
    result += i;
  }
  k(result);
}

//////////////////////////////////////////////////////////////////////

  
function exceptionDriver(f, inputValue, threshold, withResultTo) {
    makeContinuation = usingException.makeContinuation;
    applyContinuation = usingException.applyContinuation;
    usingException.threshold = threshold;
    startTrampoline = usingException.startTrampoline;
    startTrampoline(makeContinuation(function(arg, env) { 
        f(Number(arg), makeContinuation(withResultTo, {}))}),
                    inputValue);
}

function timeoutDriver(f, inputValue, threshold, withResultTo) {
    makeContinuation = usingTimeout.makeContinuation;
    applyContinuation = usingTimeout.applyContinuation;
    usingTimeout.threshold = threshold;
    startTrampoline = usingTimeout.startTrampoline;

    startTrampoline(makeContinuation(function(arg, env) { 
        f(Number(arg), makeContinuation(withResultTo, {}))}),
                    inputValue);
}

function plainDriver(f, inputValue, threshold, withResultTo) {
    f(inputValue, withResultTo);
}


function compute(driver, f) {
    var inputElt = document.getElementById('input');
    var outputElt = document.getElementById('output');
    outputElt.innerHTML = "Computing...";
    var d = new Date().getTime();
    var threshold = Number(document.getElementById('trampolineDepth').value);
    var assignToOutputElt = function(val, env) {	
	outputElt.innerHTML = outputElt.innerHTML + "<br>" + (val.toString() + 
                               " (computed in " + ((new Date()).getTime() - d) 
			       + " milliseconds)");
    };
    driver(f, inputElt.value, threshold, assignToOutputElt);
}
