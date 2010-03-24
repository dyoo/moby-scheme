load("../support.js");

// Translation of the function:
//
// (define (f)
//   (call/cc (lambda (k) (* 5 (k 4)))))
//
//
// The expected value from (f) should be 4.
//
// This should translate to the javascript code:
//
// var f = function() {
//     return callCC(function(k) { return 5 * Continuation.apply(k, 4); });
// };
//
// which should then be a-normalized to:
//
//
// var f = function() {
//     return callCC(function(k) { var t = Continuation.apply(k, 4);
//                                 return 5 * t; });
// };
//
//
// which should then be annotated to the following code:


var f = function() {
    return Continuation.CWCC(aReceiver);
};



var aReceiver = function(k) { 
    var t;
    try {
	t = Continuation.apply(k, 4);
    } catch (sce) {
	if (! (sce instanceof SaveContinuationException)) { 
	    throw sce; 
	}
	sce.Extend(new aReciever0_frame());
	throw sce;
    }
    return 5 * t; 
};


var aReceiver0 = function(t) {
    return 5 * t;
}

var aReceiver_frame = function() {
    ContinuationFrame.call(this);
}
aReceiver_frame.prototype = new ContinuationFrame();
aReceiver_frame.prototype.Invoke = function(v) { return aReceiver0(v); }
aReceiver_frame.prototype.toString = function() { return "[f0_frame]"; }

// Let's try to test this code.

var test = function() {
    return Continuation.EstablishInitialContinuation(function() { return f(); });
}
