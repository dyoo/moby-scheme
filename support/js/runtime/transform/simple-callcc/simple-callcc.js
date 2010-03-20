load("../support.js");

// Translation of the function:
//
// (define (f)
//   (vector (call/cc (lambda (k) (k "hello")))
//           "world"))
//
//
// The expected value from (f) should be #("hello" "world")
//
//
// This should translate to the javascript code:
//
// var f = function() {
//     return [callCC(function(k) { return k("hello")}),
//             "world"];
// }
//
//
// which should then be a-normalized to:
//
//
// var f = function() {
//     var temp1 = callCC(function(k) { return k("hello") });
//     return f0(temp1);
// }
//
// var f0 = function(temp1) {
//     return [temp1, "world"]
// }
//
//
// which should then be annotated to the following code:



var f = function() {
    var temp1;
    try {
        temp1 = Continuation.CWCC(function(k) { return hello; } );
    } catch (sce) {
         if (! sce instanceof SaveContinuationException) { throw sce; }
         sce.Extend(new f0_frame());
	 throw sce;
    }
    return f0(temp1);
}

var f_frame = function() {
};
f_frame.prototype = new ContinuationFrame();
f_frame.prototype.Invoke = function(v) { return f(); };


var f0 = function(temp1) {
    return [temp1, "world"];
};
var f0_frame = function(temp1) {
    this.temp1 = temp1;
};
f0_frame.prototype = new ContinuationFrame();
f0_frame.prototype.Invoke = function(v) { return f0(v); }


// Let's try to test this code.

var test = function() {
    return Continuation.EstablishInitialContinuation(function() { return f(); });
}
