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
        temp1 = Continuation.CWCC(function(k) { return k("hello"); } );
    } catch (sce) {
         sce.Extend(new f_frame());
	 throw sce;
    }
    return f0(temp1);
}
var f_frame = function() {
};
f_frame.prototype = new ContinuationFrame;
f_frame.prototype.Invoke = function() {
    return f();
}


var f0 = function(temp1) {
    return [temp1, "world"];
};

// Let's try to test this code.

var test = function() {
    return Continuation.EstablishInitialContinuation(function() { return f(); });
}
