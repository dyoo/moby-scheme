// Translation of (+ 2 (call/cc (lambda (k) (* 5 (k 4)))))
// which should evaluate to 6.
// 
// from http://www.scheme.com/tspl4/further.html#./further:h3

var f = function() {
    var temp1;
    try {
	temp1 = Continuation.CWCC(receiver);
    } catch (sce) {
	if (sce instanceof SaveContinuationException)
	    sce.Extend(new f1_frame());
	throw sce;
    }
    return 2 + temp1;
}

var f1 = function(temp1) {
    return 2 + temp1;
}

var f1_frame = function() {
}
f1_frame.prototype = new ContinuationFrame();
f1_frame.prototype.Invoke = function(temp1) {
    return f1(temp1);
};



var receiver = function(k) {
    var temp1;
    try {
	temp1 = Continuation.apply(k, 4);
    } catch (sce) {
	if (sce instanceof SaveContinuationException)
	    sce.Extend(new receiver1_frame());
	throw sce;
    }
    return 5 * temp1;
}

var receiver1 = function(temp1) {
    return 5 * temp1;
}

var receiver1_frame = function() {
}
receiver1_frame.prototype = new ContinuationFrame();
receiver1_frame.prototype.Invoke = function(v) {
    return receiver1(v);
}



var test = function() {
    return Continuation.EstablishInitialContinuation(function() { return f();});
}
