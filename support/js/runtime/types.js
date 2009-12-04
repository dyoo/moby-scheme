if (typeof(plt) === 'undefined') { var plt = {}; }


// Depends on kernel.js.
(function() {
    
    
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // Types
    
    plt.types = plt.types || {};
    

    //////////////////////////////////////////////////////////////////////
    // helper functions


    var appendChild = function(parent, child) {
	parent.appendChild(child);
    }


    // Inheritance from pg 168: Javascript, the Definitive Guide.
    var heir = function(p) {
	var f = function() {}
	f.prototype = p;
	return new f();
    }


    var _eqHashCodeCounter = 0;
    plt.types.makeEqHashCode = function() {
	_eqHashCodeCounter++;
	return _eqHashCodeCounter;
    }
    
    
    // plt.Kernel.getHashCode: any -> (or fixnum string)
    // Produces a hashcode appropriate for eq.
    plt.types.getEqHashCode = function(x) {
	if (x && x._eqHashCode) {
	    return x._eqHashCode;
	}
	if (typeof(x) == 'string') {
	    return x;
	}
	return 0;
    };



    //////////////////////////////////////////////////////////////////////


    // Structures.
    var Struct = function (constructorName, fields) {
	this._constructorName = constructorName; 
	this._fields = fields;
	this._eqHashCode = plt.types.makeEqHashCode();
    };

    plt.types.Struct = Struct;

    Struct.prototype.toWrittenString = function(cache) { 
	cache.put(this, true);
	var buffer = [];
	buffer.push("(");
	buffer.push(this._constructorName);
	for(var i = 0; i < this._fields.length; i++) {
	    buffer.push(" ");
	    buffer.push(plt.Kernel.toWrittenString(this._fields[i], cache));
	}
	buffer.push(")");
	return plt.types.String.makeInstance(buffer.join(""));
    };

    Struct.prototype.toDisplayedString = Struct.prototype.toWrittenString;

    Struct.prototype.toDomNode = function(cache) {
	cache.put(this, true);
	var node = document.createElement("div");
	node.appendChild(document.createTextNode("("));
	node.appendChild(document.createTextNode(this._constructorName));
	for(var i = 0; i < this._fields.length; i++) {
	    node.appendChild(document.createTextNode(" "));
	    appendChild(node, plt.Kernel.toDomNode(this._fields[i], cache));
	}
	node.appendChild(document.createTextNode(")"));
	return node;
    }


    Struct.prototype.isEqual = function(other, aUnionFind) {
	if (typeof(other) != 'object') {
	    return false;
	}
	if (! other._constructorName) {
	    return false;
	}
	if (other._constructorName != this._constructorName) {
	    return false;
	}
	if (typeof(other._fields) === 'undefined') {
	    return false;
	}
	if (this._fields.length != other._fields.length) {
	    return false;
	}
	for (var i = 0; i < this._fields.length; i++) {
	    if (! plt.Kernel.isEqual(this._fields[i],
				     other._fields[i],
				     aUnionFind)) {
		return false;
	    }
	}
	return true;
    };









    //////////////////////////////////////////////////////////////////////
    // Boxes
    
    var Box = function(x) { 
	plt.types.Struct.call(this, "box", [x]);
	this._eqHashCode = plt.types.makeEqHashCode();
    };

    Box.prototype = heir(plt.types.Struct.prototype);

    Box.prototype.unbox = function() {
	return this._fields[0];
    }

    Box.prototype.set = function(newVal) {
	this._fields[0] = newVal;
    }
    //////////////////////////////////////////////////////////////////////








    // We are reusing the built-in Javascript boolean class here.
    plt.types.Logic = {
	TRUE : true,
	FALSE : false
    };

    // WARNING
    // WARNING: we are extending the built-in Javascript boolean class here!
    // WARNING
    Boolean.prototype.toWrittenString = function(cache) {
	if (this.valueOf()) { return "true"; }
	return "false";
    };
    Boolean.prototype.toDisplayedString = Boolean.prototype.toWrittenString;


    Boolean.prototype.isEqual = function(other, aUnionFind){
	return this == other;
    };




    // Chars
    // Char: string -> Char
    plt.types.Char = function(val){
	this.val = val;
	this._eqHashCode = plt.types.makeEqHashCode();
    };
    
    plt.types.Char.makeInstance = function(val){
	return new plt.types.Char(val);
    };

    plt.types.Char.prototype.toWrittenString = function(cache) {
	return "#\\" + this.val;
    };

    plt.types.Char.prototype.toDisplayedString = function (cache) {
        return this.val;
    };

    plt.types.Char.prototype.getValue = function() {
	return this.val;
    };

    plt.types.Char.prototype.isEqual = function(other, aUnionFind){
	return other instanceof plt.types.Char && this.val == other.val;
    };
    
    // Symbols


    plt.types.Symbol = function(val) {
	this.val = val;
	this._eqHashCode = plt.types.makeEqHashCode();
    };

    var symbolCache = {};
    
    // makeInstance: string -> Symbol.
    plt.types.Symbol.makeInstance = function(val) {
	// To ensure that we can eq? symbols with equal values.
	if (!(val in symbolCache)) {
	    symbolCache[val] = new plt.types.Symbol(val);
	} else {
	}
	return symbolCache[val];
    };
    
    plt.types.Symbol.prototype.isEqual = function(other, aUnionFind) {
	return other instanceof plt.types.Symbol &&
	    this.val == other.val;
    };
    

    plt.types.Symbol.prototype.toString = function() {
        return this.val;
    };

    plt.types.Symbol.prototype.toWrittenString = function(cache) {
	return this.val;
    };

    plt.types.Symbol.prototype.toDisplayedString = function(cache) {
	return this.val;
    };

    
    
    
    plt.types.Empty = function() {
	this._eqHashCode = plt.types.makeEqHashCode();
    };
    plt.types.Empty.EMPTY = new plt.types.Empty();


    plt.types.Empty.prototype.isEqual = function(other, aUnionFind) {
	return other instanceof plt.types.Empty;
    };

    plt.types.Empty.prototype.first = function() {
	throw new MobyRuntimeError("first can't be applied on empty.");
    };
    plt.types.Empty.prototype.rest = function() {
	throw new MobyRuntimeError("rest can't be applied on empty.");
    };
    plt.types.Empty.prototype.isEmpty = function() {
	return true;
    };
    plt.types.Empty.prototype.toWrittenString = function(cache) { return "empty"; };
    plt.types.Empty.prototype.toDisplayedString = function(cache) { return "empty"; };



    
    // Empty.append: (listof X) -> (listof X)
    plt.types.Empty.prototype.append = function(b){
	return b;
    }
    
    plt.types.Cons = function(f, r) {
	this.f = f;
	this.r = r;
	this._eqHashCode = plt.types.makeEqHashCode();
    };
    
    plt.types.Cons.makeInstance = function(f, r) {
	return new plt.types.Cons(f, r);
    };


    // FIXME: can we reduce the recursion on this?
    plt.types.Cons.prototype.isEqual = function(other, aUnionFind) {
	if (! (other instanceof plt.types.Cons)) {
	    return plt.types.Logic.FALSE;
	}
	return (plt.Kernel.isEqual(this.first(), other.first(), aUnionFind) &&
		plt.Kernel.isEqual(this.rest(), other.rest(), aUnionFind));
    };
    
    plt.types.Cons.prototype.first = function() {
	return this.f;
    };
    
    plt.types.Cons.prototype.rest = function() {
	return this.r;
    };
    
    plt.types.Cons.prototype.isEmpty = function() {
	return false;
    };
    
    // Cons.append: (listof X) -> (listof X)
    plt.types.Cons.prototype.append = function(b){
	if (b.isEmpty())
	    return this;
	var ret = b;
	var lst = plt.Kernel.reverse(this);
	while (!lst.isEmpty()){
	    ret = plt.types.Cons.makeInstance(lst.first(), ret);
	    lst = lst.rest();
	}
	
	return ret;
    };
    
    plt.types.Cons.prototype.toWrittenString = function(cache) {
	cache.put(this, true);
	var texts = [];
	var p = this;
	while (! p.isEmpty()) {
	    texts.push(plt.Kernel.toWrittenString(p.first(), cache));
	    p = p.rest();
	}
	return "(" + texts.join(" ") + ")";
    };


    plt.types.Cons.prototype.toDisplayedString = function(cache) {
	cache.put(this, true);
	var texts = [];
	var p = this;
	while (! p.isEmpty()) {
	    texts.push(plt.Kernel.toDisplayedString(p.first(), cache));
	    p = p.rest();
	}
	return "(" + texts.join(" ") + ")";
    };



    plt.types.Cons.prototype.toDomNode = function(cache) {
	cache.put(this, true);
	var node = document.createElement("div");
	node.appendChild(document.createTextNode("("));
	var p = this;
	while (! p.isEmpty()) {
	    appendChild(node, plt.Kernel.toDomNode(p.first(), cache));
	    p = p.rest();
	    if (! p.isEmpty()) {
		appendChild(node, document.createTextNode(" "));
	    }
	}
	node.appendChild(document.createTextNode(")"));
	return node;
    };



    //////////////////////////////////////////////////////////////////////

    plt.types.Vector = function(n, initialElements) {
	this.elts = new Array(n);
	if (initialElements) {
	    for (var i = 0; i < n; i++) {
		this.elts[i] = initialElements[i];
	    }
	} else {
	    for (var i = 0; i < n; i++) {
		this.elts[i] = undefined;
	    }
	}
	this._eqHashCode = plt.types.makeEqHashCode();
    };
    plt.types.Vector.makeInstance = function(n, elts) {
	return new plt.types.Vector(n, elts);
    }
    plt.types.Vector.prototype.length = function() {
	return this.elts.length;
    };
    plt.types.Vector.prototype.ref = function(k) {
	return this.elts[k];
    };
    plt.types.Vector.prototype.set = function(k, v) {
	this.elts[k] = v;
    };

    plt.types.Vector.prototype.isEqual = function(other, aUnionFind) {
	if (other != null && other != undefined && other instanceof plt.types.Vector) {
	    if (other.length() != this.length()) {
		return false
	    }
	    for (var i = 0; i <  this.length(); i++) {
		if (! plt.Kernel.isEqual(this.elts[i], other.elts[i], aUnionFind)) {
		    return false;
		}
	    }
	    return true;
	} else {
	    return false;
	}
    }

    plt.types.Vector.prototype.toWrittenString = function(cache) {
	cache.put(this, true);
	var texts = [];
	for (var i = 0; i < this.length(); i++) {
	    texts.push(plt.Kernel.toWrittenString(this.ref(i), cache));
	}
	return "#(" + texts.join(" ") + ")";
    };
    plt.types.Vector.prototype.toDisplayedString = function(cache) {
	cache.put(this, true);
	var texts = [];
	for (var i = 0; i < this.length(); i++) {
	    texts.push(plt.Kernel.toDisplayedStringString(this.ref(i), cache));
	}
	return "#(" + texts.join(" ") + ")";
    };

    plt.types.Vector.prototype.toDomNode = function(cache) {
	cache.put(this, true);
	var node = document.createElement("div");
	node.appendChild(document.createTextNode("#("));
	for (var i = 0; i < this.length(); i++) {
	    appendChild(node,
			plt.Kernel.toDomNode(this.ref(i), cache));
	}
	node.appendChild(document.createTextNode(")"));
	return node;
    };


    //////////////////////////////////////////////////////////////////////


    // _gcd: fixnum fixnum -> fixnum
    var _gcd = function(a, b) {
	while (b != 0) {
	    var t = a;
	    a = b;
	    b = t % b;
	}
	return Math.abs(a);
    }

    // _lcm: fixnum fixnum -> integer
    var _lcm = function(a, b) {
	return Math.abs(a * b / _gcd(a, b));
    }

    // FIXME: there are two definitions of gcd floating around: which one is right?


    //////////////////////////////////////////////////////////////////////



    
    // Rationals
    
    var gcd = function(a, b) {
	var t;
	if (isNaN(a) || !isFinite(a)) {
	    throw new MobyRuntimeError("not a number: " + a);
	}
	if (isNaN(b) || !isFinite(b)) {
	    throw new MobyRuntimeError("not a number: " + b);
	}
	while (b != 0) {
	    t = a;
	    a = b;
	    b = t % b;
	}
	return a;
    }
    
    plt.types.Rational = function(n, d) {
	if (d == undefined) { d = 1; }
	if (d == 0) {
	    throw new MobyRuntimeError("cannot have zero denominator.");
	}
	var divisor = gcd(Math.abs(n), Math.abs(d));
	this.n = n / divisor;
	this.d = d / divisor;
	this._eqHashCode = plt.types.makeEqHashCode();
    };

    
    plt.types.Rational.prototype.toWrittenString = function(cache) {
	if (this.d == 1) {
	    return this.n + "";
	} else {
	    return this.n + "/" + this.d;
	}
    };

    plt.types.Rational.prototype.toDisplayedString = plt.types.Rational.prototype.toWrittenString;

    
    plt.types.Rational.prototype.level = function() {
	return 0;
    };
    
    
    plt.types.Rational.prototype.lift = function(target) {
	if (target.level() == 1)
	    return FloatPoint.makeInstance(this.n / this.d);
	if (target.level() == 2)	
	    return plt.types.Complex.makeInstance(this, 
						  plt.types.Rational.ZERO);
	throw new MobyRuntimeError("invalid level of Number");
    };
    
    plt.types.Rational.prototype.isFinite = function() {
	return true;
    };

    plt.types.Rational.prototype.isEqual = function(other, aUnionFind) {
	return this.equals(other);
    };

    plt.types.Rational.prototype.equals = function(other) {
	return other instanceof plt.types.Rational &&
	    this.n == other.n &&
	    this.d == other.d;
    };


    plt.types.Rational.prototype.isInteger = function() { 
	return this.d == 1;
    }
    
    plt.types.Rational.prototype.isRational = function() {
        return true;
    };
    
    plt.types.Rational.prototype.isReal = function() {
	return true;
    };

    
    plt.types.Rational.prototype.add = function(other) {
	return plt.types.Rational.makeInstance(this.n * other.d + 
					       this.d * other.n,
					       this.d * other.d);
    };
    
    plt.types.Rational.prototype.subtract = function(other) {
	return plt.types.Rational.makeInstance((this.n * other.d) - 
					       (this.d * other.n),
					       (this.d * other.d));
    };
    
    plt.types.Rational.prototype.multiply = function(other) {
	return plt.types.Rational.makeInstance(this.n * other.n,
					       this.d * other.d);
    };
    
    plt.types.Rational.prototype.divide = function(other) {
	if (this.d * other.n == 0) {
	    throw new MobyRuntimeError("division by zero");
	}
	return plt.types.Rational.makeInstance(this.n * other.d,
					       this.d * other.n);
    };
    

    plt.types.Rational.prototype.toExact = function() { 
	return this;
    };

    plt.types.Rational.prototype.isExact = function() {
        return true;
    };
    
    plt.types.Rational.prototype.toFixnum = function() {
	return Math.floor(this.n / this.d);  
    };

    plt.types.Rational.prototype.numerator = function() {
	return plt.types.Rational.makeInstance(this.n);
    };

    plt.types.Rational.prototype.denominator = function() {
	return plt.types.Rational.makeInstance(this.d);
    };
    
    plt.types.Rational.prototype.toFloat = function() {
	return this.n / this.d;
    };
    
    plt.types.Rational.prototype.toComplex = function(){
	return plt.types.Complex.makeInstance(this, plt.types.Rational.ZERO);
    };
    
    plt.types.Rational.prototype.greaterThan = function(other) {
	return this.n*other.d > this.d*other.n;
    };

    plt.types.Rational.prototype.greaterThanOrEqual = function(other) {
	return this.n*other.d >= this.d*other.n;
    };
    
    plt.types.Rational.prototype.lessThan = function(other) {
	return this.n*other.d < this.d*other.n;
    };

    plt.types.Rational.prototype.lessThanOrEqual = function(other) {
	return this.n*other.d <= this.d*other.n;
    };

    
    plt.types.Rational.prototype.sqrt = function() {
	if (this.n >= 0) {
	    var newN = Math.sqrt(this.n);
	    var newD = Math.sqrt(this.d);
	    if (Math.floor(newN) == newN &&
		Math.floor(newD) == newD) {
		return plt.types.Rational.makeInstance(newN, newD);
	    } else {
		return FloatPoint.makeInstance(newN / newD);
	    }
	} else {
	    var newN = Math.sqrt(- this.n);
	    var newD = Math.sqrt(this.d);
	    if (Math.floor(newN) == newN &&
		Math.floor(newD) == newD) {
		return plt.types.Complex.makeInstance(
		    plt.types.Rational.ZERO,
		    plt.types.Rational.makeInstance(newN, newD));
	    } else {
		return plt.types.Complex.makeInstance(
		    plt.types.Rational.ZERO,
		    FloatPoint.makeInstance(newN / newD));
	    }
	}
    };
    
    plt.types.Rational.prototype.abs = function() {
	return plt.types.Rational.makeInstance(Math.abs(this.n),
					       this.d);
    };
    
    plt.types.Rational.prototype.floor = function() {
	return plt.types.Rational.makeInstance(Math.floor(this.n / this.d), 1);
    };
    
    
    plt.types.Rational.prototype.ceiling = function() {
	return plt.types.Rational.makeInstance(Math.ceil(this.n / this.d), 1);
    };
    
    plt.types.Rational.prototype.conjugate = plt.types.Rational.prototype.abs;
    
    plt.types.Rational.prototype.magnitude = plt.types.Rational.prototype.abs;
    
    plt.types.Rational.prototype.log = function(){
	return FloatPoint.makeInstance(Math.log(this.n / this.d));
    };
    
    plt.types.Rational.prototype.angle = function(){
	if (0 == this.n)
	    return plt.types.Rational.ZERO;
	if (this.n > 0)
	    return plt.types.Rational.ZERO;
	else
	    return FloatPoint.pi;
    };
    
    plt.types.Rational.prototype.atan = function(){
	return FloatPoint.makeInstance(Math.atan(this.n / this.d));
    };
    
    plt.types.Rational.prototype.cos = function(){
	return FloatPoint.makeInstance(Math.cos(this.n / this.d));
    };
    
    plt.types.Rational.prototype.sin = function(){
	return FloatPoint.makeInstance(Math.sin(this.n / this.d));
    };
    
    plt.types.Rational.prototype.expt = function(a){
	return FloatPoint.makeInstance(Math.pow(this.n / this.d, a.n / a.d));
    };
    
    plt.types.Rational.prototype.exp = function(){
	return FloatPoint.makeInstance(Math.exp(this.n / this.d));
    };
    
    plt.types.Rational.prototype.acos = function(){
	return FloatPoint.makeInstance(Math.acos(this.n / this.d));
    };
    
    plt.types.Rational.prototype.asin = function(){
	return FloatPoint.makeInstance(Math.asin(this.n / this.d));
    };
    
    plt.types.Rational.prototype.imag_dash_part = function(){
	return plt.types.Rational.ZERO;
    };
    
    plt.types.Rational.prototype.real_dash_part = function(){
	return this;
    };


    plt.types.Rational.prototype.timesI = function() {
	return plt.types.Complex.makeInstance(plt.types.Rational.ZERO, this);
    };
    
    plt.types.Rational.prototype.round = function() {
	if (this.d == 2) {
	    // Round to even if it's a n/2
	    var v = this.n / this.d;
	    var fl = Math.floor(v);
	    var ce = Math.ceil(v);
	    if (fl % 2 == 0) { 
		return plt.types.Rational.makeInstance(fl); 
	    }
	    else { 
		return plt.types.Rational.makeInstance(ce); 
	    }
	} else {
	    return plt.types.Rational.makeInstance(Math.round(this.n / this.d));
	}
    };
    
    
    plt.types.Rational.prototype.half = function(){
	return plt.types.Rational.makeInstance(this.n, this.d * 2);
    };
    
    plt.types.Rational.prototype.minus = function(){
	return plt.types.Rational.makeInstance(0 - this.n, this.d);
    };
    
    
    var _rationalCache = {};
    plt.types.Rational.makeInstance = function(n, d) {
	if (n == undefined)
	    throw new MobyRuntimeError("n undefined");

	if (d == undefined) { d = 1; }

	
	if (d < 0) {
	    n = -n;
	    d = -d;
	}
	
	if (d == 1 && n in _rationalCache) {
	    return _rationalCache[n];
	}
	else {
	    return new plt.types.Rational(n, d);
	}
    };
    
    _rationalCache = {};
    (function() {
	var i;
	for(i = -500; i < 500; i++) {
	    _rationalCache[i] = new plt.types.Rational(i, 1);
	}
    })();
    plt.types.Rational.NEGATIVE_ONE = _rationalCache[-1];
    plt.types.Rational.ZERO = _rationalCache[0];
    plt.types.Rational.ONE = _rationalCache[1];
    plt.types.Rational.TWO = _rationalCache[2];
    
    
    
    
    
    
    var FloatPoint = function(n) {
	this.n = n;
	this._eqHashCode = plt.types.makeEqHashCode();
    };
    plt.types.FloatPoint = FloatPoint;


    FloatPoint.makeInstance = function(n) {
	return new FloatPoint(n);
    };

    
    FloatPoint.pi = FloatPoint.makeInstance(Math.PI);
    FloatPoint.e = FloatPoint.makeInstance(Math.E);

    var NaN = FloatPoint.makeInstance(Number.NaN);
    var inf = FloatPoint.makeInstance(Number.POSITIVE_INFINITY);
    var neginf = FloatPoint.makeInstance(Number.NEGATIVE_INFINITY);



    FloatPoint.prototype.isFinite = function() {
	return isFinite(this.n);
    };


    FloatPoint.prototype.toExact = function() {
	return plt.types.Rational.makeInstance(Math.floor(this.n), 1);
    };

    FloatPoint.prototype.isExact = function() {
	return false;
    };


    FloatPoint.prototype.level = function() {
	return 1;
    };
    
    FloatPoint.prototype.lift = function(target) {
	return plt.types.Complex.makeInstance(this, plt.types.Rational.ZERO);
    };
    
    FloatPoint.prototype.toWrittenString = function(cache) {
	if (this.n == Number.POSITIVE_INFINITY) {
	    return "+inf.0";
	} else if (this.n == Number.NEGATIVE_INFINITY) {
	    return "-inf.0";
	} else if (this.n == Number.NaN) {
	    return "+nan.0";
	} else {
	    return this.n.toString();
	}
    };
    
    FloatPoint.prototype.toDisplayedString = FloatPoint.prototype.toWrittenString;


    FloatPoint.prototype.isEqual = function(other, aUnionFind) {
	return this.equals(other);
    };

    FloatPoint.prototype.equals = function(other) {
	return ((other instanceof FloatPoint) &&
		((this.n == other.n) ||
		 (isNaN(this.n) && isNaN(other.n))));
    };


    FloatPoint.prototype.isRational = function() {
        return this.isFinite() && this.n == Math.floor(this.n);
    };

    FloatPoint.prototype.isInteger = function() {
	return this.isFinite() && this.n == Math.floor(this.n);
    };

    FloatPoint.prototype.isReal = function() {
	return true;
    };
    

    // sign: plt.types.Number -> {-1, 0, 1}
    var sign = function(n) {
	if (NumberTower.lessThan(n, plt.types.Rational.ZERO)) {
	    return -1;
	} else if (NumberTower.greaterThan(n, plt.types.Rational.ZERO)) {
	    return 1;
	} else {
	    return 0;
	}
    };


    FloatPoint.prototype.add = function(other) {
	if (this.isFinite() && other.isFinite()) {
	    return FloatPoint.makeInstance(this.n + other.n);
	} else {
	    if (isNaN(this.n) || isNaN(other.n)) {
		return NaN;
	    } else if (this.isFinite() && ! other.isFinite()) {
		return other;
	    } else if (!this.isFinite() && other.isFinite()) {
		return this;
	    } else {
		return ((sign(this) * sign(other) == 1) ?
			this : NaN);
	    };
	}
    };
    
    FloatPoint.prototype.subtract = function(other) {
	if (this.isFinite() && other.isFinite()) {
	    return FloatPoint.makeInstance(this.n - other.n);
	} else if (isNaN(this.n) || isNaN(other.n)) {
	    return NaN;
	} else if (! this.isFinite() && ! other.isFinite()) {
	    if (sign(this) == sign(other)) {
		return NaN;
	    } else {
		return this;
	    }
	} else if (this.isFinite()) {
	    return NumberTower.multiply(other, plt.types.Rational.NEGATIVE_ONE);
	} else {  // other.isFinite()
	    return this;
	}

    };
    
    FloatPoint.prototype.multiply = function(other) {
	if (this.n == 0 || other.n == 0) { return plt.types.Rational.ZERO; }

	if (this.isFinite() && other.isFinite()) {
	    return FloatPoint.makeInstance(this.n * other.n);
	} else if (isNaN(this.n) || isNaN(other.n)) {
	    return NaN;
	} else {
	    return ((sign(this) * sign(other) == 1) ? inf : neginf);
	}
    };
    
    FloatPoint.prototype.divide = function(other) {
	if (this.isFinite() && other.isFinite()) {
	    if (other.n == 0) {
		throw new MobyRuntimeError("division by zero");
	    }
            return FloatPoint.makeInstance(this.n / other.n);
	} else if (isNaN(this.n) || isNaN(other.n)) {
	    return NaN;
	} else if (! this.isFinite() && !other.isFinite()) {
	    return NaN;
	} else if (this.isFinite() && !other.isFinite()) {
	    return FloatPoint.makeInstance(0.0);
	} else if (! this.isFinite() && other.isFinite()) {
	    return ((sign(this) * sign(other) == 1) ? inf : neginf);
	}

    };
    
    
    FloatPoint.prototype.toFixnum = function() {
	return Math.floor(this.n);  
    };
    
    FloatPoint.prototype.numerator = function() {
	var stringRep = this.n.toString();
	var match = stringRep.match(/^(.*)\.(.*)$/);
	if (match) {
	    return FloatPoint.makeInstance(parseFloat(match[1] + match[2]));
	} else {
	    return this;
	}
    };

    FloatPoint.prototype.denominator = function() {
	var stringRep = this.n.toString();
	var match = stringRep.match(/^(.*)\.(.*)$/);
	if (match) {
	    return FloatPoint.makeInstance(Math.pow(10, match[2].length));
	} else {
	    return FloatPoint.makeInstance(1.0);
	}
    };


    FloatPoint.prototype.toFloat = function() {
	return this.n;
    };
    
    FloatPoint.prototype.toComplex = function(){
	return plt.types.Complex.makeInstance(this, plt.types.Rational.ZERO);
    };
    
    FloatPoint.prototype.floor = function() {
	if (! isFinite(this.n)) {
	    return this;
	}
	return plt.types.Rational.makeInstance(Math.floor(this.n), 1);
    };
    
    FloatPoint.prototype.ceiling = function() {
	if (! isFinite(this.n)) {
	    return this;
	}
	return plt.types.Rational.makeInstance(Math.ceil(this.n), 1);
    };
    


    FloatPoint.prototype.greaterThan = function(other) {
	return this.n > other.n;
    };
    
    FloatPoint.prototype.greaterThanOrEqual = function(other) {
	return this.n >= other.n;
    };
    
    FloatPoint.prototype.lessThan = function(other) {
	return this.n < other.n;
    };
    
    FloatPoint.prototype.lessThanOrEqual = function(other) {
	return this.n <= other.n;
    };

    
    FloatPoint.prototype.sqrt = function() {
	if (this.n < 0) {
	    var result = plt.types.Complex.makeInstance(
		plt.types.Rational.ZERO,
		FloatPoint.makeInstance(Math.sqrt(-this.n)));
	    return result;
	} else {
	    return FloatPoint.makeInstance(Math.sqrt(this.n));
	}
    };
    
    FloatPoint.prototype.abs = function() {
	return FloatPoint.makeInstance(Math.abs(this.n));
    };
    

    
    FloatPoint.prototype.log = function(){
	if (this.n < 0)
	    return this.toComplex().log();
	else
	    return FloatPoint.makeInstance(Math.log(this.n));
    };
    
    FloatPoint.prototype.angle = function(){
	if (0 == this.n)
	    return plt.types.Rational.ZERO;
	if (this.n > 0)
	    return plt.types.Rational.ZERO;
	else
	    return FloatPoint.pi;
    };
    
    FloatPoint.prototype.atan = function(){
	return FloatPoint.makeInstance(Math.atan(this.n));
    };
    
    FloatPoint.prototype.cos = function(){
	return FloatPoint.makeInstance(Math.cos(this.n));
    };
    
    FloatPoint.prototype.sin = function(){
	return FloatPoint.makeInstance(Math.sin(this.n));
    };
    
    FloatPoint.prototype.expt = function(a){
	if (this.n == 1) {
	    if (a.isFinite()) {
		return this;
	    } else if (isNaN(a.n)){
		return this;
	    } else {
		return this;
	    }
	} else {
	    return FloatPoint.makeInstance(Math.pow(this.n, a.n));
	}
    };
    
    FloatPoint.prototype.exp = function(){
	return FloatPoint.makeInstance(Math.exp(this.n));
    };
    
    FloatPoint.prototype.acos = function(){
	return FloatPoint.makeInstance(Math.acos(this.n));
    };
    
    FloatPoint.prototype.asin = function(){
	return FloatPoint.makeInstance(Math.asin(this.n));
    };
    
    FloatPoint.prototype.imag_dash_part = function(){
	return plt.types.Rational.ZERO;
    };
    
    FloatPoint.prototype.real_dash_part = function(){
	return this;
    };
    
    
    FloatPoint.prototype.round = function(){
	if (isFinite(this.n)) {
	    if (Math.abs(Math.floor(this.n) - this.n) == 0.5) {
		if (Math.floor(this.n) % 2 == 0)
		    return plt.types.Rational.makeInstance(Math.floor(this.n));
		return plt.types.Rational.makeInstance(Math.ceil(this.n));
	    } else {
		return plt.types.Rational.makeInstance(Math.round(this.n));
	    }
	} else {
	    return this;
	}	
    };
    
    
    FloatPoint.prototype.conjugate = FloatPoint.prototype.abs;
    
    FloatPoint.prototype.magnitude = FloatPoint.prototype.abs;
    
    FloatPoint.prototype.minus = function(){
	return FloatPoint.makeInstance(0 - this.n);
    };
    
    FloatPoint.prototype.half = function(){
	return FloatPoint.makeInstance(this.n / 2);
    };	
    
    FloatPoint.prototype.timesI = function(){
	return plt.types.Complex.makeInstance(plt.types.Rational.ZERO, this);
    };
    

    plt.types.Complex = function(r, i){
	this.r = r;
	this.i = i;
	this._eqHashCode = plt.types.makeEqHashCode();
    };
    
    // Constructs a complex number from two basic number r and i.  r and i can
    // either be plt.type.Rational or plt.type.FloatPoint.
    plt.types.Complex.makeInstance = function(r, i){
	if (typeof(r) == 'number') {
	    r = (r == Math.floor(r) ? plt.types.Rational.makeInstance(r) :
		 FloatPoint.makeInstance(r));
	}
	if (typeof(i) == 'number') {
	    i = (i == Math.floor(i) ? plt.types.Rational.makeInstance(i) :
		 FloatPoint.makeInstance(i));
	}

	var result = new plt.types.Complex(r, i);
	return result;
    };
    
    plt.types.Complex.prototype.toWrittenString = function(cache) {
	if (NumberTower.greaterThanOrEqual(
	    this.i,
	    plt.types.Rational.ZERO)) {
        return plt.Kernel.toWrittenString(this.r) + "+" + plt.Kernel.toWrittenString(this.i)+"i";
	} else {
            return plt.Kernel.toWrittenString(this.r) + plt.Kernel.toWrittenString(this.i)+"i";
	}
    };

    plt.types.Complex.prototype.toDisplayedString = plt.types.Complex.prototype.toWrittenString;



    plt.types.Complex.prototype.isFinite = function() {
	return this.r.isFinite() && this.i.isFinite();
    }


    plt.types.Complex.prototype.isRational = function() {
	return this.r.isRational() && NumberTower.equal(this.i, plt.types.Rational.ZERO);
    };

    plt.types.Complex.prototype.isInteger = function() {
	return this.r.isInteger() && NumberTower.equal(this.i, plt.types.Rational.ZERO);
    };

    plt.types.Complex.prototype.toExact = function() { 
	if (! this.isReal()) {
	    throw new MobyRuntimeError("inexact->exact: expects argument of type real number");
	}
	return this.r.toExact();
    };

    plt.types.Complex.prototype.isExact = function() { 
        return this.r.isExact() && this.i.isExact();
    };



    plt.types.Complex.prototype.level = function(){
	return 2;
    };
    
    plt.types.Complex.prototype.lift = function(target){
	throw new MobyRuntimeError("Don't know how to lift Complex number");
    };
    
    plt.types.Complex.prototype.isEqual = function(other, aUnionFind){
	return this.equals(other);
    };

    plt.types.Complex.prototype.equals = function(other) {
	var result = ((other instanceof plt.types.Complex) && 
		      (NumberTower.equal(this.r, other.r)) &&
		      (NumberTower.equal(this.i, other.i)));
	return result;
    };


    plt.types.Complex.prototype.greaterThan = function(other) {
	if (! this.isReal() || ! other.isReal()) {
	    throw new MobyRuntimeError(">: expects argument of type real number");
	}
	return NumberTower.greaterThan(this.r, other.r);
    };

    plt.types.Complex.prototype.greaterThanOrEqual = function(other) {
	if (! this.isReal() || ! other.isReal()) {
	    throw new MobyRuntimeError(">: expects argument of type real number");
	}
	return NumberTower.greaterThanOrEqual(this.r, other.r);
    };

    plt.types.Complex.prototype.lessThan = function(other) {
	if (! this.isReal() || ! other.isReal()) {
	    throw new MobyRuntimeError(">: expects argument of type real number");
	}
	return NumberTower.lessThan(this.r, other.r);
    };

    plt.types.Complex.prototype.lessThanOrEqual = function(other) {
	if (! this.isReal() || ! other.isReal()) {
	    throw new MobyRuntimeError(">: expects argument of type real number");
	}
	return NumberTower.lessThanOrEqual(this.r, other.r);
    };


    plt.types.Complex.prototype.abs = function(){
	if (!NumberTower.equal(this.i, plt.types.Rational.ZERO).valueOf())
	    throw new MobyRuntimeError("abs: expects argument of type real number");
	return this.r.abs();
    };
    
    plt.types.Complex.prototype.toFixnum = function(){
	if (!NumberTower.equal(this.i, plt.types.Rational.ZERO).valueOf())
	    throw new MobyRuntimeError("toFixnum: expects argument of type real number");
	return this.r.toFixnum();
    };

    plt.types.Complex.prototype.numerator = function() {
	if (!this.isReal())
	    throw new MobyRuntimeError("numerator: can only be applied to real number");
	return this.n.numerator();
    };
    

    plt.types.Complex.prototype.denominator = function() {
	if (!this.isReal())
	    throw new MobyRuntimeError("floor: can only be applied to real number");
	return this.n.denominator();
    };

    
    plt.types.Complex.prototype.toFloat = function(){
	if (!NumberTower.equal(this.i, plt.types.Rational.ZERO).valueOf())
	    throw new MobyRuntimeError("toFloat: expects argument of type real number");
	return this.r.toFloat();
    };
    
    plt.types.Complex.prototype.toComplex = function(){
	return this;
    };
    
    plt.types.Complex.prototype.add = function(other){
	return plt.types.Complex.makeInstance(
	    NumberTower.add(this.r, other.r),
	    NumberTower.add(this.i, other.i));
    };
    
    plt.types.Complex.prototype.subtract = function(other){
	return plt.types.Complex.makeInstance(
	    NumberTower.subtract(this.r, other.r),
	    NumberTower.subtract(this.i, other.i));
    };
    
    plt.types.Complex.prototype.multiply = function(other){

	// If the other value is real, just do primitive division
	if (other.isReal()) {
	    return plt.types.Complex.makeInstance(
		NumberTower.multiply(this.r, other.r),
		NumberTower.multiply(this.i, other.r));
	}

	var r = NumberTower.subtract(
	    NumberTower.multiply(this.r, other.r),
	    NumberTower.multiply(this.i, other.i));
	var i = NumberTower.add(
	    NumberTower.multiply(this.r, other.i),
	    NumberTower.multiply(this.i, other.r));
	if (NumberTower.equal(i, plt.types.Rational.ZERO)) {
	    return r;
	}
	return plt.types.Complex.makeInstance(r, i);
    };
    
    plt.types.Complex.prototype.divide = function(other){
	// If the other value is real, just do primitive division
	if (other.isReal()) {
	    return plt.types.Complex.makeInstance(
		NumberTower.divide(this.r, other.r),
		NumberTower.divide(this.i, other.r));
	}


	var con = other.conjugate();
	var up =  NumberTower.multiply(this, con).toComplex();

	// Down is guaranteed to be real by this point.
	var down = NumberTower.multiply(other, con);

	var result = plt.types.Complex.makeInstance(
	    NumberTower.divide(up.r, down),
	    NumberTower.divide(up.i, down));
	return result;
    };
    
    plt.types.Complex.prototype.conjugate = function(){
	var result = plt.types.Complex.makeInstance(
	    this.r, 
	    NumberTower.subtract(plt.types.Rational.ZERO, 
					   this.i));

	return result;
    };
    
    plt.types.Complex.prototype.magnitude = function(){
	var sum = NumberTower.add(
	    NumberTower.multiply(this.r, this.r),
	    NumberTower.multiply(this.i, this.i));
	return sum.sqrt();
    };
    
    plt.types.Complex.prototype.isReal = function(){
	return NumberTower.equal(this.i, plt.types.Rational.ZERO);
    };
    
    plt.types.Complex.prototype.sqrt = function(){
	if (this.isReal())
	    return this.r.sqrt();
	// http://en.wikipedia.org/wiki/Square_root#Square_roots_of_negative_and_complex_numbers	
	var r_plus_x = NumberTower.add(this.magnitude(), this.r);

	var r = r_plus_x.half().sqrt();

	var i = NumberTower.divide(this.i, NumberTower.multiply(r_plus_x, FloatPoint.makeInstance(2)).sqrt());
	

	return plt.types.Complex.makeInstance(r, i);
    };
    
    plt.types.Complex.prototype.log = function(){
	var m = this.magnitude();
	var theta = this.angle();
	var result = NumberTower.add(
	    m.log(),
	    theta.timesI());
	return result;
    };
    
    plt.types.Complex.prototype.angle = function(){
	if (this.isReal()) {
	    return this.r.angle();
	}
	if (NumberTower.equal(plt.types.Rational.ZERO, this.r)) {
	    var tmp = FloatPoint.pi.half();
	    return NumberTower.greaterThan(this.i, plt.types.Rational.ZERO) ? tmp : tmp.minus();
	} else {
	    var tmp = NumberTower.divide(this.i.abs(), this.r.abs()).atan();
	    if (NumberTower.greaterThan(this.r, plt.types.Rational.ZERO)) {
		return NumberTower.greaterThan(this.i, plt.types.Rational.ZERO) ? tmp : tmp.minus();
	    } else {
		return NumberTower.greaterThan(this.i, plt.types.Rational.ZERO) ? FloatPoint.pi.subtract(tmp) : tmp.subtract(FloatPoint.pi);
	    }
	}
    };
    
    var plusI = plt.types.Complex.makeInstance(plt.types.Rational.ZERO,
					       plt.types.Rational.ONE);
    var minusI = plt.types.Complex.makeInstance(plt.types.Rational.ZERO,
						plt.types.Rational.NEGATIVE_ONE);
    
    plt.types.Complex.prototype.atan = function(){
	if (NumberTower.equal(this, plusI) ||
	    NumberTower.equal(this, minusI)) {
	    return FloatPoint.makeInstance(Number.NEGATIVE_INFINITY);
	}
	return NumberTower.multiply(
	    plusI,
	    NumberTower.multiply(
		FloatPoint.makeInstance(0.5),
		(NumberTower.divide(
		    NumberTower.add(plusI, this),
		    NumberTower.add(
			plusI,
			NumberTower.subtract(plt.types.Rational.ZERO, this)))).log()));
    };
    
    plt.types.Complex.prototype.cos = function(){
	if (this.isReal())
	    return this.r.cos();
	var iz = this.timesI();
	var iz_minus = iz.minus();
	
	return NumberTower.add(iz.exp(), iz_minus.exp()).half();
    };
    
    plt.types.Complex.prototype.sin = function(){
	if (this.isReal())
	    return this.r.sin();
	var iz = this.timesI();
	var iz_minus = iz.minus();
	var z2 = plt.types.Complex.makeInstance(plt.types.Rational.ZERO,
						plt.types.Rational.TWO);
	var exp_minus = NumberTower.subtract(iz.exp(), iz_minus.exp());
	var result = NumberTower.divide(exp_minus, z2);
	return result;
    };
    
    plt.types.Complex.prototype.expt= function(y){
	var expo = NumberTower.multiply(y, this.log());
	return expo.exp();
    };
    
    plt.types.Complex.prototype.exp = function(){
	var r = this.r.exp();
	var cos_a = this.i.cos();
	var sin_a = this.i.sin();

	return NumberTower.multiply(
	    r,
	    NumberTower.add(cos_a, sin_a.timesI()));
    };
    
    plt.types.Complex.prototype.acos = function(){
	if (this.isReal())
	    return this.r.acos();
	var pi_half = FloatPoint.pi.half();
	var iz = this.timesI();
	var root = NumberTower.subtract(plt.types.Rational.ONE, this.multiply(this)).sqrt();
	var l = NumberTower.add(iz, root).log().timesI();
	return NumberTower.add(pi_half, l);
    };
    
    plt.types.Complex.prototype.asin = function(){
	if (this.isReal())
	    return this.r.asin();

	var oneMinusThisSq = 
	    NumberTower.subtract(
		plt.types.Rational.ONE, 
		this.multiply(this));
	var sqrtOneMinusThisSq = oneMinusThisSq.sqrt();
	return NumberTower.multiply(
	    plt.types.Rational.TWO,
	    (NumberTower.divide(
		this, 
		NumberTower.add(
		    plt.types.Rational.ONE,
		    sqrtOneMinusThisSq))).atan());
    };
    
    plt.types.Complex.prototype.ceiling = function(){
	if (!this.isReal())
	    throw new MobyRuntimeError("ceiling: can only be applied to real number");
	return this.r.ceiling();
    };
    
    plt.types.Complex.prototype.floor = function(){
	if (!this.isReal())
	    throw new MobyRuntimeError("floor: can only be applied to real number");
	return this.r.floor();
    };
    
    plt.types.Complex.prototype.imag_dash_part = function(){
	return this.i;
    };
    
    plt.types.Complex.prototype.real_dash_part = function(){
	return this.r;
    };
    
    plt.types.Complex.prototype.round = function(){
	return this.r.round();
    };
    
    
    plt.types.Complex.prototype.timesI = function(){
	return this.multiply(plt.types.Complex.makeInstance(plt.types.Rational.ZERO, plt.types.Rational.ONE));
    };
    
    plt.types.Complex.prototype.minus = function(){
	return plt.types.Complex.makeInstance(NumberTower.subtract(plt.types.Rational.ZERO, this.r),
					      NumberTower.subtract(plt.types.Rational.ZERO, this.i));
    };
    
    plt.types.Complex.prototype.half = function(){
	return plt.types.Complex.makeInstance(this.r.half(), 
					      this.i.half());
    };
    
    //////////////////////////////////////////////////////////////////////
    // NumberTower.
    // 
    var NumberTower = {};
    plt.types.NumberTower = NumberTower;

    
    NumberTower.toFixnum = function(num) {
	return num.toFixnum();
    };
    
    NumberTower.toFloat = function(num) {
	return num.toFloat();
    };
    
    NumberTower.abs = function(n) {
	return n.abs();
    };
    
    NumberTower.isFinite = function(n) {
	return n.isFinite();
    }

    NumberTower.toExact = function(x) {
	return x.toExact();
    };

    NumberTower.add = function(x, y) {
	if (x.level() < y.level()) x = x.lift(y);
	if (y.level() < x.level()) y = y.lift(x);
	return x.add(y);
    };
    
    NumberTower.subtract = function(x, y) {
	if (x.level() < y.level()) x = x.lift(y);
	if (y.level() < x.level()) y = y.lift(x);
	return x.subtract(y);
    };
    
    NumberTower.multiply = function(x, y) {
	if (x.level() < y.level()) x = x.lift(y);
	if (y.level() < x.level()) y = y.lift(x);
	return x.multiply(y);
    };
    
    NumberTower.divide = function(x, y) {
	if (x.level() < y.level()) x = x.lift(y);
	if (y.level() < x.level()) y = y.lift(x);
	return x.divide(y);
    };
    
    NumberTower.equal = function(x, y) {
	if (x.level() < y.level()) x = x.lift(y);
	if (y.level() < x.level()) y = y.lift(x);
	
	return x.equals(y);
    };
    
    NumberTower.approxEqual = function(x, y, delta) {
	return NumberTower.lessThan(NumberTower.abs(NumberTower.subtract(x, y)),
                                              delta);
    };
    
    NumberTower.greaterThanOrEqual = function(x, y){
	if (x.level() < y.level()) x = x.lift(y);
	if (y.level() < x.level()) y = y.lift(x);

	if (!(x.isReal() && y.isReal()))
	    throw new MobyRuntimeError("greaterThanOrEqual: couldn't be applied to complex number");
	return x.greaterThanOrEqual(y);
    };
    
    NumberTower.lessThanOrEqual = function(x, y){
	if (x.level() < y.level()) x = x.lift(y);
	if (y.level() < x.level()) y = y.lift(x);
	if (!(x.isReal() && y.isReal()))
	    throw new MobyRuntimeError("lessThanOrEqual: couldn't be applied to complex number");
	return x.lessThanOrEqual(y);    	
    };
    
    NumberTower.greaterThan = function(x, y){
	if (x.level() < y.level()) x = x.lift(y);
	if (y.level() < x.level()) y = y.lift(x);
	
	if (!(x.isReal() && y.isReal()))
	    throw new MobyRuntimeError("greaterThan: couldn't be applied to complex number");
	return x.greaterThan(y);
	
    };
    
    NumberTower.lessThan = function(x, y){
	if (x.level() < y.level()) x = x.lift(y);
	if (y.level() < x.level()) y = y.lift(x);

	if (!(x.isReal() && y.isReal()))
	    throw new MobyRuntimeError("lessThan: couldn't be applied to complex number");
	return x.lessThan(y);
    };
    
    NumberTower.modulo = function(m, n) {
	var result = 
	    plt.types.Rational.makeInstance(m.toFixnum() % n.toFixnum(),
					    1);

	// The sign of the result should match the sign of n.
	if (NumberTower.lessThan(n, plt.types.Rational.ZERO)) {
	    if (NumberTower.lessThanOrEqual(result, plt.types.Rational.ZERO)) {
		return result;
	    }
	    return NumberTower.add(result, n);

	} else {
	    if (NumberTower.lessThan(result, plt.types.Rational.ZERO)) {
		return NumberTower.add(result, n);
	    }
	    return result;
	}
    };
    
    NumberTower.sqr = function(x) {
	return NumberTower.multiply(x, x);
    };
    
    NumberTower.expt = function(x, y){
	if (x.level() < y.level()) x = x.lift(y);
	if (y.level() < x.level()) y = y.lift(x);
	return x.expt(y);
    };
    

    // gcd: number [number ...] -> number
    NumberTower.gcd = function(first, rest) {
	var result = Math.abs(first.toFixnum());
	for (var i = 0; i < rest.length; i++) {
	    result = _gcd(result, rest[i].toFixnum());
	}
	return plt.types.Rational.makeInstance(result);	
    };

    // lcm: number [number ...] -> number
    NumberTower.lcm = function(first, rest) {
	var result = Math.abs(first.toFixnum());
	if (result == 0) { return plt.types.Rational.ZERO; }
	for (var i = 0; i < rest.length; i++) {
	    if (rest[i].toFixnum() == 0) {
		return plt.types.Rational.ZERO;
	    }
	    result = _lcm(result, rest[i].toFixnum());
	}
	return plt.types.Rational.makeInstance(result);
    };



    // Strings
    // For the moment, we just reuse Javascript strings.
    plt.types.String = String;
    plt.types.String.makeInstance = function(s) {
	return s.valueOf();
    };
    
    
    // WARNING
    // WARNING: we are extending the built-in Javascript string class here!
    // WARNING
    plt.types.String.prototype.isEqual = function(other, aUnionFind){
	return this == other;
    };
    
    var _quoteReplacingRegexp = new RegExp("[\"\\\\]", "g");
    plt.types.String.prototype.toWrittenString = function(cache) {
    	return '"' + this.replace(_quoteReplacingRegexp,
    	                       function(match, submatch, index) {
                                       return "\\" + match;
                                   }) + '"';
    }

    plt.types.String.prototype.toDisplayedString = function(cache) {
        return this;
    }






    //////////////////////////////////////////////////////////////////////
    var MobyError = function(msg) {
	this.msg = msg;
    }
    MobyError.prototype.name= 'MobyError';
    MobyError.prototype.toString = function () { return this.name + ": " + this.msg }


    var MobyParserError = function(msg, loc) {
	MobyError.call(this, msg);
	this.loc = loc;
    }
    MobyParserError.prototype = heir(MobyError.prototype);
    MobyParserError.prototype.name= 'MobyParserError';

    
    var MobySyntaxError = function(msg, stx) {
	MobyError.call(this, msg);
	this.stx = stx;
    }
    MobySyntaxError.prototype = heir(MobyError.prototype);
    MobySyntaxError.prototype.name= 'MobySyntaxError';


    var MobyTypeError = function(msg) {
	MobyError.call(this, msg);
    }
    MobyTypeError.prototype = heir(MobyError.prototype);
    MobyTypeError.prototype.name= 'MobyTypeError';



    var MobyRuntimeError = function(msg) {
	MobyError.call(this, msg);
    }
    MobyRuntimeError.prototype = heir(MobyError.prototype);
    MobyRuntimeError.prototype.name= 'MobyRuntimeError';


    
    var MobyTestingError = function(msg) {
	MobyError.call(this, msg);
    }
    MobyTestingError.prototype = heir(MobyRuntimeError.prototype);
    MobyTestingError.prototype.name= 'MobyTestingError';




    plt.types.MobyError = MobyError;
    plt.types.MobyParserError = MobyParserError;
    plt.types.MobySyntaxError = MobySyntaxError;
    plt.types.MobyTypeError = MobyTypeError;
    plt.types.MobyRuntimeError = MobyRuntimeError;
    plt.types.MobyTestingError = MobyTestingError;




    plt.types.Box = Box;
    

})();