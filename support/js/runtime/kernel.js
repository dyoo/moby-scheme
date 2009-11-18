if (typeof(plt) == 'undefined') { plt = {} }


//////////////////////////////////////////////////////////////////////
// Kernel
// Depends on types.js.
//
// FIXME: there's a circular dependency between types.js and
// kernel.js.  It hasn't bitten us yet only because the circular
// references are in functions, rather than toplevel expressions, but
// we need to be careful.


(function() {

    // Compatibility hack: add Array.indexOf if it doesn't exist.
    if(!Array.indexOf){
	Array.prototype.indexOf = function(obj){
	    for(var i=0; i<this.length; i++){
		if(this[i]==obj){
		    return i;
		}
	    }
	    return -1;
	}
    }
    

    // Compatibility for attaching events to nodes.
    var attachEvent = function(node, eventName, fn) {
	if (node.addEventListener) {
	    // Mozilla
	    node.addEventListener(eventName, fn, false);
	} else {
	    // IE
	    node.attachEvent('on' + eventName, fn, false);
	}
    };

    var detachEvent = function(node, eventName, fn) {
	if (node.addEventListener) {
	    // Mozilla
	    node.removeEventListener(eventName, fn, false);
	} else {
	    // IE
	    node.detachEvent('on' + eventName, fn, false);
	}
    }



    // Inheritance from pg 168: Javascript, the Definitive Guide.
    var heir = function(p) {
	var f = function() {}
	f.prototype = p;
	return new f();
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


    
    //////////////////////////////////////////////////////////////////////


    // _gcd: integer integer -> integer
    var _gcd = function(a, b) {
	while (b != 0) {
	    var t = a;
	    a = b;
	    b = t % b;
	}
	return Math.abs(a);
    }

    // _lcm: integer integer -> integer
    var _lcm = function(a, b) {
	return Math.abs(a * b / _gcd(a, b));
    }


    // Returns true if x is a number.
    var isNumber = function(x) {
	return (x != null && x != undefined && (x instanceof plt.types.Rational || 
						x instanceof plt.types.FloatPoint ||
						x instanceof plt.types.Complex));
    }

    var isSymbol = function(x) {
	return (x != null && x != undefined && x instanceof plt.types.Symbol);
    }

    var isChar = function(x) {
	return x != null && x != undefined && x instanceof plt.types.Char;
    }

    var isStx = function(x) {
	return stx_question_(x);
    }

    var isString = function(x) {
	return typeof(x) == 'string';
	//return x != null && x != undefined && x instanceof plt.types.String;
    }

    var isBoolean = function(x) {
	return (x == plt.types.Logic.TRUE || x == plt.types.Logic.FALSE);
    }

    var isPair = function(x) {
	return x != null && x != undefined && x instanceof plt.types.Cons;
    }

    var isEmpty = function(x) {
	return x != null && x != undefined && x instanceof plt.types.Empty;
    }

    var isReal = function(x) {
	return (isNumber(x) && x.isReal());

    }

    var isRational = function(x) {
	return isNumber(x) && x.isRational();
    }


    var isComplex = function(x) {
	return isNumber(x);}

    var isFunction = function(x) {
	return typeof(x) == 'function';
    }

    // Returns true if x is an integer.
    var isInteger = function(x) {
	return (isNumber(x) && x.isInteger());
    }

    var isNatural = function(x) {
	return isNumber(x) && x.isInteger() && x.toInteger() >= 0;
    }


    // isAlphabeticString: string -> boolean
    var isAlphabeticString = function(s) {
	for(var i = 0; i < s.length; i++) {
	    if (! ((s[i] >= "a" && s[i] <= "z") ||
		   (s[i] >= "A" && s[i] <= "Z"))) {
		return false;
	    }
	}
	return true;
    }

    // isWhitespaceString: string -> boolean
    var isWhitespaceString = (function() {
	var pat = new RegExp("^\\s*$");
	return function(s) {
	    return (s.match(pat) ? true : false);
	}
    }());




    // Returns true if x is a vector
    var isVector = function(x) {
	return x != null && x != undefined && (x instanceof plt.types.Vector);
    }



    // arrayEach: (arrayof X) (X -> void) -> void
    // Apply some function on each element of the array.
    var arrayEach = function(arr, f) {
	for (var i = 0; i < arr.length; i++) {
	    f.apply(arr[i], [arr[i], i]);
	}
    }


    // Apply a chaining test on pairs of elements of the list [first, second, rest ...].
    var chainTest = function(test, first, second, rest) {
	if (! test(first, second).valueOf())
	    return false;
	if (rest.length == 0)
	    return true;
	if (! test(second, rest[0]).valueOf())
	    return false;
	for(var i = 0; i < rest.length - 1; i++) {
	    if (! test(rest[i], rest[i+1]).valueOf())
		return false;
	}
	return true;
    }
    

    // Apply a search on pairs of elements of the list [first, rest ...].
    var chainFind = function(comparator, first, rest) {
	var i;
	var best = first;
	for(i = 0; i < rest.length; i++) {
	    if (! (comparator(best, rest[i])).valueOf()) {
		best = rest[i];
	    }
	}
	return best;
    }
    

    // Returns true if x is a list.
    var isList = function(x) {
	return x != null && x != undefined && ((x instanceof plt.types.Cons) || (x instanceof plt.types.Empty));
    }


    // ordinalize: number -> string
    // Adds the ordinal suffix to an number, according to the rules in
    // http://en.wikipedia.org/wiki/Names_of_numbers_in_English#Ordinal_numbers
    var ordinalize = function(n) {
	var suffixes = ["th", "st", "nd", "rd", "th",
			"th", "th", "th", "th", "th"];
	if ((Math.floor(n / 10) % 10) == 1) {
	    return n + "th";
	} else {
	    return n + suffixes[n % 10];
	}
    };


    var makeTypeErrorMessage = function(functionName, typeName, position, value) {
	return plt.Kernel.format(
	    "~a: expects type <~a> as ~a argument, given: ~s",
	    [functionName, 
	     typeName,
	     ordinalize(position),
	     value]);
    }


    // Checks if x satisfies f.  If not, a MobyTypeError of msg is thrown.
    var check = function(x, f, functionName, typeName, position) {
	if (! f(x)) {
	    throw new MobyTypeError(
		makeTypeErrorMessage(functionName, typeName, position, x));
	}
    }

    // Throws exception if x is not a list.
    var checkList = function(x, functionName, position) {
	if (! isList(x)) {
	    throw new MobyTypeError(
		makeTypeErrorMessage(functionName, "list", position, x));
	}
    }

    // Checks if x is a list of f.  If not, throws a MobyTypeError of msg.
    var checkListof = function(x, f, functionName, typeName, position) {
	if (! isList(x)) {
	    throw new MobyTypeError(
		makeTypeErrorMessage(functionName, "listof " + typeName, position, x));
	}
	while (! x.isEmpty()) {
	    if (! f(x.first())) {
		throw new MobyTypeError(makeTypeErrorMessage(functionName, "listof " + typeName, position, x));
	    }
	    x = x.rest();
	}
    }


    // makeChainingComparator: (X -> boolean) string (X X (arrayof X) -> boolean) -> (X X (arrayof X) -> boolean) 
    var makeChainingComparator = function(typeCheckF, typeName, comparisonF, comparatorName) {
	return function(first, second, rest) {
	    check(first, typeCheckF, comparatorName, typeName, 1);
	    check(second, typeCheckF, comparatorName, typeName, 2);
	    arrayEach(rest, 
		      function(x, i) { check(x, typeCheckF, comparatorName, typeName, i+3) });
	    return comparisonF(first, second, rest);
	}
    }



    var makeNumericChainingComparator = function(test, comparatorName) {
	return makeChainingComparator(isNumber, "number",
				      function(first, second, rest) {
					  return chainTest(test, first, second, rest);
				      },
				      comparatorName);
    }

    var makeCharChainingComparator = function(test, comparatorName) {
	return makeChainingComparator(isChar, "char",
				      function(first, second, rest) {
					  return chainTest(test, first, second, rest);
				      },
				      comparatorName);
    }


    var makeStringChainingComparator = function(test, comparatorName) {
	return makeChainingComparator(isString, "string",
				      function(first, second, rest) {
					  return chainTest(test, first, second, rest);
				      },
				      comparatorName);
    }




    plt.Kernel = {
	
	_heir : heir,


	pi : plt.types.FloatPoint.makeInstance(Math.PI),
	e : plt.types.FloatPoint.makeInstance(Math.E),


	Struct: function (constructorName, fields) {
	    this._constructorName = constructorName; 
	    this._fields = fields;
	},

	
	struct_question_: function(thing) {
	    return (thing != null && thing != undefined && thing instanceof plt.Kernel.Struct);
	},
	
	number_question_ : function(x){
	    return isNumber(x);
	},
	
	equal_question_ : function(x, y) {
	    if (plt.Kernel.number_question_(x) && 
		plt.Kernel.number_question_(y)) {
		if ("isEqual" in x) {
		    return plt.types.NumberTower.equal(x, y);
		} else if ("isEqual" in y) {
		    return plt.types.NumberTower.equal(y, x);
		} else {
		    return (x == y);
		}
	    } else {
		return x.isEqual(y);
	    }
	},


	equal_tilde__question_ : function(x, y, delta) {
	    check(delta, isNumber, "equal~?", "number", 3);
	    if (plt.Kernel.number_question_(x).valueOf() && 
		plt.Kernel.number_question_(y).valueOf()) {
		if ("isEqual" in x) {
		    return plt.types.NumberTower.approxEqual(x, y, delta);
		} else if ("isEqual" in y) {
		    return plt.types.NumberTower.approxEqual(y, x, delta);
		} else {
		    return (x == y);
		}
	    } else {
		return x.isEqual(y);
	    }
	},

	
	eq_question_ : function(x, y){
	    return (x == y);
	}, 
	

	eqv_question_ : function(x, y){
	    if (isNumber(x) && isNumber(y) && x.level() == y.level()) {
		return plt.types.NumberTower.equal(x, y);
	    } else if (isChar(x) && isChar(y)) {
		return x.getValue() == y.getValue();
	    }
	    return x === y;
	},
	

	identity : function (x){
	    return x;
	},
	
	
	cons: function(x, y) {
	    checkList(y, "cons", 2);
	    return plt.types.Cons.makeInstance(x, y);
	},
	
	first: function(thing) {
	    checkList(thing, "first", 1);
	    return thing.first();
	},
	
	rest: function(thing) {
	    checkList(thing, "rest", 1);
	    return thing.rest();
	},
	
	
	second: function(thing) {
	    checkList(thing, "second", 1);
	    return thing.rest().first();
	},
	
	third: function(thing) {
	    checkList(thing, "third", 1);
	    return thing.rest().rest().first();
	},
	
	fourth: function(thing) {
	    checkList(thing, "fourth", 1);
	    return thing.rest().rest().rest().first();
	},
	
	fifth: function(thing) {
	    checkList(thing, "fifth", 1);
	    return thing.rest().rest().rest().rest().first();
	},
	
	
	random: function(x) {
	    check(x, isInteger, "random", "integer", 1);
	    return plt.types.Rational.makeInstance(Math.floor(plt.types.NumberTower.toInteger(x) * 
							      Math.random()),
						   1);
	},
	
	current_dash_seconds: function () {
	    return plt.types.Rational.makeInstance(new Date().getMilliseconds() / 1000);	    
	},


	floor: function(x) {
	    check(x, isNumber, "floor", "number", 1);
	    return x.floor();
	},
	
	ceiling: function(x) {
	    check(x, isNumber, "ceiling", "number", 1);
	    return x.ceiling();
	},
	
	sqrt: function(x) {
	    check(x, isNumber, "sqrt", "number", 1);
	    return x.sqrt();
	},

	integer_dash_sqrt: function(x) {
	    check(x, isInteger, "integer-sqrt", "integer", 1);
	    var result = x.sqrt();
	    if (isRational(result)) {
		return plt.types.Rational.makeInstance(result.toInteger());
	    } else if (isReal(result)) {
		return plt.types.Rational.makeInstance(result.toInteger());
	    } else {

		// it must be complex.
		return plt.types.Complex.makeInstance(
		    plt.types.Rational.makeInstance
		    (plt.Kernel.real_dash_part(result).toInteger()),
		    plt.types.Rational.makeInstance
		    (plt.Kernel.imag_dash_part(result).toInteger()));
	    }
	},
	
	sqr: function(x) {
	    check(x, isNumber, "sqr", "number", 1);
	    return plt.types.NumberTower.sqr(x);
	},
	
	sin: function(x) {
	    check(x, isNumber, "sin", "number", 1);
	    return x.sin();
	},
	
	cos: function(x) {
	    check(x, isNumber, "cos", "number", 1);
	    return x.cos();
	},
	
	modulo: function(m, n) {
	    check(m, isNumber, "modulo", "number", 1);
	    check(n, isNumber, "modulo", "number", 2);
	    return plt.types.NumberTower.modulo(m, n);
	},
	
	zero_question_: function(m) {
	    check(m, isNumber, "zero?", "number", 1);
	    return plt.types.NumberTower.equal(m, plt.types.Rational.ZERO);
	},
	
	
	_equal__tilde_ : function(x, y, delta) {
	    check(x, isNumber, "=~", "number", 1);
	    check(y, isNumber, "=~", "number", 2);
	    check(delta, isNumber, "=~", "number", 3);
	    return plt.types.NumberTower.approxEqual(x, y, delta);
	},
	
	abs: function(x) {
	    check(x, isNumber, "abs", "number", 1);
	    return plt.types.NumberTower.abs(x);
	},
	
	add1 : function(x) {
	    check(x, isNumber, "add1", "number", 1);
	    return plt.types.NumberTower.add(x, plt.types.Rational.ONE);
	},
	
	
	sub1 : function(x) {
	    check(x, isNumber, "sub1", "number", 1);
	    return plt.types.NumberTower.subtract(x, plt.types.Rational.ONE);
	},
	
	
	_plus_ : function(args) {
	    arrayEach(args, function(x, i) { check(x, isNumber, "+", "number", i+1) });
	    var i, sum = plt.types.Rational.ZERO;
	    for(i = 0; i < args.length; i++) {
		sum = plt.types.NumberTower.add(sum, args[i]);
	    }
	    return sum;
	},
	

	_dash_ : function(first, args) {
	    check(first, isNumber, "-", "number", 1);
	    arrayEach(args, function(x, i) { check(x, isNumber, "-", "number", i+2) });
	    if (args.length == 0) {
		return plt.types.NumberTower.subtract
		(plt.types.Rational.ZERO, first);
	    }
	    
	    var i, diff = first;
	    for(i = 0; i < args.length; i++) {
		diff = plt.types.NumberTower.subtract(diff, args[i]);
	    }
	    return diff;
	},
	
	
	_star_ : function(args) {
	    arrayEach(args, function(x, i) { check(x, isNumber, "*", "number", i+1) });
	    var i, prod = plt.types.Rational.ONE;
	    for(i = 0; i < args.length; i++) {
		prod = plt.types.NumberTower.multiply(prod, args[i]);
	    }
	    return prod;    
	},
	
	
	_slash_ : function(first, args) {
	    check(first, isNumber, "/", "number", 1);
	    arrayEach(args, function(x, i) { check(x, isNumber, "/", "number", i+2) });
	    var i, div = first;
	    if (args.length == 0) {
		return plt.types.NumberTower.divide(plt.types.Rational.ONE, div);
	    } else {
		for(i = 0; i < args.length; i++) {
		    div = plt.types.NumberTower.divide(div, args[i]);
		}
		return div;    
	    }
	},
	

	_equal_ : makeNumericChainingComparator(plt.types.NumberTower.equal, "="),
	_greaterthan__equal_: makeNumericChainingComparator(plt.types.NumberTower.greaterThanOrEqual, ">="),
	_lessthan__equal_: makeNumericChainingComparator(plt.types.NumberTower.lessThanOrEqual, "<="),
	_greaterthan_: makeNumericChainingComparator(plt.types.NumberTower.greaterThan, ">"),
	_lessthan_: makeNumericChainingComparator(plt.types.NumberTower.lessThan, "<"),

	
	min : function(first, rest) {
	    check(first, isNumber, "min", "number", 1);
	    arrayEach(rest, function(x, i) { check(this, isNumber, "min", "number", i+2); });
	    return chainFind(plt.types.NumberTower.lessThanOrEqual,
			     first, 
			     rest);
	},
	
	max : function(first, rest) {
	    check(first, isNumber, "max", "number", 1);
	    arrayEach(rest, function(x, i) { check(this, isNumber, "max", "number", i+2); });
	    return chainFind(plt.types.NumberTower.greaterThanOrEqual,
			     first, 
			     rest);
	},
	

	lcm : function(first, rest) {
	    check(first, isInteger, "lcm", "number", 1);
	    arrayEach(rest, function(x, i) { check(this, isInteger, "lcm", "number", i+2); });
	    var result = Math.abs(first.toInteger());
	    if (result == 0) { return plt.types.Rational.ZERO; }
	    for (var i = 0; i < rest.length; i++) {
		if (rest[i].toInteger() == 0) {
		    return plt.types.Rational.ZERO;
		}
		result = _lcm(result, rest[i].toInteger());
	    }
	    return plt.types.Rational.makeInstance(result);
	},

	
	gcd : function(first, rest) {
	    check(first, isInteger, "gcd", "number", 1);
	    arrayEach(rest, function(x, i) { check(this, isInteger, "gcd", "number", i+2); });	    
	    var result = Math.abs(first.toInteger());
	    for (var i = 0; i < rest.length; i++) {
		result = _gcd(result, rest[i].toInteger());
	    }
	    return plt.types.Rational.makeInstance(result);
	},

	exact_dash__greaterthan_inexact: function(x) {
	    check(x, isNumber, "exact->inexact", "number", 1);
	    return plt.types.FloatPoint.makeInstance(x.toFloat());
	},
	
	inexact_dash__greaterthan_exact: function(x) {
	    check(x, isNumber, "inexact->exact", "number", 1);
	    return plt.types.NumberTower.toExact(x);
	},

	exact_question_ : function(x) {
	    check(x, isNumber, "exact?", "number", 1);
	    return x.isExact();
	},

	inexact_question_ : function(x) {
	    check(x, isNumber, "inexact?", "number", 1);
	    return ! x.isExact();
	},
	
	rational_question_ : function(x) {
	    return (plt.Kernel.number_question_(x) &&
		    x.isRational());
	},

	number_dash__greaterthan_string: function(x) {
	    check(x, isNumber, "number->string", "number", 1);
	    return plt.types.String.makeInstance(plt.Kernel.toWrittenString(x));
	},
	
	conjugate: function(x){
	    check(x, isNumber, "conjugate", "number", 1);
	    return x.conjugate();
	},
	
	magnitude: function(x){
	    check(x, isNumber, "magnitude", "number", 1);
	    return x.magnitude();
	},
	
	log : function(x) {
	    check(x, isNumber, "log", "number", 1);
	    return x.log();
	},
	
	angle : function(x) {
	    check(x, isNumber, "angle", "number", 1);
	    return x.angle();
	},
	
	atan : function(x, args) {
	    if (args.length == 0) {
		check(x, isNumber, "atan", "number", 1);
		return x.atan();
	    } else if (args.length == 1) {
		check(x, isReal, "atan", "number", 1);
		check(args[0], isReal, "atan", "number", 2);
		return plt.types.FloatPoint.makeInstance(
		    Math.atan2(plt.types.NumberTower.toFloat(x),
			       plt.types.NumberTower.toFloat(args[0])));
	    } else {
		// FIXME: this should really be an error at compile time.
		throw new MobyRuntimeError(plt.Kernel.format("atan: expects 1 to 2 arguments, given ~a.", [plt.types.Rational.makeInstance(args.length)]));
	    }
	},
	
	expt : function(x, y){
	    check(x, isNumber, "expt", "number", 1);
	    check(y, isNumber, "expt", "number", 2);
	    return plt.types.NumberTower.expt(x, y);
	},
	
	exp : function(x){
	    check(x, isNumber, "exp", "number", 1);
	    return x.exp();
	},
	
	acos : function(x){
	    check(x, isNumber, "acos", "number", 1);
	    return x.acos();
	},
	
	asin : function(x){
	    check(x, isNumber, "asin", "number", 1);
	    return x.asin();
	},
	
	tan : function(x){
	    check(x, isNumber, "tan", "number", 1);
	    return plt.types.NumberTower.divide(x.sin(), x.cos());
	},
	
	complex_question_ : function(x){
	    return isComplex(x);
	},
	
	cosh : function(x) {
	    check(x, isNumber, "cosh", "number", 1);
	    return this._plus_([this.exp(x), this.exp(x.minus())]).half();
	},
	
	sinh : function(x) {
	    check(x, isNumber, "sinh", "number", 1);
	    return plt.types.NumberTower.subtract(this.exp(x), this.exp(x.minus())).half();
	},
	
	denominator : function(x) {
	    check(x, isRational, "denominator", "rational", 1);
	    return x.denominator();
	},
	
	numerator : function(x){
	    check(x, isRational, "numerator", "rational", 1);
	    return x.numerator();
	},
	
	odd_question_ : function(x){
	    check(x, isNumber, "odd?", "number", 1);
	    return (Math.abs((x.toInteger() % 2)) == 1);
	},
	
	even_question_ : function(x) {
	    check(x, isNumber, "even?", "number", 1);
	    return (Math.abs((x.toInteger() % 2)) == 0);
	},
	
	positive_question_ : function(x){
	    check(x, isNumber, "positive?", "number", 1);
	    return this._greaterthan_(x, plt.types.Rational.ZERO, []);
	},
	
	negative_question_ : function(x){
	    check(x, isNumber, "negative?", "number", 1);
	    return this._lessthan_(x, plt.types.Rational.ZERO, []);
	},
	
	imag_dash_part : function(x){
	    check(x, isNumber, "imag-part", "number", 1);
	    return x.imag_dash_part();
	},
	
	real_dash_part : function(x){
	    check(x, isNumber, "real-part", "number", 1);
	    return x.real_dash_part();
	},
	

	make_dash_polar: function(r, theta) {
	    // special case: if theta is zero, just return
	    // the scalar.
	    if (plt.types.NumberTower.equal(theta, plt.types.Rational.ZERO)) {
		return r;
	    }
	    var x = plt.types.NumberTower.multiply(r, theta.cos());
	    var y = plt.types.NumberTower.multiply(r, theta.sin());
	    return plt.types.Complex.makeInstance(x, y);
	},

	integer_question_ : function(x){
	    // check(x, isNumber, "integer?", "number", 1);
	    return (isInteger(x));
	},
	
	make_dash_rectangular : function(x, y){
	    return plt.types.Complex.makeInstance(x, y);
	},
	
	quotient : function(x, y){
	    check(x, isInteger, "quotient", "integer", 1);
	    check(y, isInteger, "quotient", "integer", 2);
	    var div = plt.types.NumberTower.divide(x,y);
	    if (plt.Kernel.positive_question_(div)) {
		return plt.types.Rational.makeInstance(div.floor().toInteger(),
						       1);
	    } else {
		return plt.types.Rational.makeInstance(div.ceiling().toInteger(),
						       1);
	    }
	},
	
	remainder : function(x, y) {
	    check(x, isNumber, "remainder", "number", 1);
	    check(y, isNumber, "remainder", "number", 2);
	    return plt.types.Rational.makeInstance(x.toInteger() % y.toInteger(), 1);
	},
	

	real_question_ : function(x){
	    return isReal(x);
	},
	
	
	round : function(x){
	    check(x, isNumber, "round", "number", 1);
	    return x.round();
	},
	
	sgn : function(x){
	    check(x, isNumber, "sgn", "number", 1);
	    if (this.positive_question_(x).valueOf())
		return plt.types.Rational.ONE;
	    if (this.negative_question_(x).valueOf())
		return plt.types.Rational.NEGATIVE_ONE;
	    else
		return plt.types.Rational.ZERO;
	},
	


	boolean_equal__question_ : function(x, y){
	    check(x, isBoolean, "boolean=?", "boolean", 1);
	    check(y, isBoolean, "boolean=?", "boolean", 2);
	    return x == y;
	},
	
	boolean_question_ : function(x){
	    return isBoolean(x);
	},
	
	false_question_ : function(x){
	    return (x == plt.types.Logic.FALSE);
	},
	
	not : function(x){
	    // Restriction on x being a boolean has been weakened.
	    //check(x, isBoolean, "not", "boolean", 1);

	    if (!x || x === plt.types.Logic.FALSE)
		return plt.types.Logic.TRUE;
	    return plt.types.Logic.FALSE;

	    //return (!( x && x.valueOf() )) ? plt.types.Logic.TRUE : plt.types.Logic.FALSE;
	},
	
	symbol_dash__greaterthan_string : function(x){
	    check(x, isSymbol, "symbol->string", "symbol", 1);
	    return plt.types.String.makeInstance(x.val);
	},
	
	symbol_equal__question_ : function(x, y){
	    check(x, isSymbol, "symbol=?", "symbol", 1);
	    check(y, isSymbol, "symbol=?", "symbol", 2);
	    return (x.val == y.val);
	},
	
	symbol_question_ : function(x){
	    return isSymbol(x);
	},
	
	
	append : function(first, rest){
	    checkList(first, "append", 1);
            var ret = first;
	    var i;
	    for (i = 0; i < rest.length; i++) {
		checkList(rest[i], "append", i+2);
		ret = ret.append(rest[i]);
	    }
	    return ret;
	},
	
	reverse : function(lst){
	    checkList(lst, "reverse", 1);
	    var ret = plt.types.Empty.EMPTY;
	    while (!lst.isEmpty()){
		ret = plt.types.Cons.makeInstance(lst.first(), ret);
		lst = lst.rest();
	    }
	    
	    return ret;
	}, 
	
	assq : function(x, lst){
	    checkList(lst, "assq", 2);
	    while (!lst.isEmpty() && !plt.Kernel.eq_question_(x, lst.first().first()))
		lst = lst.rest();
	    if (lst.isEmpty())
		return plt.types.Logic.FALSE;
	    else return lst.first();
	},
	
	caaar : function(lst){
	    checkList(lst, "caaar", 1);
	    return lst.first().first().first();
	},
	
	caadr : function(lst){
	    checkList(lst, "caadr", 1);
	    return lst.first().first().rest();
	},
	
	caar : function(lst){
	    checkList(lst, "caar", 1);
	    return lst.first().first();
	},
	
	cadar : function(lst){
	    checkList(lst, "cadar", 1);
	    return lst.first().rest().first();
	},
	
	cadddr : function(lst){
	    checkList(lst, "cadddr", 1);
	    return lst.rest().rest().rest().first();
	},
	
	caddr : function(lst){
	    checkList(lst, "caddr", 1);
	    return lst.rest().rest().first();
	},
	
	cadr : function(lst){
	    checkList(lst, "cadr", 1);
	    return lst.rest().first();
	},
	
	car : function(lst){
	    checkList(lst, "car", 1);
	    return lst.first();
	},
	
	cdaar : function(lst){
	    checkList(lst, "cdaar", 1);
	    return lst.first().first().rest();
	},
	
	cdadr : function(lst){
	    checkList(lst, "cdadr", 1);
	    return lst.rest().first().rest();
	},
	
	cdar : function(lst){
	    checkList(lst, "cdar", 1);
	    return lst.first().rest();
	},
	
	cddar : function(lst){
	    checkList(lst, "cddar", 1);
	    return lst.first().rest().rest();
	},
	
	cdddr : function(lst){
	    checkList(lst, "cdddr", 1);
	    return lst.rest().rest().rest();
	},
	
	cddr : function(lst){
	    checkList(lst, "cddr", 1);
	    return lst.rest().rest();
	},
	
	cdr : function(lst){
	    checkList(lst, "cdr", 1);
	    return lst.rest();
	},

	null_question_ : function(x){
	    return isEmpty(x);
	},
	
	empty_question_: function(x) {
	    return isEmpty(x);
	},
	
	pair_question_ : function(x){
	    return isPair(x);
	},
	
	cons_question_: function(x){
	    return isPair(x);
	},

	list_question_: function(x) {
	    return isList(x);
	},

	
	sixth : function(lst){
	    checkList(lst, "sixth", 1);
	    return lst.rest().rest().rest().rest().rest().first();
	},
	
	seventh: function(lst){
	    checkList(lst, "seventh", 1);
	    return lst.rest().rest().rest().rest().rest().rest().first();
	},
	
	eighth : function(lst){
	    checkList(lst, "eighth", 1);
	    return lst.rest().rest().rest().rest().rest().rest().rest().first();
	},

	set_dash_car_bang_ : function(lst, newVal){
	    checkList(lst, "set-car!", 1);
	    lst.f = newVal;
	    return undefined;
	},

	set_dash_cdr_bang_ : function(lst, newListVal){
	    checkList(lst, "set-cdr!", 1);
	    checkList(newListVal, "set-cdr!", 2);
	    lst.r = newListVal;
	    return undefined;
	},
	
	length : function(lst){
	    checkList(lst, "length", 1);
	    var ret = plt.types.Rational.ZERO;
	    for (; !lst.isEmpty(); lst = lst.rest()) {
		ret = plt.Kernel.add1(ret);
	    }
	    return ret;
	},
	
	list : function(items){
	    var ret = plt.types.Empty.EMPTY;
	    for (var i = items.length - 1; i >=0; i--) {
		ret = plt.types.Cons.makeInstance(items[i], ret);
	    }
	    return ret;
	},
	
	list_star_ : function(items, otherItems){
	    var lastListItem = otherItems.pop();
	    if (lastListItem == undefined || ! lastListItem instanceof plt.types.Cons) {
		throw new MobyTypeError("list*: " + lastListItem + " not a list");
	    }
	    otherItems.unshift(items);
	    return plt.Kernel.append(plt.Kernel.list(otherItems), [lastListItem]);
	},
	
	list_dash_ref : function(lst, x){
	    checkList(lst, "list-ref", 1);
	    check(x, isNatural, "list-ref", "natural", 2);
	    var i = plt.types.Rational.ZERO;
	    for (; plt.Kernel._lessthan_(i, x,[]); i = plt.Kernel.add1(i)) {
		if (lst.isEmpty()) {
		    throw new MobyRuntimeError("list-ref: index too small");
		}
		else {
		    lst = lst.rest();
		}
	    }
	    return lst.first();
	},
	
	remove : function(item, lst){
	    checkList(lst, "member", 2);
	    var originalLst = lst;
	    var result = plt.types.Empty.EMPTY;
	    while (!lst.isEmpty()){
		if (plt.Kernel.equal_question_(item, lst.first()).valueOf()) {
		    return plt.Kernel.append(plt.Kernel.reverse(result),
					     [lst.rest()]);
		} else {
		    result = plt.types.Cons.makeInstance(lst.first(),
							 result);
		    lst = lst.rest();
		}
	    }
	    return originalLst;
	},

	member : function(item, lst){
	    checkList(lst, "member", 2);
	    while (!lst.isEmpty()){
		if (plt.Kernel.equal_question_(item, lst.first()).valueOf())
		    return plt.types.Logic.TRUE;
		lst = lst.rest();
	    }
	    
	    return plt.types.Logic.FALSE;
	},

	
	memq : function(item, lst){
	    checkList(lst, "memq", 2);
	    while (!lst.isEmpty()){
		if (plt.Kernel.eq_question_(item, lst.first()).valueOf())
		    return lst;
		lst = lst.rest();
	    }
	    
	    return plt.types.Logic.FALSE;
	},
	

	memv : function(item, lst){
	    checkList(lst, "memv", 2);
	    while (!lst.isEmpty()){
		if (plt.Kernel.eqv_question_(item, lst.first()).valueOf())
		    return lst;
		lst = lst.rest();
	    }
	    
	    return plt.types.Logic.FALSE;
	},


	memf : function(testF, lst) {
	    check(testF, isFunction, "memf", "function", 1);
	    checkList(lst, "memf", 2);
	    // TODO: add contract on higher order argument testF.    
	    while (!lst.isEmpty()){
		if (testF([lst.first()])) {
		    return lst;
		}
		lst = lst.rest();
	    }
	    return plt.types.Logic.FALSE;
	},


	compose: function(functions) {
	    // TODO: add contract on higher order argument testF.
	    return function(args) {
		var resultArray = args;
		for (var i = functions.length - 1; i >= 0; i--) {
		    resultArray = [functions[i](resultArray)];
		}
		return resultArray[0];
	    }
	},
	

	string_dash__greaterthan_number : function(str){
	    check(str, isString, "string->number", "string", 1);
	    try {
		var stxList = plt.reader.readSchemeExpressions(str, "");
		if (plt.types.NumberTower.equal(plt.Kernel.length(stxList),
						plt.types.Rational.ONE)) {
		    var result = stx_dash_e(stxList.first());
		    if (isNumber(result)) {
			return result;
		    } else {
			return plt.types.Logic.FALSE;
		    }
		} else {
		    return plt.types.Logic.FALSE;
		}
	    } catch (e) {
		return plt.types.Logic.FALSE;
	    }
// 	    var aNum = str * 1;
// 	    if (isNaN(aNum))
// 		return plt.types.Logic.FALSE;
// 	    if (Math.floor(aNum) == aNum && isFinite(aNum)) {
// 		return plt.types.Rational.makeInstance(aNum);
// 	    }
// 	    return plt.types.FloatPoint.makeInstance(aNum);
	},
	

	string_dash__greaterthan_symbol : function(str){
	    check(str, isString, "string->symbol", "string", 1);
	    return plt.types.Symbol.makeInstance(str);
	},


	string_dash__greaterthan_int: function(str) {
	    check(str, isString, "string->int", "string", 1);
	    return plt.types.Rational.makeInstance(str.toString().charCodeAt(0), 1);
	},

	
	string_dash_append : function(arr){
	    arrayEach(arr, function(x, i) { check(x, isString, "string-append", "string", i+1) });
            return plt.types.String.makeInstance(arr.join(""));
	},


	replicate: function(n, s) {
	    check(n, isNatural, "replicate", "natural", 1);
	    check(s, isString, "replicate", "string", 2);
	    var buffer = [];
	    for (var i = 0; i < n.toInteger(); i++) {
		buffer.push(s);
	    }
	    return plt.types.String.makeInstance(buffer.join(""));
	},

	
	string_equal__question_ : makeStringChainingComparator(
	    function(x, y){return x == y;}, "string=?"),
	

	string_lessthan__equal__question_: makeStringChainingComparator(
	    function(x, y){return x <= y;}, "string<=?"),


	string_lessthan__question_: makeStringChainingComparator(
	    function(x, y){return x < y;}, "string<?"),
	

	string_greaterthan__equal__question_: makeStringChainingComparator(
	    function(x, y){return x >= y;}, "string>=?"),
	

	string_greaterthan__question_: makeStringChainingComparator(
	    function(x, y){return x > y;}, "string>?"),
	

	string_dash_ci_equal__question_ : makeStringChainingComparator(
	    function(x, y){return x.toUpperCase() == y.toUpperCase();}, "string-ci=?"),
	

	string_dash_ci_lessthan__equal__question_ : makeStringChainingComparator(
	    function(x, y){return x.toUpperCase() <= y.toUpperCase();}, "string-ci<=?"),
	

	string_dash_ci_lessthan__question_ : makeStringChainingComparator(
	    function(x, y){return x.toUpperCase() < y.toUpperCase();}, "string-ci<?"),
	

	string_dash_ci_greaterthan__question_ : makeStringChainingComparator(
	    function(x, y){return x.toUpperCase() > y.toUpperCase();}, "string-ci>?"),
	

	string_dash_ci_greaterthan__equal__question_ : makeStringChainingComparator(
	    function(x, y){return x.toUpperCase() >= y.toUpperCase();}, "string-ci>=?"),
	

	string_dash_copy : function(str){
	    check(str, isString, "string-copy", "string", 1);
	    return str.substring(0, str.length);
	},
	
	string_dash_length : function(str){
	    check(str, isString, "string-length", "string", 1);
	    return plt.types.Rational.makeInstance(str.length, 1);
	},
	
	string_dash_ref : function(str, i){
	    check(str, isString, "string-ref", "string", 1);
	    check(i, isNatural, "string-ref", "natural", 2);
	    if (i.toInteger() >= str.length) {
		throw new MobyRuntimeError("string-ref: index >= length");
	    }
	    return plt.types.String.makeInstance(str.charAt(i.toInteger()));
	},

	string_dash_ith : function (str, i) {
	    check(str, isString, "string-ith", "string", 1);
	    check(i, isNatural, "string-ith", "natural", 2);
	    if (i.toInteger() >= str.length) {
		throw new MobyRuntimeError("string-ith: index >= string length");
	    }
	    return plt.types.String.makeInstance(str.substring(i.toInteger(), i.toInteger()+1));
	},

	int_dash__greaterthan_string: function (i) {
	    check(i, isInteger, "int->string", "integer", 1);
	    return plt.types.String.makeInstance(String.fromCharCode(i.toInteger()));
	},

	
	string_question_ : function(str){
	    return isString(str);
	},
	

	substring : function(str, begin, end){
	    check(str, isString, "substring", "string", 1);
	    check(begin, isNatural, "substring", "natural", 2);
	    check(end, isNatural, "substring", "natural", 3);
	    if (begin.toInteger() > end.toInteger()) {
		throw new MobyRuntimeError("substring: begin > end");
	    }
	    if (end.toInteger() > str.length) {
		throw new MobyRuntimeError("substring: end > length");
	    }
	    return String.makeInstance(str.substring(begin.toInteger(), end.toInteger()));
	},

	char_question_: function(x) {
	    return isChar(x);
	},
	
	char_dash__greaterthan_integer : function(ch){
	    check(ch, isChar, "char->integer", "char", 1);
	    var str = new String(ch.val);
	    return plt.types.Rational.makeInstance(str.charCodeAt(0), 1);
	},
	
	integer_dash__greaterthan_char : function(n){
	    check(n, isInteger, "integer->char", "integer", 1);
	    var str = String.fromCharCode(n.toInteger());
	    return plt.types.Char.makeInstance(str);
	},
	
	
	char_equal__question_ : makeCharChainingComparator(
	    function(x, y) { return x.val == y.val; }, "char=?"),
	
	char_lessthan__question_ : makeCharChainingComparator(
	    function(x, y){ return x.val < y.val; }, "char<?"),
	
	
	char_lessthan__equal__question_ : makeCharChainingComparator(
	    function(x, y){ return x.val <= y.val; }, "char<=?"),

	
	char_greaterthan__question_ : makeCharChainingComparator(
	    function(x, y){ return x.val > y.val; }, "char>?"),
	
	char_greaterthan__equal__question_ : makeCharChainingComparator(
	    function(x, y){ return x.val >= y.val; }, "char>=?"),
	
	char_dash_ci_equal__question_ : makeCharChainingComparator(
	    function(x, y){ return x.val.toUpperCase() == y.val.toUpperCase(); }, "char-ci=?"),

	char_dash_ci_lessthan__question_ : makeCharChainingComparator(
	    function(x, y){ return x.val.toUpperCase() < y.val.toUpperCase(); }, "char-ci<?"),


	char_dash_ci_lessthan__equal__question_ : makeCharChainingComparator(
	    function(x, y){ return x.val.toUpperCase() <= y.val.toUpperCase(); }, "char-ci<=?"),
	
	char_dash_ci_greaterthan__question_ : makeCharChainingComparator(
	    function(x, y){ return x.val.toUpperCase() > y.val.toUpperCase(); }, "char-ci>?"),

	
	char_dash_ci_greaterthan__equal__question_ : makeCharChainingComparator(
	    function(x, y){ return x.val.toUpperCase() >= y.val.toUpperCase(); }, "char-ci>=?"),
	
	
	char_dash_numeric_question_ : function(ch){
	    check(ch, isChar, "char-numeric?", "char", 1);
	    var str = ch.val;
	    return (str >= "0" && str <= "9");
	},

	char_dash_alphabetic_question_ : function(ch){
	    check(ch, isChar, "char-alphabetic?", "char", 1);
	    var str = ch.val;
	    return isAlphabeticString(str);
	},

	char_dash_whitespace_question_ : function(ch){
	    check(ch, isChar, "char-whitespace?", "char", 1);
	    var str = ch.val;
	    return isWhitespaceString(str);
	},

	char_dash_upper_dash_case_question_ : function(ch){
	    check(ch, isChar, "char-upper-case?", "char", 1);
	    return isAlphabeticString(ch.val) && ch.val.toUpperCase() == ch.val;
	},
	
	char_dash_lower_dash_case_question_ : function(ch){
	    check(ch, isChar, "char-lower-case?", "char", 1);
	    return isAlphabeticString(ch.val) && ch.val.toLowerCase() == ch.val;
	},


	char_dash_upcase : function(ch){
	    check(ch, isChar, "char-upcase", "char", 1);
	    return plt.types.Char.makeInstance(ch.val.toUpperCase());
	},

	
	char_dash_downcase : function(ch){
	    check(ch, isChar, "char-downcase", "char", 1);
	    return plt.types.Char.makeInstance(ch.val.toLowerCase());
	},
	

	
	// list->string: (listof char) -> string
	list_dash__greaterthan_string : function(lst){
	    checkListof(lst, isChar, "list->string", "char", 1);
	    var ret = "";
	    while (!lst.isEmpty()){
		ret += lst.first().val;
		lst = lst.rest();
	    }
	    return plt.types.String.makeInstance(ret);
	},

	implode: function(lst) {
	    checkListof(lst, isString, "implode", "string", 1);
	    var ret = [];
	    while (!lst.isEmpty()){
		ret.push(lst.first().toString());
		lst = lst.rest();
	    }
	    return plt.types.String.makeInstance(ret.join(""));
	},
	



	string_dash_numeric_question_: function(s) {
	    check(s, isString, "string-numeric?", "string", 1);
	    for (var i = 0 ; i < s.length; i++) {
		if (s[i] < '0' || s[i] > '9') {
		    return plt.types.Logic.FALSE;
		}
	    }
	    return plt.types.Logic.TRUE;
	},


	string_dash_alphabetic_question_: function(s) {
	    check(s, isString, "string-alphabetic?", "string", 1);
	    return isAlphabeticString(s) ? plt.types.Logic.TRUE : plt.types.Logic.FALSE;
	},


	string_dash_whitespace_question_: function(s) {
	    check(s, isString, "string-whitespace?", "string", 1);
	    return isWhitespaceString(s) ? plt.types.Logic.TRUE : plt.types.Logic.FALSE;
	},


	string_dash_upper_dash_case_question_: function(s) {
	    check(s, isString, "string-upper-case?", "string", 1);
	    return isAlphabeticString(s) && s.toUpperCase() == s;
	},


	string_dash_lower_dash_case_question_: function(s) {
	    check(s, isString, "string-lower-case?", "string", 1);
	    return isAlphabeticString(s) && s.toLowerCase() == s;
	},


	string : function(chars) {
	    arrayEach(chars, function(x, i) { check(this, isChar, "string", "char", i+1); });
	    var buffer = [];
	    for(var i = 0; i < chars.length; i++) {
		buffer.push(chars[i].val);
	    }
	    return String.makeInstance(buffer.join(""));
	},


	make_dash_string : function(n, ch){
	    check(n, isNatural, "make-string", "natural", 1);
	    check(ch, isChar, "make-string", "char", 2);
	    var ret = "";
	    var c = ch.val;
	    var i = plt.types.Rational.ZERO;
	    for (;  plt.Kernel._lessthan_(i, n, []); i = plt.Kernel.add1(i)) {
		ret += c;
	    }
	    return plt.types.String.makeInstance(ret);
	},
	
	string_dash__greaterthan_list : function(str){
	    check(str, isString, "string->list", "string", 1);
	    var s = str;
	    var ret = plt.types.Empty.EMPTY;
	    for (var i = s.length - 1; i >= 0; i--) {
		ret = plt.types.Cons.makeInstance
		(plt.types.Char.makeInstance(s.charAt(i)),
		 ret);
	    }
	    return ret;
	},


	explode: function (str) {
	    check(str, isString, "explode", "string", 1);
	    var s = str;
	    var ret = plt.types.Empty.EMPTY;
	    for (var i = s.length - 1; i >= 0; i--) {
		ret = plt.types.Cons.makeInstance
		(plt.types.String.makeInstance(s.charAt(i)),
		 ret);
	    }
	    return ret;	    
	}



	
    };
    



    // DEBUGGING: get out all the functions defined in the kernel.
    plt.Kernel._dumpKernelSymbols = function() {
	var result = plt.types.Empty.EMPTY;
	for (var sym in plt.Kernel) {
	    result = plt.types.Cons.makeInstance(plt.types.Symbol.makeInstance(sym),
						 result);
	}
	return result;
    };



    // open-input-stx: string -> (listof stx)
    plt.Kernel.openInputStx = function(path) {
	// Doesn't do anything here.
	throw new MobyRuntimeError("open-input-stx currently unsupported");
    };


    //////////////////////////////////////////////////////////////////////
    var EqHashTable = function(inputHash) {
	this.hash = new plt._Hashtable(function(x) { return plt.Kernel.toWrittenString(x); },
				       function(x, y) { return x === y; });
    };
    EqHashTable.prototype.toWrittenString = function(cache) {
	return "<hash>";
    };
    EqHashTable.prototype.toDisplayedString = function(cache) {
	return "<hash>";
    };

    EqHashTable.prototype.isEqual = function(other) {
	return this === other;
    };

    var EqualHashTable = function(inputHash) {
	this.hash = new plt._Hashtable(function(x) { return plt.Kernel.toWrittenString(x); },
				       function(x, y) { return plt.Kernel.equal_question_(x, y); });
    };
    EqualHashTable.prototype.toWrittenString = function(cache) {
	return "<hash>";
    };
    EqualHashTable.prototype.toDisplayedString = function(cache) {
	return "<hash>";
    };

    EqualHashTable.prototype.isEqual = function(other) {
	return this === other;
    };


    // makeHashEq: -> hash
    plt.Kernel.makeHashEq = function() {
	var myhash = new EqHashTable();
	return myhash;
    };

    plt.Kernel.makeHash = function() {
	return new EqualHashTable();
    };

    // plt.Kernel.hashSet: hash object value -> undefined
    // Mutates the hash with a new key/value binding.
    plt.Kernel.hashSetBang = function(obj, key, val) {
	check(obj, isHash, "hash-set!", "hash", 1);
	obj.hash.put(key, val);
	return undefined;
    };

    plt.Kernel.hashRef = function(obj, key, defaultVal) {
	check(obj, isHash, "hash-ref", "hash", 1);
	if (obj.hash.containsKey(key)) {
	    return obj.hash.get(key);
	} else {
	    if (isFunction(defaultVal)) {
		return defaultVal([]);
	    }
	    return defaultVal;
	}
    };
    
    plt.Kernel.hashRemoveBang = function(obj, key) {
	check(obj, isHash, "hash-remove!", "hash", 1);
	obj.hash.remove(key);
	return undefined;
    };

    plt.Kernel.hashMap = function(ht, f) {
	check(ht, isHash, "hash-map", "hash", 1);
	var result = plt.types.Empty.EMPTY;
	var keys = ht.hash.keys();
	for (var i = 0; i < keys.length; i++){
	    var val = ht.hash.get(keys[i]);
	    result = plt.Kernel.cons(f([keys[i], val]),
				     result);
	}
	return result;
    };


    plt.Kernel.hashForEach = function(ht, f) {
	check(ht, isHash, "hash-map", "hash", 1);
	var result = plt.types.Empty.EMPTY;
	var keys = ht.hash.keys();
	for (var i = 0; i < keys.length; i++){
	    var val = ht.hash.get(keys[i]);
	    f([keys[i], val]);
	}
	return undefined;
    };

    var isHash = function(x) {
	return ((x != null) && 
		(x != undefined) && 
		((x instanceof EqHashTable) || (x instanceof EqualHashTable)));
    }
    plt.Kernel.isHash = isHash;



    //////////////////////////////////////////////////////////////////////

    

    plt.Kernel.apply = function(f, secondArg, restArgs) {
	var argList;
	var argArray = [];

	check(f, isFunction, "apply", "function", 1);
	if (restArgs.length == 0) {
	    argList = secondArg;
	    checkList(argList, "apply", 2);
	    while (! argList.isEmpty()) {
		var elt = argList.first()
		argArray.push(elt);
		argList = argList.rest();
	    }	
	} else {
	    argList = restArgs.pop();
	    checkList(argList, "apply", 3);
	    while (! argList.isEmpty()) {
		var elt = argList.first()
		argArray.push(elt);
		argList = argList.rest();
	    }	
	    while(restArgs.length > 0) {
		argArray.unshift(restArgs.pop());
	    }
	    argArray.unshift(secondArg);

	}
	if (procedureArityIncludes(f, argArray.length)) {
	    return f(argArray);
	} else {
	    throw new MobyRuntimeError(
		plt.Kernel.format(
		    "~a: expects ~a, given ~a: ~s", 
		    [f,
		     procedureArityDescription(f),
		     argArray.length,
		     plt.Kernel.list(argArray)]));
	}
    };


    plt.Kernel.map = function(f, arglists) {
	check(f, isFunction, "map", "function", 1);
	arrayEach(arglists, function(x, i) { 
	    checkList(x, "map", i+2); });
	// TODO: add contract on higher order argument f.
	var results = plt.types.Empty.EMPTY;
	while (!arglists[0].isEmpty()) {
	    var args = [];
	    for (var i = 0; i < arglists.length; i++) {
		args.push(arglists[i].first());
		arglists[i] = arglists[i].rest();
	    }
	    results = plt.Kernel.cons(f(args), results);
	}
	return plt.Kernel.reverse(results);
    };


    plt.Kernel.for_dash_each = function(f, arglists) {
	check(f, isFunction, "for-each", "function", 1);
	arrayEach(arglists, function(x, i) { 
	    checkList(x, "for-each", i+2); });
	// TODO: add contract on higher order argument f.
	while (!arglists[0].isEmpty()) {
	    var args = [];
	    for (var i = 0; i < arglists.length; i++) {
		args.push(arglists[i].first());
		arglists[i] = arglists[i].rest();
	    }
	    f(args);
	}
	return undefined;
    };



    plt.Kernel.andmap = function(f, arglists) {
	check(f, isFunction, "andmap", "function", 1);
	arrayEach(arglists, function(x, i) { 
	    checkList(x, "andmap", i+2); });

	// TODO: add contract on higher order argument f.
	while (!arglists[0].isEmpty()) {
	    var args = [];
	    for (var i = 0; i < arglists.length; i++) {
		args.push(arglists[i].first());
		arglists[i] = arglists[i].rest();
	    }
	    if (! f(args)) {
		return plt.types.Logic.FALSE;
	    }
	}

	return plt.types.Logic.TRUE;
    };



    plt.Kernel.ormap = function(f, arglists) {
	check(f, isFunction, "ormap", "function", 1);
	arrayEach(arglists, function(x, i) { 
	    checkList(x, "ormap", i+2);});
	// TODO: add contract on higher order argument f.
	while (! arglists[0].isEmpty()) {
	    var args = [];
	    for (var i = 0; i < arglists.length; i++) {
		args.push(arglists[i].first());
		arglists[i] = arglists[i].rest();
	    }
	    if (f(args)) {
		return plt.types.Logic.TRUE;
	    }
	}
	return plt.types.Logic.FALSE;
    };





    plt.Kernel.filter = function(f, elts) {
	check(f, isFunction, "filter", "function", 1);
	check(elts, isList, "filter", "list", 2);
	// TODO: add contract on higher order argument f.
	var results = plt.types.Empty.EMPTY;
	while (! elts.isEmpty()) {
	    if (f([elts.first()])) {
		results = plt.types.Cons.makeInstance(elts.first(), results);
	    }
	    elts = elts.rest();
	}
	return plt.Kernel.reverse(results);
    };


    plt.Kernel.foldl = function(f, acc, arglists) {
	check(f, isFunction, "foldl", "function", 1);
	arrayEach(arglists, function(x, i) { check(x, isList, "foldl", "list", i+3)});
	// TODO: add contract on higher order argument f.
	var result = acc;
	while (!arglists[0].isEmpty()) {
	    var args = [];
	    for (var i = 0; i < arglists.length; i++) {
		args.push(arglists[i].first());
		arglists[i] = arglists[i].rest();
	    }
	    args.push(result);
	    result = f(args);
	}
	return result;
    };


    plt.Kernel.foldr = function(f, acc, arglists) {
	check(f, isFunction, "foldr", "function", 1);
	arrayEach(arglists, function(x, i) { check(x, isList, "foldr", "list", i+3)});
	// TODO: add contract on higher order argument f.
	var result = acc;
	for (var i = 0; i < arglists.length; i++) {
	    arglists[i] = plt.Kernel.reverse(arglists[i]);
	}
	while (!arglists[0].isEmpty()) {
	    var args = [];
	    for (var i = 0; i < arglists.length; i++) {
		args.push(arglists[i].first());
		arglists[i] = arglists[i].rest();
	    }
	    args.push(result);
	    result = f(args);
	}
	return result;
    };



    plt.Kernel.argmin = function(f, elts) {
	check(f, isFunction, "argmin", "function", 1);
	check(elts, isPair, "argmin", "nonempty list", 2);
	// TODO: add contract on higher order argument f.
	var bestSoFar = elts.first();
	var bestMetric = f([elts.first()]).toFloat();
	elts = elts.rest();

	while (! elts.isEmpty()) {
	    var nextMetric = f([elts.first()]).toFloat();
	    if (nextMetric < bestMetric) {
		bestSoFar = elts.first();
		bestMetric = nextMetric;
	    }
	    elts = elts.rest();
	}
	return bestSoFar;
    };


    plt.Kernel.argmax = function(f, elts) {
	check(f, isFunction, "argmax", "function", 1);
	check(elts, isPair, "argmax", "nonempty list", 2);
	// TODO: add contract on higher order argument f.
	var bestSoFar = elts.first();
	var bestMetric = f([elts.first()]).toFloat();
	elts = elts.rest();

	while (! elts.isEmpty()) {
	    var nextMetric = f([elts.first()]).toFloat();
	    if (nextMetric > bestMetric) {
		bestSoFar = elts.first();
		bestMetric = nextMetric;
	    }
	    elts = elts.rest();
	}
	return bestSoFar;
    };






    plt.Kernel.sort = function(l, cmpF) {
	check(l, isList, "sort", "list", 1);
	check(cmpF, isFunction, "sort", "function", 2);

	// TODO: add contract on higher order argument cmpF.
	var arr = [];
	while(!l.isEmpty()) {
	    arr.push(l.first());
	    l = l.rest();
	}
	arr.sort(function(x, y) { return cmpF([x, y]) ? -1 : 1; });
	return plt.Kernel.list(arr);
    };

    plt.Kernel.quicksort = plt.Kernel.sort;



    plt.Kernel.build_dash_list = function(n, f) {
	check(n, isNatural, "build-list", "natural", 1);
	check(f, isFunction, "build-list", "function", 2);

	// TODO: add contract on higher order argument f.
	var result = plt.types.Empty.EMPTY;
	for(var i = 0; i < n.toInteger(); i++) {
	    result = plt.Kernel.cons(f([plt.types.Rational.makeInstance(i, 1)]),
				     result);
	}
	return plt.Kernel.reverse(result);
    };


    plt.Kernel.build_dash_string = function(n, f) {
	check(n, isNatural, "build-string", "natural", 1);
	check(f, isFunction, "build-string", "function", 2);

	// TODO: add contract on higher order argument f.
	var chars = [];
	for(var i = 0; i < n.toInteger(); i++) {
	    var ch = f([plt.types.Rational.makeInstance(i, 1)]);
	    //	    check(ch, isChar, "char");
	    chars.push(ch.val);
	}
	return plt.types.String.makeInstance(chars.join(""));
    };




    plt.Kernel.format = function(formatStr, args) {
	check(formatStr, isString, "format", "string", 1);
	var pattern = new RegExp("~[sSaAn%~]", "g");
	var buffer = args;
	function f(s) {
	    if (s == "~~") {
		return "~";
	    } else if (s == '~n' || s == '~%') {
		return "\n";
	    } else if (s == '~s' || s == "~S") {
		if (buffer.length == 0) {
		    throw new MobyRuntimeError(
			"format: fewer arguments passed than expected");
		}
		return plt.Kernel.toWrittenString(buffer.shift());
	    } else if (s == '~a' || s == "~A") {
		if (buffer.length == 0) {
		    throw new MobyRuntimeError(
			"format: fewer arguments passed than expected");
		}
		return plt.Kernel.toDisplayedString(buffer.shift());
	    } else {
		throw new MobyRuntimeError("Unimplemented format " + s);
	    }
	}
	var result = plt.types.String.makeInstance(formatStr.replace(pattern, f));
	if (buffer.length > 0) {
	    throw new MobyRuntimeError("format: More arguments passed than expected");
	}
	return result;
    }


    // args: arrayof plt.types.Char
    plt.Kernel.string = function(args) {
	var vals = [];
	for(var i = 0; i < args.length; i++) {
	    vals.push(args[i].getValue());
	}
	return plt.types.String.makeInstance(vals.join(""));
    };

    


    plt.Kernel.procedure_question_ = function(f) {
	return isFunction(f);
    };
    

    plt.Kernel.procedure_dash_arity = function(f) {
	check(f, isFunction, "procedure-arity", "function", 1);
	return f.procedureArity;
    };


    // procedureArityIncludes: function fixnum -> boolean
    // Returns true if the procedure arity of f includes n; false otherwise.
    var procedureArityIncludes = function(f, n) {
	if (isPair(f.procedureArity)) {
	    return n >= f.procedureArity.rest().first().toInteger();
	} else {
	    return n == f.procedureArity.toInteger();
	}
    };
    
    // procedureArityDescription: function -> string
    var procedureArityDescription = function(f) {
	if (isPair(f.procedureArity)) {
	    return ("at least " + 
		    (f.procedureArity.rest().first().toInteger() == 1) ? 
		    "one argument" : 
		    f.procedureArity.rest().first().toInteger() + " arguments");
	} else {
	    return ((f.procedureArity.toInteger() == 1) ? 
		    "one argument" : f.procedureArity.toInteger() + " arguments");
	}
    };


    plt.Kernel.xml_dash__greaterthan_s_dash_exp  = function(s) {
	check(s, isString, "xml->s-exp", "string", 1);
	if (s.length == 0) { 
	    return plt.types.String.makeInstance(""); 
	}
	var xmlDoc;
	try {
	    //Internet Explorer
	    xmlDoc=new ActiveXObject("Microsoft.XMLDOM");
	    xmlDoc.async="false";
	    xmlDoc.loadXML(s);
	    // FIXME: check parse errors
	}
	catch(e) {
	    var parser=new DOMParser();
	    xmlDoc=parser.parseFromString(s, "text/xml");
	    // FIXME: check parse errors
	}

	var parseAttributes = function(attrs) {
	    var result = plt.types.Empty.EMPTY;
	    for (var i = 0; i < attrs.length; i++) {
		var keyValue= plt.types.Cons.makeInstance(
		    plt.types.Symbol.makeInstance(attrs.item(i).nodeName),
		    plt.types.Cons.makeInstance(
			attrs.item(i).nodeValue,
			plt.types.Empty.EMPTY));
		result = plt.types.Cons.makeInstance(keyValue, result);
	    }
	    return plt.types.Cons.makeInstance(
		plt.types.Symbol.makeInstance("@"),
		plt.Kernel.reverse(result));
	};

	var parse = function(node) {
	    if (node.nodeType == Node.ELEMENT_NODE) {
		var result = plt.types.Empty.EMPTY;
		var child = node.firstChild;
		while (child != null) {
		    var nextResult = parse(child);
		    if (isString(nextResult) && 
			!result.isEmpty() &&
			isString(result.first())) {
			result = plt.types.Cons.makeInstance(result.first() + nextResult,
							     result.rest());
		    } else {
			result = plt.types.Cons.makeInstance(nextResult, result);
		    }
		    child = child.nextSibling;
		}
		result = plt.Kernel.reverse(result);
		result = plt.types.Cons.makeInstance(
		    parseAttributes(node.attributes),
		    result);
		result = plt.types.Cons.makeInstance(
		    plt.types.Symbol.makeInstance(node.nodeName),
		    result);
		return result;
	    } else if (node.nodeType == Node.TEXT_NODE) {
		return node.textContent;
	    } else if (node.nodeType == Node.CDATA_SECTION_NODE) {
		return node.data;
	    } else {
		return plt.types.Empty.EMPTY;
	    }
	};
	var result = parse(xmlDoc.firstChild);
	return result;
    };

    plt.Kernel.split_dash_whitespace = function(s) {
	s = s.replace(/^\s+/, "");
	s = s.replace(/\s+$/, "");
	return plt.Kernel.list(s.split(/\s+/));
    };
    

    // Boxes
    
    var Box = function(x) { 
	plt.Kernel.Struct.call(this, "box", [x]);
    };

    Box.prototype = heir(plt.Kernel.Struct.prototype);
    
    
    plt.Kernel.box = function(any) {
	return new Box(any);
    };
    
    plt.Kernel.unbox = function(obj) {
	check(obj, plt.Kernel.box_question_, "unbox", "box", 1);
	return obj._fields[0];
    };
    
    plt.Kernel.box_question_ = function(obj) {
	return obj != null && obj != undefined && obj instanceof Box ;
    };

    plt.Kernel.set_dash_box_bang_ = function(obj, newVal) {
	check(obj, plt.Kernel.box_question_, "set-box!", "box", 1);
	obj._fields[0] = newVal;
	return undefined;
    };
    
    


    
    
    // Posns
    
    var posn = function(x,y) { 
	plt.Kernel.Struct.call(this, "make-posn", [x, y]);
    }

    posn.prototype = heir(plt.Kernel.Struct.prototype);

    var make_dash_posn = function(id0,id1) { 
	return new posn(id0,id1); 
    }

    var posn_dash_x = function(obj) { 
	check(obj, posn_question_, "posn-x", "posn", 1);
	return obj._fields[0]; 
    }

    var posn_dash_y = function(obj) { 
	check(obj, posn_question_, "posn-y", "posn", 1);
	return obj._fields[1]; 
    }

    var posn_question_ = function(obj) { 
        return obj != null && obj != undefined && obj instanceof posn ; 
    }
    
    plt.Kernel.make_dash_posn = make_dash_posn;
    plt.Kernel.posn_question_ = posn_question_;
    plt.Kernel.posn_dash_x = posn_dash_x;
    plt.Kernel.posn_dash_y = posn_dash_y;
    
    

    plt.Kernel.error = function(name, msg) {
	check(name, isSymbol, "error", "symbol", 1);
	check(msg, isString, "error", "string", 2);
	throw new MobyRuntimeError(plt.Kernel.format("~a: ~a", [name, msg]).toString());
    };

    plt.Kernel.syntax_dash_error = function(msg, stx) {
	check(msg, isString, "syntax-error", "string", 1);
	check(stx, isStx, "syntax-error", "stx", 2);
	throw new MobySyntaxError(msg, stx);
    };




    plt.Kernel.toWrittenString = function(x, cache) {
	if (! cache) { cache = new plt._Hashtable(); }

	if (x && cache.containsKey(x)) {
	    return "...";
	}

	if (x == undefined || x == null) {
	    return "<undefined>";
	}
	if (typeof(x) == 'string') {
	    return x.toWrittenString();
	}
	if (typeof(x) != 'object' && typeof(x) != 'function') {
	    return x.toString();
	}
	if ('toWrittenString' in x) {
	    return x.toWrittenString(cache);
	}
	if ('toDisplayedString' in x) {
	    return x.toDisplayedString(cache);
	} else {
	    return x.toString();
	}
    };


    plt.Kernel.toDisplayedString = function(x, cache) {
	if (! cache) { cache = new plt._Hashtable(); }
	if (x && cache.containsKey(x)) {
	    return "...";
	}

	if (x == undefined || x == null) {
	    return "<undefined>";
	}
	if (typeof(x) == 'string') {
	    return x.toDisplayedString();
	}
	if (typeof(x) != 'object' && typeof(x) != 'function') {
	    return x.toString();
	}
	if ('toWrittenString' in x) {
	    return x.toWrittenString(cache);
	}
	if ('toDisplayedString' in x) {
	    return x.toDisplayedString(cache);
	} else {
	    return x.toString();
	}
    };



    // toDomNode: scheme-value -> dom-node
    plt.Kernel.toDomNode = function(x, cache) {
	if (! cache) { cache = new plt._Hashtable();}
	if (x && cache.containsKey(x)) {
	    return document.createTextNode("...");
	}

	if (x == undefined || x == null) {
	    var node = document.createTextNode("<undefined>");
	    return node;
	}
	if (typeof(x) == 'string') {
	    var node = document.createTextNode(x.toWrittenString());
	    return node;
	}
	if (typeof(x) != 'object' && typeof(x) != 'function') {
	    var node = document.createTextNode(x.toString());
	    return node;
	}
	if (x.nodeType) {
	    return x;
	}
	if ('toDomNode' in x) {
	    return x.toDomNode(cache);
	}
	if ('toWrittenString' in x) {
	    var node = document.createTextNode(plt.Kernel.toWrittenString(x, cache));
	    return node;
	}
	if ('toDisplayedString' in x) {
	    var node = document.createTextNode(plt.Kernel.toDisplayedString(x, cache));
	    return node;
	} else {
	    var node = document.createTextNode(x.toString());
	    return node;
	}
    };




    plt.Kernel.Struct.prototype.toWrittenString = function(cache) { 
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

    plt.Kernel.Struct.prototype.toDisplayedString = plt.Kernel.Struct.prototype.toWrittenString;


    var appendChild = function(parent, child) {
	parent.appendChild(child);
    }


    plt.Kernel.Struct.prototype.toDomNode = function(cache) {
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


    plt.Kernel.Struct.prototype.isEqual = function(other) {
	if (typeof(other) != 'object') {
	    return false;
	}
	if (! other._constructorName) {
	    return false;
	}
	if (other._constructorName != this._constructorName) {
	    return false;
	}
	if (! '_fields' in other) {
	    return false;
	}
	if (this._fields.length != other._fields.length) {
	    return false;
	}
	for (var i = 0; i < this._fields.length; i++) {
	    if (! plt.Kernel.equal_question_(this._fields[i],
					     other._fields[i])) {
		return false;
	    }
	}
	return true;
    };



    // reportError: (or exception string) -> void
    // Reports an error to the user, either at the console
    // if the console exists, or as alerts otherwise.
    plt.Kernel.reportError = function(e) {
	var reporter;
	if (typeof(console) != 'undefined' && 
	    typeof(console.log) != 'undefined') {
	    reporter = (function(x) { console.log(x); alert(x); });
	} else {
	    reporter = (function(x) { alert(x); });
	}
	if (typeof e == 'string') {
	    reporter(e);
	} else if (e.msg) {
	    reporter(e.msg);
	} else {
	    reporter(e.toString());
	}
	if (plt.Kernel.lastLoc) {
	    reporter("Error was raised around " + plt.Kernel.lastLoc);
	}
    };



    plt.Kernel._void_ = function(args) {
	return undefined;
    };





    plt.Kernel.build_dash_vector = function(n, f) {
	check(n, isNatural, "build-vector", "natural", 1);
	check(f, isFunction, "build-vector", "function", 2);
	var elts = [];
	for(var i = 0; i < n.toInteger(); i++) {
	    elts[i] = f([plt.types.Rational.makeInstance(i, 1)])
	}
	return plt.types.Vector.makeInstance(n.toInteger(),
					     elts);
    };

    plt.Kernel.make_dash_vector = function(n, args) {
	check(n, isNatural, "make-vector", "natural", 1);
	// FIXME: not quite right.  We need mixed arity function definition.
	check(args, function(x) { return x.length == 0 || x.length == 1}, "make-vector", "at most two", 2);
	var len = n.toInteger();
	var i;
	var result = plt.types.Vector.makeInstance(len);
	if (args.length == 1) {
	    for (i = 0; i < len; i++) {
		result.set(i, args[0]);
	    }
	}
	return result;
    };

    plt.Kernel.vector = function(args) {
	return plt.types.Vector.makeInstance(args.length, args);
    };

    plt.Kernel.vector_dash_length = function(vec) {
	check(vec, isVector, "vector-length", "vector", 1);
	return plt.types.Rational.makeInstance(vec.length());
    };

    plt.Kernel.vector_dash_ref = function(vec, k) {
	check(vec, isVector, "vector-ref", "vector", 1);
	check(k, function(x) { return isNatural(x) && x.toInteger() < vec.length()}, "vector-ref", "natural < vector length", 2);
	return vec.ref(k.toInteger());
    };

    plt.Kernel.vector_dash_set_bang_ = function(vec, k, v) {
	check(vec, isVector, "vector-set!", "vector", 1);
	check(k, function(x) { return isNatural(x) && x.toInteger() < vec.length()}, "vector-set!", "natural < vector length", 2);
	return vec.set(k.toInteger(), v);
    };

    plt.Kernel.vector_question_ = function(x) {
	return isVector(x) ? plt.types.Logic.TRUE : plt.types.Logic.FALSE;
    }



    plt.Kernel.check_dash_expect = function(testThunk, expectedThunk) {
	var val = testThunk([]);
	var expectedVal = expectedThunk([]);
	if (! plt.Kernel.equal_question_(val, expectedVal)) {
	    throw new MobyTestingError(
		plt.Kernel.format("~s doesn't match the expected value ~s",
				  [val, expectedVal]));
	}
    };

    plt.Kernel.check_dash_within = function(testThunk, expectedThunk, boundsThunk) {
	var val = testThunk([]);
	var expectedVal = expectedThunk([]);
	var boundsVal = boundsThunk([]);
	if (! plt.Kernel._equal__tilde_(val, expectedVal, boundsVal)) {
	    throw new MobyTestingError(
		plt.Kernel.format("~s doesn't match the expected value ~s within ~s",
				  [val, expectedVal, boundsVal]));
	}
    };

    plt.Kernel.check_dash_error = function(testThunk, msgThunk) {
	var msg = msgThunk([]);
	var val;
	try {
	    val = testThunk([]);
	} catch (e) {
	    if (! plt.Kernel.equal_question_(e.msg, msg)) {
		throw new MobyTestingError(
		    plt.Kernel.format(
			"check-error encountered the error ~s instead of the expected error ~s.",
			[e.msg, msg]));
	    } else {
		return;
	    }
	}
	throw new MobyTestingError(
	    plt.Kernel.format(
		"check-error expected the error ~s, but instead received the value ~s.",
		[msg, val]))
    };









    // As a program runs, the lastLoc will be assigned to the last location
    // we've evaluated in the program.
    plt.Kernel.lastLoc = undefined;
    plt.Kernel.setLastLoc = function(loc) {
	plt.Kernel.lastLoc = loc;
	return true;
    }
    


    plt.Kernel.printf = function(formatStr, args) {
	var msg = plt.Kernel.format(formatStr, args);
	plt.Kernel.printHook(msg);
	return undefined;
    }

    plt.Kernel.printHook = function(str) {
    };


    // Expose the predicates.
    plt.Kernel.isSymbol = isSymbol;
    plt.Kernel.isChar = isChar;
    plt.Kernel.isString = isString;
    plt.Kernel.isBoolean = isBoolean;
    plt.Kernel.isPair = isPair;
    plt.Kernel.isEmpty = isEmpty;
    plt.Kernel.isReal = isReal;
    plt.Kernel.isRational = isRational;
    plt.Kernel.isComplex = isComplex;
    plt.Kernel.isInteger = isInteger;
    plt.Kernel.isNatural = isNatural;
    plt.Kernel.isNumber = isNumber;
    plt.Kernel.isAlphabeticString = isAlphabeticString;
    plt.Kernel.isWhitespaceString = isWhitespaceString;
    plt.Kernel.isList = isList;
    plt.Kernel.isVector = isVector;
    plt.Kernel.isFunction = isFunction;
    

    plt.Kernel.arrayEach = arrayEach;

    // Expose the runtime type checkers.
    plt.Kernel.check = check;
    plt.Kernel.checkList = checkList;
    plt.Kernel.checkListof = checkListof;


    // Expose the error classes.
    plt.Kernel.MobyError = MobyError;
    plt.Kernel.MobyParserError = MobyParserError;
    plt.Kernel.MobySyntaxError = MobySyntaxError;
    plt.Kernel.MobyTypeError = MobyTypeError;
    plt.Kernel.MobyRuntimeError = MobyRuntimeError;



    plt.Kernel.attachEvent = attachEvent;
    plt.Kernel.detachEvent = detachEvent;


    plt.Kernel.ordinalize = ordinalize;

    
})();
