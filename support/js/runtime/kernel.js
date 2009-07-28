var plt = plt || {};


//////////////////////////////////////////////////////////////////////
// Kernel
(function() {


    // Inheritance from pg 168: Javascript, the Definitive Guide.
    function heir(p) {
	function f() {}
	f.prototype = p;
	return new f();
    }



    // _gcd: integer integer -> integer
    function _gcd(a, b) {
	while (b != 0) {
	    t = a;
	    a = b;
	    b = t % b;
	}
	return a;
    }

    // _lcm: integer integer -> integer
    function _lcm(a, b) {
	return a * b / _gcd(a, b);
    }


    // Returns true if x is a number.
    function isNumber(x) {
	return (x instanceof plt.types.Rational || 
		x instanceof plt.types.FloatPoint ||
		x instanceof plt.types.Complex);
    }


    // Returns true if x is an integer.
    function isInteger(x) {
	return isNumber(x) && plt.types.NumberTower.equal(x, x.floor());
    }


    // arrayEach: (arrayof X) (X -> void) -> void
    // Apply some function on each element of the array.
    function arrayEach(arr, f) {
	for (var i = 0; i < arr.length; i++) {
	    f.apply(arr[i], [arr[i]]);
	}
    }


    // Apply a chaining test on pairs of elements of the list [first, second, rest ...].
    function chainTest(test, first, second, rest) {
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
    function chainFind(comparator, first, rest) {
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
    function isList(x) {
	return (x instanceof plt.types.Cons) || (x instanceof plt.types.Empty);
    }


    // Checks if x satisfies f.  If not, a TypeError of msg is thrown.
    function check(x, f, msg) {
	if (! f(x)) {
	    throw new TypeError(msg);
	}
    }

    // Throws exception if x is not a list.
    function checkList(x, msg) {
	if (! isList(x)) {
	    throw new TypeError(msg);
	}
    }

    // Checks if x is a list of f.  If not, throws a TypeError of msg.
    function checkListof(x, f, msg) {
	if (! isList(x)) {
	    throw new TypeError(msg);
	}
	while (! x.isEmpty()) {
	    if (! f(x.first())) {
		throw new TypeError(msg);
	    }
	    x = x.next();
	}
    }


    // checkNumericComparison: (number number (arrayof number) -> boolean) -> (number number (arrayof number) -> boolean) 
    function checkNumericComparison(comparisonF) {
	return function(first, second, rest) {
	    check(first, isNumber, "first must be a number");
	    check(second, isNumber, "second must be a number");
	    arrayEach(rest, 
		      function() { checkListof(this, isNumber, 
					       "all arguments must be numbers") });
	    return comparisonF(first, second, rest);
	}
    }



    function makeNumericComparator(test) {
	return checkNumericComparison(function(first, second, rest) {
	    return chainTest(test, first, second, rest);
	});
    }




    plt.Kernel = {
	
	_heir : heir,


	pi : plt.types.FloatPoint.makeInstance(Math.PI),
	e : plt.types.FloatPoint.makeInstance(Math.E),


	Struct: function () {
	},

	
	struct_question_: function(thing) {
	    return (thing instanceof this.Struct);
	},
	
	number_question_ : function(x){
	    return isNumber(x);
	},
	
	equal_question_ : function(x, y) {
	    if (plt.Kernel.number_question_(x).valueOf() && 
		plt.Kernel.number_question_(y).valueOf()) {
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
	
	eq_question_ : function(x, y){
	    return (x == y);
	}, 
	
	
	identity : function (x){
	    return x;
	},
	
	
	cons: function(x, y) {
	    checkList(y, "second argument to cons must be a list.");
	    return plt.types.Cons.makeInstance(x, y);
	},
	
	first: function(thing) {
	    checkList(thing, "first must consume a list");
	    return thing.first();
	},
	
	rest: function(thing) {
	    checkList(thing, "rest must consume a list");
	    return thing.rest();
	},
	
	
	second: function(thing) {
	    checkList(thing, "second must consume a list");
	    return thing.rest().first();
	},
	
	third: function(thing) {
	    checkList(thing, "third must consume a list");
	    return thing.rest().rest().first();
	},
	
	fourth: function(thing) {
	    checkList(thing, "fourth must consume a list");
	    return thing.rest().rest().rest().first();
	},
	
	fifth: function(thing) {
	    checkList(thing, "fifth must consume a list");
	    return thing.rest().rest().rest().rest().first();
	},
	
	
	random: function(x) {
	    return plt.types.Rational.makeInstance
	    (Math.floor(plt.types.NumberTower.toInteger(x) * 
			Math.random()),
	     1);
	},
	
	floor: function(x) {
	    return x.floor();
	},
	
	ceiling: function(x) {
	    return x.ceiling();
	},
	
	sqrt: function(x) {
	    check(x, isNumber, "number");
	    return x.sqrt();
	},

	integer_dash_sqrt: function(x) {
	    check(x, isInteger, "integer");
	    return plt.types.Rational.makeInstance(x.sqrt().toInteger());
	},
	
	sqr: function(x) {
	    check(x, isNumber, "number");
	    return plt.types.NumberTower.sqr(x);
	},
	
	sin: function(x) {
	    check(x, isNumber, "number");
	    return x.sin();
	},
	
	cos: function(x) {
	    check(x, isNumber, "number");
	    return x.cos();
	},
	
	modulo: function(m, n) {
	    check(m, isNumber, "number");
	    check(n, isNumber, "number");
	    return plt.types.NumberTower.modulo(m, n);
	},
	
	zero_question_: function(m) {
	    check(m, isNumber, "number");
	    return plt.types.NumberTower.equal(m, plt.types.Rational.ZERO);
	},
	
	
	_equal__tilde_ : function(x, y, delta) {
	    return plt.types.NumberTower.approxEqual(x, y, delta);
	},
	
	abs: function(x) {
	    check(x, isNumber, "number");
	    return plt.types.NumberTower.abs(x);
	},
	
	add1 : function(x) {
	    check(x, isNumber, "number");
	    return plt.types.NumberTower.add(x, plt.types.Rational.ONE);
	},
	
	
	sub1 : function(x) {
	    check(x, isNumber, "number");
	    return plt.types.NumberTower.subtract(x, plt.types.Rational.ONE);
	},
	
	
	_plus_ : function(args) {
	    var i, sum = plt.types.Rational.ZERO;
	    for(i = 0; i < args.length; i++) {
		sum = plt.types.NumberTower.add(sum, args[i]);
	    }
	    return sum;
	},
	
	_dash_ : function(first, args) {
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
	    var i, prod = plt.types.Rational.ONE;
	    for(i = 0; i < args.length; i++) {
		prod = plt.types.NumberTower.multiply(prod, args[i]);
	    }
	    return prod;    
	},
	
	
	_slash_ : function(first, args) {
	    var i, div = first;
	    for(i = 0; i < args.length; i++) {
		div = plt.types.NumberTower.divide(div, args[i]);
	    }
	    return div;    
	},
	
	_equal_ : makeNumericComparator(plt.types.NumberTower.equal),
	_greaterthan__equal_: makeNumericComparator(plt.types.NumberTower.greaterThanOrEqual),
	_lessthan__equal_: makeNumericComparator(plt.types.NumberTower.lessThanOrEqual),
	_greaterthan_: makeNumericComparator(plt.types.NumberTower.greaterThan),
	_lessthan_: makeNumericComparator(plt.types.NumberTower.lessThan),

	
	min : function(first, rest) {
	    check(first, isNumber, "number");
	    arrayEach(rest, function() { check(this, isNumber, "number"); });
	    return chainFind(plt.types.NumberTower.lessThanOrEqual,
			     first, 
			     rest);
	},
	
	max : function(first, rest) {
	    check(first, isNumber, "number");
	    arrayEach(rest, function() { check(this, isNumber, "number"); });
	    return chainFind(plt.types.NumberTower.greaterThanOrEqual,
			     first, 
			     rest);
	},
	

	lcm : function(first, rest) {
	    check(first, isInteger, "number");
	    arrayEach(rest, function() { check(this, isInteger, "number"); });
	    var result = first.toInteger();
	    for (var i = 0; i < rest.length; i++) {
		result = _lcm(result, rest[i].toInteger());
	    }
	    return plt.types.Rational.makeInstance(result);
	},

	
	gcd : function(first, rest) {
	    check(first, isInteger, "number");
	    arrayEach(rest, function() { check(this, isInteger, "number"); });	    
	    var result = first.toInteger();
	    for (var i = 0; i < rest.length; i++) {
		result = _gcd(result, rest[i].toInteger());
	    }
	    return plt.types.Rational.makeInstance(result);
	},

	
	inexact_dash__greaterthan_exact: function(x) {
	    return plt.types.NumberTower.toExact(x);
	},

	exact_question_ : function(x) {
	    check(x, isNumber, "number");
	    return x.isExact();
	},

	inexact_question_ : function(x) {
	    check(x, isNumber, "number");
	    return ! x.isExact();
	},
	
	rational_question_ : function(x) {
	    return (plt.Kernel.number_question_(x) &&
		    x.isRational());
	},

	number_dash__greaterthan_string: function(x) {
	    return plt.types.String.makeInstance(x.toWrittenString());
	},
	
	conjugate: function(x){
	    return x.conjugate();
	},
	
	magnitude: function(x){
	    return x.magnitude();
	},
	
	log : function(x) {
	    check(x, isNumber, "number");
	    return x.log();
	},
	
	angle : function(x) {
	    return x.angle();
	},
	
	atan : function(x) {
	    check(x, isNumber, "number");
	    return x.atan();
	},
	
	expt : function(x, y){
	    check(x, isNumber, "number");
	    check(y, isNumber, "number");
	    return plt.types.NumberTower.expt(x, y);
	},
	
	exp : function(x){
	    check(x, isNumber, "number");
	    return x.exp();
	},
	
	acos : function(x){
	    check(x, isNumber, "number");
	    return x.acos();
	},
	
	asin : function(x){
	    check(x, isNumber, "number");
	    return x.asin();
	},
	
	tan : function(x){
	    check(x, isNumber, "number");
	    return plt.types.NumberTower.divide(x.sin(), x.cos());
	},
	
	complex_question_ : function(x){
	    return (x instanceof plt.types.Complex || 
		    x instanceof plt.types.Rational ||
		    x instanceof plt.types.FloatPoint);
	},
	
	cosh : function(x) {
	    check(x, isNumber, "number");
	    return this._plus_([this.exp(x), this.exp(x.minus())]).half();
	},
	
	sinh : function(x) {
	    check(x, isNumber, "number");
	    return plt.types.NumberTower.subtract(this.exp(x), this.exp(x.minus())).half();
	},
	
	denominator : function(x) {
	    return plt.types.Rational.makeInstance(x.d, 1);
	},
	
	numerator : function(x){
	    return plt.types.Rational.makeInstance(x.n, 1);
	},
	
	odd_question_ : function(x){
	    check(x, isNumber, "number");
	    return (x.toInteger() % 2 == 1);
	},
	
	even_question_ : function(x) {
	    check(x, isNumber, "number");
	    return (x.toInteger() % 2 == 0);
	},
	
	positive_question_ : function(x){
	    check(x, isNumber, "number");
	    return this._greaterthan_(x, Rational.ZERO, []);
	},
	
	negative_question_ : function(x){
	    check(x, isNumber, "number");
	    return this._lessthan_(x, Rational.ZERO, []);
	},
	
	imag_dash_part : function(x){
	    return x.imag_dash_part();
	},
	
	real_dash_part : function(x){
	    return x.real_dash_part();
	},
	
	integer_question_ : function(x){
	    check(x, isNumber, "number");
	    return this.equal_question_(x, x.floor());
	},
	
	make_dash_rectangular : function(x, y){
	    return plt.types.Complex.makeInstance(x.toFloat(), y.toFloat());
	},
	
	string_equal__question_ : function(first, second, rest){
	    return chainTest(function(x, y){return x == y;}, first, second, rest);
	},
	
	string_lessthan__equal__question_: function(first, second, rest){
	    return chainTest(function(x, y){return x <= y;}, first, second, rest);
	},
	
	string_lessthan__question_: function(first, second, rest){
	    return chainTest(function(x, y){return x < y;}, first, second, rest);
	},
	
	string_greaterthan__equal__question_: function(first, second, rest){
	    return chainTest(function(x, y){return x >= y;}, first, second, rest);
	},
	
	string_greaterthan__question_: function(first, second, rest){
	    return chainTest(function(x, y){return x > y;}, first, second, rest);
	},
	
	quotient : function(x, y){
	    check(x, isNumber, "number");
	    check(y, isNumber, "number");
	    return plt.types.Rational.makeInstance(plt.types.NumberTower.divide(x,y).floor().toInteger(),
						   1);
	},
	
	remainder : function(x, y) {
	    check(x, isNumber, "number");
	    check(y, isNumber, "number");
	    return plt.types.Rational.makeInstance(x.toInteger() % y.toInteger(), 1);
	},
	

	real_question_ : function(x){
	    return (plt.Kernel.number_question_(x) &&
		    x.isReal());
	},
	
	
	round : function(x){
	    return x.round();
	},
	
	sgn : function(x){
	    check(x, isNumber, "number");
	    if (this.positive_question_(x).valueOf())
		return plt.types.Rational.ONE;
	    if (this.negative_question_(x).valueOf())
		return plt.types.Rational.NEGATIVE_ONE;
	    else
		return plt.types.Rational.ZERO;
	},
	
	boolean_equal__question_ : function(x, y){
	    return x == y;
	},
	
	boolean_question_ : function(x){
	    return (x == plt.types.Logic.TRUE || x == plt.types.Logic.FALSE);
	},
	
	false_question_ : function(x){
	    return (x == plt.types.Logic.FALSE);
	},
	
	not : function(x){
	    return (!(x.valueOf())) ? plt.types.Logic.TRUE : plt.types.Logic.FALSE;
	},
	
	symbol_dash__greaterthan_string : function(x){
	    return plt.types.String.makeInstance(x.val);
	},
	
	symbol_equal__question_ : function(x, y){
	    return (x.val == y.val);
	},
	
	symbol_question_ : function(x){
	    return (x instanceof plt.types.Symbol);
	},
	
	
	append : function(first, rest){
	    checkList(first, "append must consume lists");
            var ret = first;
	    var i;
	    for (i = 0; i < rest.length; i++) {
		checkList(rest[i], "append must consume lists");
		ret = ret.append(rest[i]);
	    }
	    return ret;
	},
	
	reverse : function(lst){
	    checkList(lst, "reverse must consume a list");
	    var ret = plt.types.Empty.EMPTY;
	    while (!lst.isEmpty()){
		ret = plt.types.Cons.makeInstance(lst.first(), ret);
		lst = lst.rest();
	    }
	    
	    return ret;
	}, 
	
	assq : function(x, lst){
	    checkList(lst, "assq must consume a list");
	    while (!lst.isEmpty() && !plt.Kernel.eq_question_(x, lst.first().first()))
		lst = lst.rest();
	    if (lst.isEmpty())
		return plt.types.Logic.FALSE;
	    else return lst.first();
	},
	
	caaar : function(lst){
	    checkList(lst, "caaar must consume a list");
	    return lst.first().first().first();
	},
	
	caadr : function(lst){
	    checkList(lst, "caadr must consume a list");
	    return lst.first().first().rest();
	},
	
	caar : function(lst){
	    checkList(lst, "caar must consume a list");
	    return lst.first().first();
	},
	
	cadar : function(lst){
	    checkList(lst, "cadar must consume a list");
	    return lst.first().rest().first();
	},
	
	cadddr : function(lst){
	    checkList(lst, "cadddr must consume a list");
	    return lst.rest().rest().rest().first();
	},
	
	caddr : function(lst){
	    checkList(lst, "caddr must consume a list");
	    return lst.rest().rest().first();
	},
	
	cadr : function(lst){
	    checkList(lst, "cadr must consume a list");
	    return lst.rest().first();
	},
	
	car : function(lst){
	    checkList(lst, "car must consume a list");
	    return lst.first();
	},
	
	cdaar : function(lst){
	    checkList(lst, "cdaar must consume a list");
	    return lst.first().first().rest();
	},
	
	cdadr : function(lst){
	    checkList(lst, "cdadr must consume a list");
	    return lst.rest().first().rest();
	},
	
	cdar : function(lst){
	    checkList(lst, "cdar must consume a list");
	    return lst.first().rest();
	},
	
	cddar : function(lst){
	    checkList(lst, "cddar must consume a list");
	    return lst.first().rest().rest();
	},
	
	cdddr : function(lst){
	    checkList(lst, "cdddr must consume a list");
	    return lst.rest().rest().rest();
	},
	
	cddr : function(lst){
	    checkList(lst, "cddr must consume a list");
	    return lst.rest().rest();
	},
	
	cdr : function(lst){
	    checkList(lst, "cdr must consume a list");
	    return lst.rest();
	},
	
	cons_question_: function(lst){
	    return lst instanceof plt.types.Cons;
	},
	
	sixth : function(lst){
	    checkList(lst, "sixth must consume a list");
	    return lst.rest().rest().rest().rest().rest().first();
	},
	
	seventh: function(lst){
	    checkList(lst, "seventh must consume a list");
	    return lst.rest().rest().rest().rest().rest().rest().first();
	},
	
	eighth : function(lst){
	    checkList(lst, "eighth must consume a list");
	    return lst.rest().rest().rest().rest().rest().rest().rest().first();
	},
	
	length : function(lst){
	    checkList(lst, "length must consume a list");
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
		throw new TypeError("list*: " + lastListItem + " not a list");
	    }
	    otherItems.unshift(items);
	    return plt.Kernel.append(plt.Kernel.list(otherItems), [lastListItem]);
	},
	
	list_dash_ref : function(lst, x){
	    checkList(lst, "list-ref must consume a list");
	    var i = plt.types.Rational.ZERO;
	    for (; plt.Kernel._lessthan_(i, x,[]); i = plt.Kernel.add1(i)) {
		lst = lst.rest();
	    }
	    return lst.first();
	},
	
	member : function(item, lst){
	    checkList(lst, "member must consume a list");
	    while (!lst.isEmpty()){
		if (plt.Kernel.equal_question_(item, lst.first()).valueOf())
		    return plt.types.Logic.TRUE;
		lst = lst.rest();
	    }
	    
	    return plt.types.Logic.FALSE;
	},
	
	memq : function(item, lst){
	    checkList(lst, "memq must consume a list");
	    while (!lst.isEmpty()){
		if (plt.Kernel.eq_question_(item, lst.first()).valueOf())
		    return lst;
		lst = lst.rest();
	    }
	    
	    return plt.types.Logic.FALSE;
	},
	
	eqv_question_ : function(x, y){
	    // FIXME: must check for number and do numerical equivalence.
	    return x == y;
	},
	
	memv : function(item, lst){
	    checkList(lst, "memv must consume a list");
	    while (!lst.isEmpty()){
		if (plt.Kernel.eqv_question_(item, lst.first()).valueOf())
		    return lst;
		lst = lst.rest();
	    }
	    
	    return plt.types.Logic.FALSE;
	},
	
	null_question_ : function(x){
	    return x instanceof plt.types.Empty;
	},
	
	empty_question_: function(x) {
	    return x instanceof plt.types.Empty;
	},
	
	pair_question_ : function(x){
	    return x instanceof plt.types.Cons;
	},
	
	string_dash__greaterthan_number : function(str){
	    var aNum = str * 1;
	    if (isNaN(aNum))
		return plt.types.Logic.FALSE;
	    return plt.types.FloatPoint.makeInstance(aNum);
	},
	
	string_dash__greaterthan_symbol : function(str){
	    return plt.types.Symbol.makeInstance(str);
	},
	
	string_dash_append : function(arr){
            return plt.types.String.makeInstance(arr.join(""));
	},
	
	string_dash_ci_equal__question_ : function(first, second, rest){
	    first = first.toUpperCase();
	    second = second.toUpperCase();
	    for (var i = 0; i < rest.length; i++) {
		rest[i] = rest[i].toUpperCase();
	    }
	    return plt.Kernel.string_equal__question_(first, second, rest);
	},
	
	string_dash_ci_lessthan__equal__question_ : function(first, second, rest){
	    first = first.toUpperCase();
	    second = second.toUpperCase();
	    for (var i = 0; i < rest.length; i++) {
		rest[i] = rest[i].toUpperCase();
	    }
	    return plt.Kernel.string_lessthan__equal__question_(first, second, rest);
	},
	
	string_dash_ci_lessthan__question_ : function(first, second, rest){
	    first = first.toUpperCase();
	    second = second.toUpperCase();
	    for (var i = 0; i < rest.length; i++) {
		rest[i] = rest[i].toUpperCase();
	    }
	    return plt.Kernel.string_lessthan__question_(first, second, rest);
	},
	
	string_dash_ci_greaterthan__question_ : function(first, second, rest){
	    return !plt.Kernel.string_dash_ci_lessthan__equal__question_(first, second, rest);
	},
	
	string_dash_ci_greaterthan__equal__question_ : function(first, second, rest){
	    return !plt.Kernel.string_dash_ci_lessthan__question_(first, second, rest);
	},
	
	string_dash_copy : function(str){
	    return plt.types.String.makeInstance(str);
	},
	
	string_dash_length : function(str){
	    return plt.types.Rational.makeInstance(str.length, 1);
	},
	
	string_dash_ref : function(str, i){
	    return str.charAt(i.toInteger());
	},

	string_dash_ith : function (str, i) {
	    return plt.types.String.makeInstance(str.substring(i.toInteger(), i.toInteger()+1));
	},

	int_dash_greaterthan_string: function (i) {
	    return plt.types.String.makeInstance(String.fromCharCode(i.toInteger()));
	},

	string_dash_greaterthan_int: function(str) {
	    return plt.types.Rational.makeInstance(str.toString().charCodeAt(0), 1);
	},

	
	string_question_ : function(str){
	    return typeof(str) == 'string';
	},
	
	substring : function(str, begin, end){
	    return str.substring(begin.toInteger(), end.toInteger());
	},

	char_question_: function(x) {
	    return x instanceof plt.types.Char;
	},
	
	char_dash__greaterthan_integer : function(ch){
	    var str = new String(ch.val);
	    return plt.types.Rational.makeInstance(str.charCodeAt(0), 1);
	},
	
	integer_dash__greaterthan_char : function(n){
	    var str = String.fromCharCode(n.toInteger());
	    return plt.types.Char.makeInstance(str);
	},
	
	char_dash_alphabetic_question_ : function(c){
	    var str = c.val;
	    return (str >= "a" && str <= "z") || (str >= "A" && str <= "Z");
	},
	
	char_equal__question_ : function(first, second, rest){
	    return chainTest(function(x, y){return x.isEqual(y);}, first, second, rest);
	},
	
	char_lessthan__question_ : function(first, second, rest){
	    return chainTest(function(x, y){return x.val < y.val}, first, second, rest);
	},
	
	char_lessthan__equal__question_ : function(first, second, rest){
	    return chainTest(function(x, y){return x.val <= y.val}, first, second, rest);
	},
	
	char_greaterthan__question_ : function(first, second, rest){
	    return !char_lessthan__equal__question_(first, second, rest);
	},
	
	char_greaterthan__equal__question_ : function(first, second, rest){
	    return !char_lessthan__question_(first, second, rest);
	},
	
	char_dash_ci_equal__question_ : function(first, second, rest){
	    first = plt.types.Char.makeInstance(first.val.toUpperCase());
	    second = plt.types.Char.makeInstance(second.val.toUpperCase());
	    for (var i = 0; i < rest.length; i++) {
		rest[i] = plt.types.Char.makeInstance(rest[i].val.toUpperCase());
	    }
	    return plt.Kernel.char_equal__question_(first, second, rest);
	},
	
	char_dash_ci_lessthan__question_ : function(first, second, rest){
	    first = plt.types.Char.makeInstance(first.val.toUpperCase());
	    second = plt.types.Char.makeInstance(second.val.toUpperCase());
	    for (var i = 0; i < rest.length; i++) {
		rest[i] = plt.types.Char.makeInstance(rest[i].val.toUpperCase());
	    }
	    return plt.Kernel.char_lessthan__question_(first, second, rest);
	},

	char_dash_ci_lessthan__equal__question_ : function(first, second, rest){
	    first = plt.types.Char.makeInstance(first.val.toUpperCase());
	    second = plt.types.Char.makeInstance(second.val.toUpperCase());
	    for (var i = 0; i < rest.length; i++) {
		rest[i] = plt.types.Char.makeInstance(rest[i].val.toUpperCase());
	    }
	    return plt.Kernel.char_lessthan__equal__question_(first, second, rest);
	},
	
	char_dash_ci_greaterthan__question_ : function(first, second, rest){
	    return !plt.Kernel.char_dash_ci_lessthan__equal__question_(first,second,rest);
	},
	
	char_dash_ci_greaterthan__equal__question_ : function(first, second, rest){
	    return !plt.Kernel.char_dash_ci_lessthan__question_(first,second,rest);
	},
	
	char_dash_downcase : function(ch){
	    var down = ch.val.toLowerCase();
	    return plt.types.Char.makeInstance(down);
	},
	
	char_dash_lower_dash_case_question_ : function(ch){
	    return plt.Kernel.char_dash_alphabetic_question_(ch) && plt.Kernel.equal_question_(ch, plt.Kernel.char_dash_downcase(ch));
	},
	
	char_dash_numeric_question_ : function(ch){
	    var str = ch.val;
	    return (str >= "0" && str <= "9");
	},
	
	char_dash_upcase : function(ch){
	    var up = ch.val.toUpperCase();
	    return plt.types.Char.makeInstance(up);
	},
	
	char_dash_upper_dash_case_question_ : function(ch){
	    return plt.Kernel.char_dash_alphabetic_question_(ch) && plt.Kernel.equal_question_(ch, plt.Kernel.char_dash_upcase(ch));
	},
	
	char_dash_whitespace_question_ : function(ch){
	    return plt.Kernel.equal_question_(ch, plt.types.Char.makeInstance(" "));
	},
	
	// list->string: (listof char) -> string
	list_dash__greaterthan_string : function(lst){
	    var ret = "";
	    while (!lst.isEmpty()){
		ret += lst.first().val;
		lst = lst.rest();
	    }
	    return plt.types.String.makeInstance(ret);
	},

	implode: function(lst) {
	    var ret = [];
	    while (!lst.isEmpty()){
		ret.push(lst.first().toString());
		lst = lst.rest();
	    }
	    return plt.types.String.makeInstance(ret.join(""));
	},
	

	make_dash_string : function(n, ch){
	    var ret = "";
	    var c = ch.val;
	    var i = plt.types.Rational.ZERO;
	    for (;  plt.Kernel._lessthan_(i, n, []); i = plt.Kernel.add1(i)) {
		ret += c;
	    }
	    return plt.types.String.makeInstance(ret);
	},
	
	string_dash__greaterthan_list : function(str){
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
    




    plt.Kernel.Struct.prototype.toWrittenString = function() { return "<struct>"};



    // DEBUGGING: get out all the functions defined in the kernel.
    plt.Kernel._dumpKernelSymbols = function() {
	var result = plt.types.Empty.EMPTY;
	for (var sym in plt.Kernel) {
	    result = plt.types.Cons.makeInstance(plt.types.Symbol.makeInstance(sym),
						 result);
	}
	return result;
    };


    function HashTable(inputHash) {
	this.hash = inputHash;
    }


    // kernelMakeImmutableHashEq: list -> hash
    plt.Kernel._kernelMakeImmutableHashEq = function(pairs) {
	var myhash = {};
	while (! pairs.isEmpty()) {
	    var nextPair = pairs.first();
	    var aKey = nextPair.first(); 
	    var aVal = nextPair.rest(); 
	    myhash[aKey] = aVal;
	    pairs = pairs.rest();
	}
	return new HashTable(myhash);
    };

    // plt.Kernel._kernelHashSet: hash object value -> hash
    plt.Kernel._kernelHashSet = function(obj, key, val) {
	var newHash = {};
	var hash = obj.hash;
	for (var k in hash) {
	    newHash[k] = hash[k];
	}
	newHash[key] = val;
	return new HashTable(newHash);
    };

    plt.Kernel._kernelHashRef = function(obj, key, defaultVal) {
	if (key in obj.hash) {
	    return obj.hash[key];
	} else {
	    if (typeof(defaultVal) == 'function')
		return defaultVal([]);
	    return defaultVal;
	}
    };
    
    plt.Kernel._kernelHashRemove = function(obj, key) {
	var newHash = {};
	var hash = obj.hash;
    	for (var k in hash) {
	    if (k != key)
    	    	newHash[k] = hash[k];
	}
	return new HashTable(newHash);
    };

    plt.Kernel._kernelHashMap = function(ht, f) {
	var result = plt.types.Empty.EMPTY;
	var key;
	for (key in ht.hash) {
	    var val = ht.hash[key];
	    result = plt.Kernel.cons(f([key, val]),
				     result);
	}
	return result;
    };


    plt.Kernel._resolveModulePath = function(path) {
	return path;
    };

    plt.Kernel._normalizePath = function(path) {
        return path;
    };

    plt.Kernel._pathToString = function(path) {
        return path.toString();
    };


    plt.Kernel.map = function(f, arglists) {
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

    plt.Kernel.foldl = function(f, acc, arglists) {
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

    plt.Kernel.build_dash_list = function(n, f) {
	var result = plt.types.Empty.EMPTY;
	for(var i = 0; i < n.toInteger(); i++) {
	    result = plt.Kernel.cons(f([plt.types.Rational.makeInstance(i, 1)]),
				     result);
	}
	return plt.Kernel.reverse(result);
    };

    plt.Kernel.format = function(formatStr, args) {
	// not right yet, but let's see how well this works.
	return plt.types.String.makeInstance(formatStr + args.join(" "));
    }


    // args: arrayof plt.types.Char
    plt.Kernel.string = function(args) {
	var vals = [];
	for(var i = 0; i < args.length; i++) {
	    vals.push(args[i].getValue());
	}
	return plt.types.String.makeInstance(vals.join(""));
    };

    
    


    
    
    // Posns
    
    function posn(x,y) { this.x = x;
			 this.y = y; }

    posn.prototype = heir(plt.Kernel.Struct.prototype);

    posn.prototype.isEqual = function(other) {
        if (other instanceof posn) {
            return (((plt.Kernel.equal_question_((posn_dash_y(this)),(posn_dash_y(other)))))&&((((plt.Kernel.equal_question_((posn_dash_x(this)),(posn_dash_x(other)))))&&(plt.types.Logic.TRUE))));
        } else {
            return plt.types.Logic.FALSE;
        }
    } 

    posn.prototype.toWrittenString = function() {
	return "(make-posn " + this.x.toWrittenString() + " " + this.y.toWrittenString() + ")";
    }

    posn.prototype.toDisplayedString = function () {
	return "(make-posn " + this.x.toDisplayedString() + " " + this.y.toDisplayedString();
    }

    function make_dash_posn(id0,id1) { return new posn(id0,id1); }
    function posn_dash_x(obj) { return obj.x; }
    function posn_dash_y(obj) { return obj.y; }
    function posn_question_(obj) { 
        return obj instanceof posn ; 
    }
    
    plt.Kernel.make_dash_posn = make_dash_posn;
    plt.Kernel.posn_question_ = posn_question_;
    plt.Kernel.posn_dash_x = posn_dash_x;
    plt.Kernel.posn_dash_y = posn_dash_y;
    
    
    
    plt.types.Logic = {
	TRUE : true,
	FALSE : false
    };
    
    Boolean.prototype.toWrittenString = function() {
	if (this.valueOf()) { return "true"; }
	return "false";
    };
    Boolean.prototype.toDisplayedString = Boolean.prototype.toWrittenString;



    plt.Kernel.error = function(msg, args) {
	die(msg + ": " + args);
    }

    
    function die(msg) {
	// We're trying to error out so that we get a stack track from firebug.
	//  console.log(msg);
	//  console.trace();
	throw new TypeError(msg.toString());
    }
    
    



    // Base class for all images.
    function BaseImage(pinholeX, pinholeY) {
	this.pinholeX = pinholeX;
	this.pinholeY = pinholeY;
    }
    plt.Kernel.BaseImage = BaseImage;


    BaseImage.prototype.updatePinhole = function(x, y) {
	var aCopy = {};
	for (attr in this) {
	    aCopy[attr] = this[attr];
	}
	aCopy.pinholeX = x;
	aCopy.pinholeY = y;
	return aCopy;
    }


    plt.Kernel.image_question_ = function(thing) {
	return (thing instanceof BaseImage);
    };



    plt.Kernel.image_equal__question_ = function(thing, other) {
	return thing == other ? plt.types.Logic.TRUE : plt.types.Logic.FALSE;
    };



    
})();
