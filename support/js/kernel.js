var org = {};
org.plt = {};
 
 
 
//////////////////////////////////////////////////////////////////////
// Kernel
(function() {
 
    function chainTest(test, first, second, rest) {
  if (! test(first, second))
      return false;
  if (rest.length == 0)
      return true;
  if (! test(second, rest[0]))
      return false;
  for(var i = 0; i < rest.length - 1; i++) {
      if (! test(rest[i], rest[i+1]))
    return false;
  }
  return true;
    }
 
 
    function chainFind(comparator, first, rest) {
  var i;
  var best = first;
  for(i = 0; i < rest.length; i++) {
      if (! comparator(best, rest[i])) {
    best = rest[i];
      }
  }
  return best;
    }
 
 
    org.plt.Kernel = {
  Struct: function () {
  },
  
  struct_question_: function(thing) {
      return thing instanceof this.Struct;
  },
 
  equal_question_ : function(x, y) {
      if ("isEqual" in x) {
		return org.plt.types.NumberTower.equal(x, y);
      } else if ("isEqual" in y) {
		return org.plt.types.NumberTower.equal(y, x);
      } else {
    return x == y;
      }
  },
 
  
  identity : function (x){
      return x;
  },
 
 
  cons: function(x, y) {
      return org.plt.types.Cons.makeInstance(x, y);
  },
 
  empty_question_: function(thing) {
      return thing.isEmpty();
  },
 
  first: function(thing) {
      return thing.first();
  },
 
  rest: function(thing) {
      return thing.rest();
  },
 
 
  second: function(thing) {
      return thing.rest().first();
  },
 
  third: function(thing) {
      return thing.rest().rest().first();
  },
 
  fourth: function(thing) {
      return thing.rest().rest().rest().first();
  },
 
  fifth: function(thing) {
      return thing.rest().rest().rest().rest().first();
  },
 
 
  random: function(x) {
      return org.plt.types.Rational.makeInstance
      (Math.floor(org.plt.types.NumberTower.toInteger(x) * 
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
	return x.sqrt();
  },
 
  sqr: function(x) {
      return org.plt.types.NumberTower.sqr(x);
  },
 
  sin: function(x) {
	return x.sin();
      //return org.plt.types.NumberTower.sin(x);
  },
 
  cos: function(x) {
	return x.cos();
  },
 
  modulo: function(m, n) {
      return org.plt.types.NumberTower.modulo(m, n);
  },
 
  zero_question_: function(m) {
      return org.plt.types.NumberTower.equal(m, org.plt.types.Rational.ZERO);
  },
 
 
  _equal__tilde_ : function(x, y, delta) {
      // FIXME: check against other args too.
      return org.plt.types.NumberTower.approxEqual(x, y, delta);
  },
 
  abs: function(x) {
      return org.plt.types.NumberTower.abs(x);
  },
 
  add1 : function(x) {
      return org.plt.types.NumberTower.add(x, org.plt.types.Rational.ONE);
  },
 
  
  sub1 : function(x) {
      return org.plt.types.NumberTower.subtract(x, org.plt.types.Rational.ONE);
  },
 
 
  _plus_ : function(args) {
      var i, sum = org.plt.types.Rational.ZERO;
      for(i = 0; i < args.length; i++) {
    sum = org.plt.types.NumberTower.add(sum, args[i]);
      }
      return sum;
  },
 
  _dash_ : function(first, args) {
      if (args.length == 0) {
    return org.plt.types.NumberTower.subtract
    (org.plt.types.Rational.ZERO, first);
      }
 
      var i, diff = first;
      for(i = 0; i < args.length; i++) {
    diff = org.plt.types.NumberTower.subtract(diff, args[i]);
      }
      return diff;
  },
 
 
  _star_ : function(args) {
      var i, prod = org.plt.types.Rational.ONE;
      for(i = 0; i < args.length; i++) {
    prod = org.plt.types.NumberTower.multiply(prod, args[i]);
      }
      return prod;    
  },
 
 
  _slash_ : function(first, args) {
      var i, div = first;
      for(i = 0; i < args.length; i++) {
    div = org.plt.types.NumberTower.divide(div, args[i]);
      }
      return div;    
  },
 
 
  _equal_ : function(first, second, rest) {
      // FIXME: check against other args too.
      return chainTest(org.plt.types.NumberTower.equal,
           first,
           second,
           rest);
  },
 
 
  _greaterthan__equal_: function(first, second, rest) {
      return chainTest(org.plt.types.NumberTower.greaterThanOrEqual,
           first,
           second,
           rest);
  },
 
  _lessthan__equal_: function(first, second, rest) {
      return chainTest(org.plt.types.NumberTower.lessThanOrEqual,
           first,
           second,
           rest);
  },
 
  _greaterthan_: function(first, second, rest) {
      return chainTest(org.plt.types.NumberTower.greaterThan,
           first,
           second,
           rest);
  },
 
  _lessthan_: function(first, second, rest) {
      return chainTest(org.plt.types.NumberTower.lessThan,
           first,
           second,
           rest);
  },
 
  min : function(first, rest) {
      return chainFind(org.plt.types.NumberTower.lessThanOrEqual,
           first, 
           rest);
  },
 
  max : function(first, rest) {
      return chainFind(org.plt.types.NumberTower.greaterThanOrEqual,
           first, 
           rest);
  },
  
  string_equal__question_: function(x, y) {
      return x.isEqual(y);
  },
 
  symbol_equal__question_: function(x, y) {
      return x.isEqual(y);
  },
 
  not : function(x) {
      return !x;
  },
 
  number_dash__greaterthan_string: function(x) {
      return org.plt.types.String.makeInstance(x.toString());
  },
  
  conjugate: function(x){
	return x.conjugate();
  },
  
  magnitude: function(x){
	return x.magnitude();
  },
  
  log : function(x) {
	return x.log();
  },
  
  angle : function(x) {
	return x.angle();
  },
  
  atan : function(x) {
	return x.atan();
  },
  
  expt : function(x, y){
	return org.plt.types.NumberTower.expt(x, y);
  },
  
  exp : function(x){
	return x.exp();
  },
  
  acos : function(x){
	return x.acos();
  },
  
  asin : function(x){
	return x.asin();
  },
  
  tan : function(x){
	return org.plt.types.NumberTower.divide(x.sin(), x.cos());
  },
  
  complex_question_ : function(x){
	return x instanceof org.plt.types.Complex || x instanceof org.plt.types.Rational || x instanceof org.plt.types.FloatPoint;
  },
  
  cosh : function(x) {
	return this._plus_([this.exp(x), this.exp(x.minus())]).half();
  },
  
  sinh : function(x) {
	return org.plt.types.NumberTower.subtract(this.exp(x), this.exp(x.minus())).half();
  },
  
  denominator : function(x) {
	return org.plt.types.Rational.makeInstance(x.d, 1);
  },
  
  numerator : function(x){
	return org.plt.types.Rational.makeInstance(x.n, 1);
  },
  
  odd_question_ : function(x){
	return (x.toInteger() % 2 == 1);
  },
  
  even_question_ : function(x) {
	return (x.toInteger() % 2 == 0);
  },
  
  positive_question_ : function(x){
	return this._greaterthan_(x, Rational.ZERO, []);
  },
  
  negative_question_ : function(x){
	return this._lessthan_(x, Rational.ZERO, []);
  },
  
  imag_dash_part : function(x){
	return x.imag_dash_part();
  },
  
  real_dash_part : function(x){
	return x.real_dash_part();
  },
  
  integer_question_ : function(x){
	return this.equal_question_(x, x.floor());
  },
  
  make_dash_rectangular : function(x, y){
	return org.plt.types.Complex.makeInstance(x.toFloat(), y.toFloat());
  },
 
  max : function(first, rest){
	var i, ret = first;
	for (i = 0; i < rest.length; i++)
		if (org.plt.types.NumberTower.greaterThan(rest[i], ret))
			ret = rest[i];
	return ret;
  },
  
  min : function(first, rest){
	var i, ret = first;
	for (i = 0; i < rest.length; i++)
		if (org.plt.types.NumberTower.lessThan(rest[i], ret))
			ret = rest[i];
	return ret;
  },
  
  number_dash__greaterthan_string : function(n){
	return org.plt.types.String.makeInstance(n);
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
	return org.plt.types.Rational.makeInstance(org.plt.types.NumberTower.divide(x,y).floor(), 1);
  },
  
  remainder : function(x, y) {
	return org.plt.types.Rational.makeInstance(x.toInteger() % y.toInteger(), 1);
  },
  
  real_question_ : function(x){
	return x instanceof org.plt.types.Rational || x instanceof org.plt.types.FloatPoint || (x instanceof org.plt.types.Complex && x.isReal());
  },
  
  
  round : function(x){
	return x.round();
  },
  
  sgn : function(x){
	if (this.positive_question_(x))
		return Rational.ONE;
	if (this.negative_question_(x))
		return Rational.NEGATIVE_ONE;
	else
		return Rational.ZERO;
  },
  
  zero_question_ : function(x){
		return org.plt.types.NumberTower.equal(x, Rational.ZERO);
  },
  
  boolean_equal__question_ : function(x, y){
	return x == y;
  },
  
  boolean_question_ : function(x){
	return x == org.plt.types.Logic.TRUE || x == org.plt.types.Logic.FALSE;
  },
  
  false_question_ : function(x){
	return  x == org.plt.types.Logic.FALSE;
  },
  
  not : function(x){
	return x == org.plt.types.Logic.FALSE ? org.plt.types.Logic.TRUE : org.plt.types.Logic.FALSE;
  },
  
  symbol_dash__greaterthan_string : function(x){
	return org.plt.types.String.makeInstance(x);
  },
  
  symbol_equal__question_ : function(x, y){
	return x.val == y.val;
  },
  
  symbol_question_ : function(x){
	return x instanceof org.plt.types.Symbol;
  },
  
  empty_question_ : function(x){
	return x instanceof org.plt.types.Empty;
  },
  
  /*
  append : function(first, second, rest){
	
  },*/
  
  HEREEEEEEEEEEEEEEEEE : function(){}
 
  };
 
 
 
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // Types
 
    org.plt.types = {};
 
 
 
    // Posns
 
    function posn(x,y) { this.x = x;
       this.y = y; }
    posn.prototype = new org.plt.Kernel.Struct();
    posn.prototype.isEqual = function(other) {
        if (other instanceof posn) {
            return (((org.plt.Kernel.equal_question_((posn_dash_y(this)),(posn_dash_y(other)))))&&((((org.plt.Kernel.equal_question_((posn_dash_x(this)),(posn_dash_x(other)))))&&(org.plt.types.Logic.TRUE))));
        } else {
            return false;
        }
    } 
    function make_dash_posn(id0,id1) { return new posn(id0,id1); }
    function posn_dash_x(obj) { return obj.x; }
    function posn_dash_y(obj) { return obj.y; }
    function posn_question_(obj) { 
        return obj instanceof posn ; 
    }
 
    org.plt.Kernel.make_dash_posn = make_dash_posn;
    org.plt.Kernel.posn_question_ = posn_question_;
    org.plt.Kernel.posn_dash_x = posn_dash_x;
    org.plt.Kernel.posn_dash_y = posn_dash_y;
 
 
    org.plt.Kernel.error = function(msg) {
  die(msg);
    }
 
 
    org.plt.types.Logic = {
  TRUE : true,
  FALSE : false
    };
 
 
    function die(msg) {
  // We're trying to error out so that we get a stack track from firebug.
  console.log(msg);
  console.trace();
  throw new TypeError(msg.toString());
    }
 
 
 
    // Strings
    // For the moment, we just reuse Javascript strings.
    org.plt.types.String = String;
    org.plt.types.String.makeInstance = function(s) {
  return s;
    };
    org.plt.types.String.prototype.isEqual = function(other) {
  return this == other;
    }
 
 
 
 
    // Symbols
    org.plt.types.Symbol = function(val) {
  this.val = val;
    }
 
    // makeInstance: string -> Symbol.
    org.plt.types.Symbol.makeInstance = function(val) {
  return new org.plt.types.Symbol(val);
    };
 
    org.plt.types.Symbol.prototype.isEqual = function(other) {
  return other instanceof org.plt.types.Symbol &&
      this.val == other.val;
    };
 
    org.plt.types.Symbol.prototype.toString = function() {
  return this.val;
    };
 
 
 
    org.plt.types.Empty = function() {};
    org.plt.types.Empty.EMPTY = new org.plt.types.Empty();
    org.plt.types.Empty.prototype.first = function() {
  die("first can't be applied on empty.");
    };
    org.plt.types.Empty.prototype.rest = function() {
  die("rest can't be applied on empty.");
    };
    org.plt.types.Empty.prototype.isEmpty = function() {
  return true;
    };
 
 
 
    org.plt.types.Cons = function(f, r) {
  this.f = f;
  this.r = r;
    };
 
    org.plt.types.Cons.makeInstance = function(f, r) {
  return new org.plt.types.Cons(f, r);
    };
 
    org.plt.types.Cons.prototype.first = function() {
  return this.f;
    };
 
    org.plt.types.Cons.prototype.rest = function() {
  return this.r;
    };
 
    org.plt.types.Cons.prototype.isEmpty = function() {
  return false;
    };
    
 
 
 
    // Rationals
 
    function gcd(a, b) {
  var t;
  if (isNaN(a) || !isFinite(a)) {
      die("not a number: " + a);
  }
  if (isNaN(b) || !isFinite(b)) {
      die("not a number: " + b);
  }
  while (b != 0) {
      t = a;
      a = b;
      b = t % b;
  }
  return a;
    }
 
    org.plt.types.Rational = function(n, d) {
  var divisor = gcd(Math.abs(n), Math.abs(d));
  this.n = n / divisor;
  this.d = d / divisor;
    };
 
    org.plt.types.Rational.prototype.toString = function() {
  if (this.d == 1) {
      return this.n + "";
  } else {
      return this.n + "/" + this.d;
  }
    };
 
    org.plt.types.Rational.prototype.level = function() {
  return 0;
    };
 
 
    org.plt.types.Rational.prototype.lift = function(target) {
		if (target.level() == 1)
			return org.plt.types.FloatPoint.makeInstance(this.n / this.d);
		if (target.level() == 2)	
			return org.plt.types.Complex.makeInstance(this.n / this.d, 0);
		throw new Error("invalid level of Number");
    };
 
    org.plt.types.Rational.prototype.isEqual = function(other) {
  return other instanceof org.plt.types.Rational &&
      this.n == other.n &&
      this.d == other.d;
    };
 
 
    org.plt.types.Rational.prototype.add = function(other) {
  return org.plt.types.Rational.makeInstance(this.n * other.d + 
               this.d * other.n,
               this.d * other.d);
    };
 
    org.plt.types.Rational.prototype.subtract = function(other) {
  return org.plt.types.Rational.makeInstance((this.n * other.d) - 
               (this.d * other.n),
               (this.d * other.d));
    };
 
    org.plt.types.Rational.prototype.multiply = function(other) {
  return org.plt.types.Rational.makeInstance(this.n * other.n,
               this.d * other.d);
    };
 
    org.plt.types.Rational.prototype.divide = function(other) {
  return org.plt.types.Rational.makeInstance(this.n * other.d,
               this.d * other.n);
    };
 
 
    org.plt.types.Rational.prototype.toInteger = function() {
  return Math.floor(this.n / this.d);  
    };
 
    org.plt.types.Rational.prototype.toFloat = function() {
  return this.n / this.d;
    };
 
	org.plt.types.Rational.prototype.toComplex = function(){
		return org.plt.types.Complex.makeInstance(this.n / this.d, 0);
	};
 
    org.plt.types.Rational.prototype.greaterThanOrEqual = function(other) {
  return this.n*other.d >= this.d*other.n;
    };
 
    org.plt.types.Rational.prototype.lessThan = function(other) {
  return this.n*other.d < this.d*other.n;
    };
 
    org.plt.types.Rational.prototype.sqrt = function() {
	return this.toFloat().sqrt();
	/*
  var result = Math.sqrt(this.n / this.d);
  if (result == Math.floor(result)) {
      return org.plt.types.Rational.makeInstance(result, 1);
  } else {
      return org.plt.types.FloatPoint.makeInstance(result);
  }
  */
    };
 
    org.plt.types.Rational.prototype.abs = function() {
  return org.plt.types.Rational.makeInstance(Math.abs(this.n),
               this.d);
    };
 
    org.plt.types.Rational.prototype.floor = function() {
  return org.plt.types.Rational.makeInstance(Math.floor(this.n / this.d), 1);
    };
 
 
    org.plt.types.Rational.prototype.ceiling = function() {
  return org.plt.types.Rational.makeInstance(Math.ceil(this.n / this.d), 1);
    };
	
	org.plt.types.Rational.prototype.conjugate = org.plt.types.Rational.prototype.abs;
	
	org.plt.types.Rational.prototype.magnitude = org.plt.types.Rational.prototype.abs;
	
	org.plt.types.Rational.prototype.log = function(){
		return org.plt.types.FloatPoint.makeInstance(Math.log(this.n / this.d));
	};
	
	org.plt.types.Rational.prototype.angle = function(){
		if (0 == this.n)
			throw new Error("angle: undefined for 0");
		if (this.n > 0)
			return Rational.ZERO;
		else
			return org.plt.Kernel.pi;
	};
	
	org.plt.types.Rational.prototype.atan = function(){
		return org.plt.types.FloatPoint.makeInstance(Math.atan(this.n / this.d));
	};
	
	org.plt.types.Rational.prototype.cos = function(){
		return org.plt.types.FloatPoint.makeInstance(Math.cos(this.n / this.d));
	};
	
	org.plt.types.Rational.prototype.sin = function(){
		return org.plt.types.FloatPoint.makeInstance(Math.sin(this.n / this.d));
	};
	
	org.plt.types.Rational.prototype.expt = function(a){
		return org.plt.types.FloatPoint.makeInstance(Math.pow(this.n / this.d, a.n / a.d));
	};
	
	org.plt.types.Rational.prototype.exp = function(){
		return org.plt.types.FloatPoint.makeInstance(Math.exp(this.n / this.d));
	};
	
	org.plt.types.Rational.prototype.acos = function(){
		return org.plt.types.FloatPoint.makeInstance(Math.acos(this.n / this.d));
	};
	
	org.plt.types.Rational.prototype.asin = function(){
		return org.plt.types.FloatPoint.makeInstance(Math.asin(this.n / this.d));
	};
	
	org.plt.types.Rational.prototype.imag_dash_part = function(){
		return Rational.ZERO;
	};
	
	org.plt.types.Rational.prototype.real_dash_part = function(){
		return this;
	};
	
	org.plt.types.Rational.prototype.round = function(){
		return this;
	};
	
	org.plt.types.Rational.prototype.HEREEEEEEEEEEEEEEEEE = function(){};
	
	org.plt.types.Rational.prototype.half = function(){
		return org.plt.types.Rational.makeInstance(this.n, this.d * 2);
	};
	
	org.plt.types.Rational.prototype.minus = function(){
		return org.plt.types.Rational.makeInstance(0 - this.n, this.d);
	};
 
 
    var _rationalCache = {};
    org.plt.types.Rational.makeInstance = function(n, d) {
  if (n == undefined)
      die("n undefined");
  if (d == undefined)
      die("d undefined");
 
  if (d < 0) {
      n = -n;
      d = -d;
  }
 
  if (d == 1 && n in _rationalCache) {
      return _rationalCache[n];
  }
  else {
      return new org.plt.types.Rational(n, d);
  }
    };
 
    _rationalCache = {};
    (function() {
  var i;
  for(i = -500; i < 500; i++)
      _rationalCache[i] = new org.plt.types.Rational(i, 1);
    })();
    org.plt.types.Rational.NEGATIVE_ONE = _rationalCache[-1];
    org.plt.types.Rational.ZERO = _rationalCache[0];
    org.plt.types.Rational.ONE = _rationalCache[1];
 
 
 
 
 
 
    org.plt.types.FloatPoint = function(n) {
  this.n = n;
    };
 
    org.plt.types.FloatPoint.prototype.level = function() {
  return 1;
    };
 
    org.plt.types.FloatPoint.prototype.lift = function(target) {
		return org.plt.types.Complex.makeInstance(this.n, 0);
    };
 
    org.plt.types.FloatPoint.prototype.toString = function() {
  return this.n.toString();
    };
 
    org.plt.types.FloatPoint.prototype.isEqual = function(other) {
  return other instanceof org.plt.types.FloatPoint &&
      this.n == other.n;
    };
 
    org.plt.types.FloatPoint.prototype.add = function(other) {
  return org.plt.types.FloatPoint.makeInstance(this.n + other.n);
    };
 
    org.plt.types.FloatPoint.prototype.subtract = function(other) {
  return org.plt.types.FloatPoint.makeInstance(this.n - other.n);
    };
 
    org.plt.types.FloatPoint.prototype.multiply = function(other) {
  return org.plt.types.FloatPoint.makeInstance(this.n * other.n);
    };
 
    org.plt.types.FloatPoint.prototype.divide = function(other) {
  return org.plt.types.FloatPoint.makeInstance(this.n / other.n);
    };
 
 
    org.plt.types.FloatPoint.prototype.toInteger = function() {
  return Math.floor(this.n);  
    };
 
    org.plt.types.FloatPoint.prototype.toFloat = function() {
  return this.n;
    };
	
	org.plt.types.FloatPoint.prototype.toComplex = function(){
		return org.plt.types.Complex.makeInstance(this.n, 0);
	};
 
    org.plt.types.FloatPoint.prototype.floor = function() {
  return org.plt.types.Rational.makeInstance(Math.floor(this.n), 1);
    };
 
    org.plt.types.FloatPoint.prototype.ceiling = function() {
  return org.plt.types.Rational.makeInstance(Math.ceil(this.n), 1);
    };
 
 
    org.plt.types.FloatPoint.prototype.greaterThanOrEqual = function(other) {
  return this.n >= other.n;
    };
 
    org.plt.types.FloatPoint.prototype.lessThan = function(other) {
  return this.n < other.n;
    };
 
 
    org.plt.types.FloatPoint.prototype.sqrt = function() {
	if (this.n < 0)
		return org.plt.types.Complex.makeInstance(0, Math.sqrt(0 - this.n));
	else
		return org.plt.types.FloatPoint.makeInstance(Math.sqrt(this.n));
    };
 
    org.plt.types.FloatPoint.prototype.abs = function() {
  return org.plt.types.FloatPoint.makeInstance(Math.abs(this.n));
    };
 
    org.plt.types.FloatPoint.makeInstance = function(n) {
  return new org.plt.types.FloatPoint(n);
    };
	
	org.plt.types.FloatPoint.prototype.log = function(){
		if (this.n < 0)
			return this.toComplex().log();
		else
			return org.plt.types.FloatPoint.makeInstance(Math.log(this.n));
	};
	
	org.plt.types.FloatPoint.prototype.angle = function(){
		if (0 == this.n)
			throw new Error("angle: undefined for 0");
		if (this.n > 0)
			return Rational.ZERO;
		else
			return org.plt.Kernel.pi;
	};
	
	org.plt.types.FloatPoint.prototype.atan = function(){
		return org.plt.types.FloatPoint.makeInstance(Math.atan(this.n));
	};
	
	org.plt.types.FloatPoint.prototype.cos = function(){
		return org.plt.types.FloatPoint.makeInstance(Math.cos(this.n));
	};
	
	org.plt.types.FloatPoint.prototype.sin = function(){
		return org.plt.types.FloatPoint.makeInstance(Math.sin(this.n));
	};
	
	org.plt.types.FloatPoint.prototype.expt = function(a){
		return org.plt.types.FloatPoint.makeInstance(Math.pow(this.n, a.n));
	};
	
	org.plt.types.FloatPoint.prototype.exp = function(){
		return org.plt.types.FloatPoint.makeInstance(Math.exp(this.n));
	};
	
	org.plt.types.FloatPoint.prototype.acos = function(){
		return org.plt.types.FloatPoint.makeInstance(Math.acos(this.n));
	};
	
	org.plt.types.FloatPoint.prototype.asin = function(){
		return org.plt.types.FloatPoint.makeInstance(Math.asin(this.n));
	};
	
	org.plt.types.FloatPoint.prototype.imag_dash_part = function(){
		return Rational.ZERO;
	};
	
	org.plt.types.FloatPoint.prototype.real_dash_part = function(){
		return this;
	};
	
	
	org.plt.types.FloatPoint.prototype.round = function(){
		if (org.plt.types.NumberTower.lessThan(this.subtract(FloatPoint.makeInstance(0.5)).floor(), this.floor()))
			return this.floor();
		else 
			return this.ceiling();
			
	};
	
	org.plt.types.FloatPoint.prototype.HEREEEEEEEEEEEEEEEEE = function(){};
	
	org.plt.types.FloatPoint.prototype.conjugate = org.plt.types.FloatPoint.prototype.abs;
	
	org.plt.types.FloatPoint.prototype.magnitude = org.plt.types.FloatPoint.prototype.abs;
	
	org.plt.types.FloatPoint.prototype.minus = function(){
		return org.plt.types.FloatPoint.makeInstance(0 - this.n);
	};
	
	org.plt.types.FloatPoint.prototype.half = function(){
		return org.plt.types.FloatPoint.makeInstance(this.n / 2);
	};	
	
	org.plt.types.FloatPoint.prototype.timesI = function(){
		return org.plt.types.Complex.makeInstance(0, this.n);
	};
 
    org.plt.Kernel.pi = org.plt.types.FloatPoint.makeInstance(Math.PI);
    org.plt.Kernel.e = org.plt.types.FloatPoint.makeInstance(Math.E);
 
	org.plt.types.Complex = function(r, i){
		this.r = org.plt.types.FloatPoint.makeInstance(r);
		this.i = org.plt.types.FloatPoint.makeInstance(i);
	};
 
	org.plt.types.Complex.makeInstance = function(r, i){
		return new org.plt.types.Complex(r, i);
	};
	
	org.plt.types.Complex.prototype.level = function(){
		return 2;
	};
 
	org.plt.types.Complex.prototype.lift = function(target){
		throw new Error("Don't know how to lift Complex number");
	};
 
	org.plt.types.Complex.prototype.isEqual = function(other){
		return other instanceof org.plt.types.Complex  && this.r.isEqual(other.r) && this.i.isEqual(other.i);
    };
	
	org.plt.types.Complex.prototype.abs = function(){
		if (!org.plt.types.NumberTower.equal(this.i, org.plt.types.Rational.ZERO))
			throw new Error("abs: expects argument of type real number");
		return this.r.abs();
	};
	
	org.plt.types.Complex.prototype.toInteger = function(){
		if (!org.plt.types.NumberTower.equal(this.i, org.plt.types.Rational.ZERO))
			throw new Error("toInteger: expects argument of type real number");
		return this.r.toInteger();
	};
	
	org.plt.types.Complex.prototype.toFloat = function(){
		if (!org.plt.types.NumberTower.equal(this.i, org.plt.types.Rational.ZERO))
			throw new Error("toFloat: expects argument of type real number");
		return this.r.toFloat();
	};
	
	org.plt.types.Complex.prototype.toComplex = function(){
		return org.plt.types.Complex.makeInstance(this.r.n, this.i.n);
	};
	
	org.plt.types.Complex.prototype.add = function(other){
		return org.plt.types.Complex.makeInstance(this.r.n + other.r.n, this.i.n + other.i.n);
	};
	
	org.plt.types.Complex.prototype.subtract = function(other){
		return org.plt.types.Complex.makeInstance(this.r.n - other.r.n, this.i.n - other.i.n);
	};
	
	org.plt.types.Complex.prototype.multiply = function(other){
		var r = this.r.n * other.r.n - this.i.n * other.i.n;
		var i = this.r.n * other.i.n + this.i.n * other.r.n;
		return org.plt.types.Complex.makeInstance(r, i);
	};
	
	org.plt.types.Complex.prototype.divide = function(other){
		var con = other.conjugate();
		var up =  org.plt.types.NumberTower.multiply(this, con);
		var down = org.plt.types.NumberTower.multiply(other, con);
		return org.plt.types.Complex.makeInstance(up.r.n / down.r.n, up.i.n / down.r.n);
	};
	
	org.plt.types.Complex.prototype.conjugate = function(){
		return org.plt.types.Complex.makeInstance(this.r.n, 0 - this.i.n);
	};
	
	org.plt.types.Complex.prototype.magnitude = function(){
		var sum = org.plt.types.FloatPoint.makeInstance(this.r.n*this.r.n + this.i.n * this.i.n);
		return org.plt.types.FloatPoint.makeInstance(sum.sqrt().n);
	};
	
	org.plt.types.Complex.prototype.isReal = function(){
		return this.i.n == 0;
	};
	
	org.plt.types.Complex.prototype.sqrt = function(){
		if (this.isReal())
			return this.r.sqrt();
			
		// http://en.wikipedia.org/wiki/Square_root#Square_roots_of_negative_and_complex_numbers	
		var r_plus_x = org.plt.types.NumberTower.add(this.magnitude(), this.r);

		var r = r_plus_x.half().sqrt();
		var i = org.plt.types.NumberTower.divide(this.i, org.plt.types.NumberTower.multiply(r_plus_x, org.plt.types.FloatPoint.makeInstance(2)).sqrt());
		
		return org.plt.types.Complex.makeInstance(r.toFloat(), i.toFloat());
	};
	
	org.plt.types.Complex.prototype.log = function(){
		return org.plt.types.Complex.makeInstance(this.magnitude().log().toFloat(), this.angle().toFloat());
	};
	
	org.plt.types.Complex.prototype.angle = function(){
		if (this.isReal())
			return this.r.angle();
		if (0 == this.r.n){
			var tmp = org.plt.Kernel.pi.half();
			return this.i.n > 0 ? tmp : tmp.minus();
		} else {
			var tmp = org.plt.types.NumberTower.divide(this.i.abs(), this.r.abs()).atan();
			if (this.r.n > 0)
				return this.i.n > 0 ? tmp : tmp.minus();
			else
				return this.i.n > 0 ? org.plt.Kernel.pi.subtract(tmp) : tmp.subtract(org.plt.Kernel.pi);
		}
	};
	
	org.plt.types.Complex.prototype.atan = function(){
		if (this.isReal())
			return this.r.atan();
		var iz = this.timesI();
		var part1 = org.plt.types.Complex.makeInstance(1, iz.minus()).log();
		var part2 = org.plt.types.Complex.makeInstance(1, iz).log();
		return part1.subtract(part2).timesI().half();
	};
	
	org.plt.types.Complex.prototype.cos = function(){
		if (this.isReal())
			return this.r.cos();
		var iz = this.timesI();
		var iz_minus = iz.minus();
		
		return org.plt.types.NumberTower.add(iz.exp(), iz_minus.exp()).half();
	};
	
	org.plt.types.Complex.prototype.sin = function(){
		if (this.isReal())
			return this.r.sin();
		var iz = this.timesI();
		var iz_minus = iz.minus();
		var z2 = org.plt.types.Complex.makeInstance(0, 2);
		
		var exp_minus = org.plt.types.NumberTower.subtract(iz.exp(), iz_minus.exp());
		
		return org.plt.types.NumberTower.divide(exp_minus, z2);
	};
	
	org.plt.types.Complex.prototype.expt= function(y){
		var expo = y.multiply(this.log());
		return expo.exp();
	};
	
	org.plt.types.Complex.prototype.exp = function(){
		var part1 = this.r.exp();
		var part2 = org.plt.types.Complex.makeInstance(this.i.cos(), this.i.sin().timesI());
		
		return org.plt.types.NumberTower.multiply(part1, part2);
	};
	
	org.plt.types.Complex.prototype.acos = function(){
		if (this.isReal())
			return this.r.acos();
		var pi_half = org.plt.Kernel.pi.half();
		var iz = this.timesI();
		var root = org.plt.types.NumberTower.subtract(Rational.ONE, this.multiply(this)).sqrt();
		var l = org.plt.types.NumberTower.add(iz, root).log().timesI();
		return org.plt.types.NumberTower.add(pi_half, l);
	};
	
	org.plt.types.Complex.prototype.asin = function(){
		if (this.isReal())
			return this.r.asin();
		var iz = this.timesI();
		var root = org.plt.types.NumberTower.subtract(Rational.ONE, this.multiply(this)).sqrt();
		var ret = org.plt.types.NumberTower.add(iz, root).log().timesI().minus();
	};
	
	org.plt.types.Complex.prototype.ceiling = function(){
		if (!this.isReal())
			throw new Error("ceiling: can only be applied to real number");
		return this.r.ceiling();
	};
	
	org.plt.types.Complex.prototype.floor = function(){
		if (!this.isReal())
			throw new Error("floor: can only be applied to real number");
		return this.r.floor();
	};
	
	org.plt.types.Complex.prototype.imag_dash_part = function(){
		return this.i;
	};
	
	org.plt.types.Complex.prototype.real_dash_part = function(){
		return this.r;
	};
	
	org.plt.types.Complex.prototype.round = function(){
		return this.r.round();
	};
	
	org.plt.types.Complex.prototype.HEREEEEEEEEEEEEEEEEE = function(){};
	
	org.plt.types.Complex.prototype.timesI = function(){
		return this.multiply(org.plt.types.Complex.makeInstance(0, 1));
	};
	
	org.plt.types.Complex.prototype.minus = function(){
		return org.plt.types.Complex.makeInstance(0 - this.r.n, 0 - this.i.n);
	};
	
	org.plt.types.Complex.prototype.half = function(){
		return org.plt.types.Complex.makeInstance(this.r.n/2, this.i.n/2);
	};
 
    //////////////////////////////////////////////////////////////////////
    // NumberTower.
    // 
    // Currently only support Rational and Floating.
    org.plt.types.NumberTower = {};
 
 
    org.plt.types.NumberTower.toInteger = function(num) {
  return num.toInteger();
    };
 
    org.plt.types.NumberTower.toFloat = function(num) {
  return num.toFloat();
    };
 
    org.plt.types.NumberTower.abs = function(n) {
  return n.abs();
    }
 
    org.plt.types.NumberTower.add = function(x, y) {
  if (x.level() < y.level()) x = x.lift(y);
  if (y.level() < x.level()) y = y.lift(x);
  return x.add(y);
    };
 
    org.plt.types.NumberTower.subtract = function(x, y) {
  if (x.level() < y.level()) x = x.lift(y);
  if (y.level() < x.level()) y = y.lift(x);
  return x.subtract(y);
    };
 
    org.plt.types.NumberTower.multiply = function(x, y) {
  if (x.level() < y.level()) x = x.lift(y);
  if (y.level() < x.level()) y = y.lift(x);
  return x.multiply(y);
    };
 
    org.plt.types.NumberTower.divide = function(x, y) {
  if (x.level() < y.level()) x = x.lift(y);
  if (y.level() < x.level()) y = y.lift(x);
  return x.divide(y);
    };
 
    org.plt.types.NumberTower.equal = function(x, y) {
	if (x.level() < y.level()) x = x.lift(y);
  if (y.level() < x.level()) y = y.lift(x);
	
  return x.isEqual(y);
    };
 
    org.plt.types.NumberTower.approxEqual = function(x, y, delta) {
  // fixme: use delta
  return x.isEqual(y);
    };
	
	org.plt.types.NumberTower.greaterThanOrEqual = function(x, y){
		x = x.toComplex();
		y = y.toComplex();
		if (!(x.isReal() && y.isReal()))
			throw new Error("greaterThanOrEqual: couldn't be applied to complex number");
		return x.r >= y.r;
	};
	
	org.plt.types.NumberTower.lessThanOrEqual = function(x, y){
		x = x.toComplex();
		y = y.toComplex();
		if (!(x.isReal() && y.isReal()))
			throw new Error("lessThanOrEqual: couldn't be applied to complex number");
		return x.r <= y.r;
	};
	
	org.plt.types.NumberTower.greaterThan = function(x, y){
		x = x.toComplex();
		y = y.toComplex();
		
		if (!(x.isReal() && y.isReal()))
			throw new Error("greaterThan: couldn't be applied to complex number");
		return x.r > y.r;
		
	};
	
	org.plt.types.NumberTower.lessThan = function(x, y){
		x = x.toComplex();
		y = y.toComplex();
		if (!(x.isReal() && y.isReal()))
			throw new Error("lessThan: couldn't be applied to complex number");
		return x.r < y.r;
	};
 
    org.plt.types.NumberTower.modulo = function(m, n) {
  var result = 
      org.plt.types.Rational.makeInstance(m.toInteger() % n.toInteger(),
            1);
  if (org.plt.types.NumberTower.lessThan(result, org.plt.types.Rational.ZERO)) {
      return org.plt.types.NumberTower.add(result, n);
  }
  return result;
    };
 
    org.plt.types.NumberTower.sqr = function(x) {
  return org.plt.types.NumberTower.multiply(x, x);
    };
	
	org.plt.types.NumberTower.expt = function(x, y){
		if (x.level() < y.level()) x = x.lift(y);
		if (y.level() < x.level()) y = y.lift(x);
		return x.expt(y);
	};
 
 
 
    //////////////////////////////////////////////////////////////////////
    // Platform-specific stuff.
    org.plt.platform = {};
 
 
    org.plt.platform.getInstance = function() {
  return JavascriptPlatform;
    };
 
    var JavascriptPlatform = {};
 
    JavascriptPlatform.getLocationService = function() {
  return JavascriptLocationService;
    };
 
    JavascriptPlatform.getTiltService = function() {
  return JavascriptTiltService;
    };
 
    var JavascriptLocationService = { 
  startService : function() {
      // fill me in.
  },
  shutdownService : function() {
      // fill me in.
  },
 
  addLocationListener : function(listener) {
      // fill me in.
 
  }
    };
 
    var JavascriptTiltService = { 
  startService : function() {
      // fill me in.
  },
  shutdownService : function() {
      // fill me in.
  },
 
  addOrientationChangeListener : function(listener) {
      // fill me in.
 
  },
  addAccelerationChangeListener : function(listener) {
      // fill me in.
  }
    };
 
 
})();