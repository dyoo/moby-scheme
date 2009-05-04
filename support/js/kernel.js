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
      return org.plt.types.NumberTower.floor(x);
  },
 
  ceiling: function(x) {
      return org.plt.types.NumberTower.ceiling(x);
  },
 
  sqrt: function(x) {
      return org.plt.types.NumberTower.sqrt(x);
  },
 
  sqr: function(x) {
      return org.plt.types.NumberTower.sqr(x);
  },
 
  sin: function(x) {
      return org.plt.types.NumberTower.sin(x);
  },
 
  cos: function(x) {
      return org.plt.types.NumberTower.cos(x);
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
  }
 
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
 
 
    org.plt.types.Rational.prototype.toInteger = function(other) {
  return Math.floor(this.n / this.d);  
    };
 
    org.plt.types.Rational.prototype.toFloat = function(other) {
  return this.n / this.d;
    };
 
 
    org.plt.types.Rational.prototype.greaterThanOrEqual = function(other) {
  return this.n*other.d >= this.d*other.n;
    };
 
    org.plt.types.Rational.prototype.lessThan = function(other) {
  return this.n*other.d < this.d*other.n;
    };
 
    org.plt.types.Rational.prototype.sqrt = function() {
  var result = Math.sqrt(this.n / this.d);
  if (result == Math.floor(result)) {
      return org.plt.types.Rational.makeInstance(result, 1);
  } else {
      return org.plt.types.FloatPoint.makeInstance(result);
  }
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
		return org.plt.types.Complex.makeInstance(this, 0);
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
 
 
    org.plt.types.FloatPoint.prototype.toInteger = function(other) {
  return Math.floor(this.n);  
    };
 
    org.plt.types.FloatPoint.prototype.toFloat = function(other) {
  return this.n;
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
  return org.plt.types.FloatPoint.makeInstance(Math.sqrt(this.n));
    };
 
 
    org.plt.types.FloatPoint.prototype.abs = function() {
  return org.plt.types.FloatPoint.makeInstance(Math.abs(this.n));
    };
 
 
 
    org.plt.types.FloatPoint.makeInstance = function(n) {
  return new org.plt.types.FloatPoint(n);
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
	
	org.plt.types.Complex.prototype.add = function(other){
		return org.plt.types.Complex.makeInstance(org.plt.types.NumberTower.add(this.r, other.r),
		org.plt.types.NumberTower.add(this.i, other.i));
	};
	
	org.plt.types.Complex.prototype.subtract = function(other){
		return org.plt.types.Complex.makeInstance(org.plt.types.NumberTower.subtract(this.r, other.r),
		org.plt.types.NumberTower.subtract(this.i, other.i));
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
  while (x.level() < y.level()) x = x.lift(y);
  while (y.level() < x.level()) y = y.lift(x);
  return x.add(y);
    };
 
    org.plt.types.NumberTower.subtract = function(x, y) {
  while (x.level() < y.level()) x = x.lift(y);
  while (y.level() < x.level()) y = y.lift(x);
  return x.subtract(y);
    };
 
    org.plt.types.NumberTower.multiply = function(x, y) {
  while (x.level() < y.level()) x = x.lift(y);
  while (y.level() < x.level()) y = y.lift(x);
  return x.multiply(y);
    };
 
    org.plt.types.NumberTower.divide = function(x, y) {
  while (x.level() < y.level()) x = x.lift(y);
  while (y.level() < x.level()) y = y.lift(x);
  return x.divide(y);
    };
 
    org.plt.types.NumberTower.equal = function(x, y) {
	while (x.level() < y.level()) x = x.lift(y);
  while (y.level() < x.level()) y = y.lift(x);
	
  return x.isEqual(y);
    };
 
    org.plt.types.NumberTower.approxEqual = function(x, y, delta) {
  // fixme: use delta
  return x.isEqual(y);
    };
 
    org.plt.types.NumberTower.greaterThanOrEqual = function(x, y) {
  return x.toFloat() >= y.toFloat();
    };
 
    org.plt.types.NumberTower.lessThanOrEqual = function(x, y) {
  return x.toFloat() <= y.toFloat();
    };
 
    org.plt.types.NumberTower.greaterThan = function(x, y) {
  return x.toFloat() > y.toFloat();
    };
 
    org.plt.types.NumberTower.lessThan = function(x, y) {
  return x.toFloat() < y.toFloat();
    };
 
    org.plt.types.NumberTower.sqrt = function(x) {
  return x.sqrt();
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
 
    org.plt.types.NumberTower.floor = function(x) {
  return x.floor();
    };
 
    org.plt.types.NumberTower.ceiling = function(x) {
  return x.ceiling();
    };
 
    org.plt.types.NumberTower.sin = function(x) {
  return org.plt.types.FloatPoint.makeInstance(Math.sin(x.toFloat()));
    };
 
    org.plt.types.NumberTower.cos = function(x) {
  return org.plt.types.FloatPoint.makeInstance(Math.cos(x.toFloat()));
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