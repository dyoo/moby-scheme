var plt = plt || {};
 
 
 
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
 
 
    plt.Kernel = {
  Struct: function () {
  },
  
  struct_question_: function(thing) {
      return thing instanceof this.Struct;
  },
  
  number_question_ : function(x){
      return (x instanceof plt.types.Rational || 
	      x instanceof plt.types.FloatPoint ||
	      x instanceof plt.types.Complex);
  },
 
  equal_question_ : function(x, y) {
    if (plt.Kernel.number_question_(x) && 
	plt.Kernel.number_question_(y)) {
	  if ("isEqual" in x) {
	    return plt.types.NumberTower.equal(x, y);
	  } else if ("isEqual" in y) {
	    return plt.types.NumberTower.equal(y, x);
	  } else {
	    return x == y;
	  }
    } else {
      return x.isEqual(y);
    }
  },
  
  eq_question_ : function(x, y){
	return x == y;
  }, 
 
  
  identity : function (x){
      return x;
  },
 
 
  cons: function(x, y) {
      return plt.types.Cons.makeInstance(x, y);
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
	return x.sqrt();
  },
 
  sqr: function(x) {
      return plt.types.NumberTower.sqr(x);
  },
 
  sin: function(x) {
	return x.sin();
  },
 
  cos: function(x) {
	return x.cos();
  },
 
  modulo: function(m, n) {
      return plt.types.NumberTower.modulo(m, n);
  },
 
  zero_question_: function(m) {
      return plt.types.NumberTower.equal(m, plt.types.Rational.ZERO);
  },
 
 
  _equal__tilde_ : function(x, y, delta) {
      // FIXME: check against other args too.
      return plt.types.NumberTower.approxEqual(x, y, delta);
  },
 
  abs: function(x) {
      return plt.types.NumberTower.abs(x);
  },
 
  add1 : function(x) {
      return plt.types.NumberTower.add(x, plt.types.Rational.ONE);
  },
 
  
  sub1 : function(x) {
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
 
 
  _equal_ : function(first, second, rest) {
      // FIXME: check against other args too.
      return chainTest(plt.types.NumberTower.equal,
           first,
           second,
           rest);
  },
 
 
  _greaterthan__equal_: function(first, second, rest) {
      return chainTest(plt.types.NumberTower.greaterThanOrEqual,
           first,
           second,
           rest);
  },
 
  _lessthan__equal_: function(first, second, rest) {
      return chainTest(plt.types.NumberTower.lessThanOrEqual,
           first,
           second,
           rest);
  },
 
  _greaterthan_: function(first, second, rest) {
      return chainTest(plt.types.NumberTower.greaterThan,
           first,
           second,
           rest);
  },
 
  _lessthan_: function(first, second, rest) {
      return chainTest(plt.types.NumberTower.lessThan,
           first,
           second,
           rest);
  },
 
  min : function(first, rest) {
      return chainFind(plt.types.NumberTower.lessThanOrEqual,
           first, 
           rest);
  },
 
  max : function(first, rest) {
      return chainFind(plt.types.NumberTower.greaterThanOrEqual,
           first, 
           rest);
  },
 
  symbol_equal__question_: function(x, y) {
      return x.isEqual(y);
  },
 
  not : function(x) {
      return !x;
  },


  inexact_dash__greaterthan_exact: function(x) {
    return plt.types.NumberTower.toExact(x);
  },
 
  number_dash__greaterthan_string: function(x) {
      return plt.types.String.makeInstance(x.toString());
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
	return plt.types.NumberTower.expt(x, y);
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
	return plt.types.NumberTower.divide(x.sin(), x.cos());
  },
  
  complex_question_ : function(x){
	return x instanceof plt.types.Complex || x instanceof plt.types.Rational || x instanceof plt.types.FloatPoint;
  },
  
  cosh : function(x) {
	return this._plus_([this.exp(x), this.exp(x.minus())]).half();
  },
  
  sinh : function(x) {
	return plt.types.NumberTower.subtract(this.exp(x), this.exp(x.minus())).half();
  },
  
  denominator : function(x) {
	return plt.types.Rational.makeInstance(x.d, 1);
  },
  
  numerator : function(x){
	return plt.types.Rational.makeInstance(x.n, 1);
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
	return plt.types.Complex.makeInstance(x.toFloat(), y.toFloat());
  },
 
  max : function(first, rest){
	var i, ret = first;
	for (i = 0; i < rest.length; i++)
		if (plt.types.NumberTower.greaterThan(rest[i], ret))
			ret = rest[i];
	return ret;
  },
  
  min : function(first, rest){
	var i, ret = first;
	for (i = 0; i < rest.length; i++)
		if (plt.types.NumberTower.lessThan(rest[i], ret))
			ret = rest[i];
	return ret;
  },
  
  string_equal__question_ : function(first, second, rest){
	return chainTest(function(x, y){return x.toString() == y.toString();}, first, second, rest);
  },
  
  string_lessthan__equal__question_: function(first, second, rest){
	return chainTest(function(x, y){return x.toString() <= y.toString();}, first, second, rest);
  },
  
  string_lessthan__question_: function(first, second, rest){
	return chainTest(function(x, y){return x.toString() < y.toString();}, first, second, rest);
  },
  
  string_greaterthan__equal__question_: function(first, second, rest){
	return chainTest(function(x, y){return x.toString() >= y.toString();}, first, second, rest);
  },
  
  string_greaterthan__question_: function(first, second, rest){
	return chainTest(function(x, y){return x.toString() > y.toString();}, first, second, rest);
  },
  
  quotient : function(x, y){
	return plt.types.Rational.makeInstance(plt.types.NumberTower.divide(x,y).floor(), 1);
  },
  
  remainder : function(x, y) {
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
	if (this.positive_question_(x))
		return plt.types.Rational.ONE;
	if (this.negative_question_(x))
		return plt.types.Rational.NEGATIVE_ONE;
	else
		return plt.types.Rational.ZERO;
  },
  
  zero_question_ : function(x){
		return plt.types.NumberTower.equal(x, plt.types.Rational.ZERO);
  },
  
  boolean_equal__question_ : function(x, y){
	return x == y;
  },
  
  boolean_question_ : function(x){
	return x == plt.types.Logic.TRUE || x == plt.types.Logic.FALSE;
  },
  
  false_question_ : function(x){
	return  x == plt.types.Logic.FALSE;
  },
  
  not : function(x){
	return x == plt.types.Logic.FALSE ? plt.types.Logic.TRUE : plt.types.Logic.FALSE;
  },
  
  symbol_dash__greaterthan_string : function(x){
	return plt.types.String.makeInstance(x);
  },
  
  symbol_equal__question_ : function(x, y){
	return x.val == y.val;
  },
  
  symbol_question_ : function(x){
	return x instanceof plt.types.Symbol;
  },
  
  empty_question_ : function(x){
	return x instanceof plt.types.Empty;
  },
  
  append : function(first, rest){
        var ret = first;
	var i;
	for (i = 0; i < rest.length; i++)
		ret = ret.append(rest[i]);
	return ret;
  },
  
  reverse : function(lst){
	var ret = plt.types.Empty.EMPTY;
	while (!lst.isEmpty()){
		ret = plt.types.Cons.makeInstance(lst.first(), ret);
		lst = lst.rest();
	}
	
	return ret;
  }, 
    
  assq : function(x, lst){
	while (!lst.isEmpty() && !plt.Kernel.eq_question_(x, lst.first().first()))
		lst = lst.rest();
	if (lst.isEmpty())
		return plt.types.Logic.FALSE;
	else return lst.first();
  },
  
  caaar : function(lst){
	return lst.first().first().first();
  },
  
  caadr : function(lst){
	return lst.first().first().rest();
  },
  
  caar : function(lst){
	return lst.first().first();
  },
  
  cadar : function(lst){
	return lst.first().rest().first();
  },
  
  cadddr : function(lst){
	return lst.rest().rest().rest().first();
  },
  
  caddr : function(lst){
	return lst.rest().rest().first();
  },
  
  cadr : function(lst){
	return lst.rest().first();
  },
  
  car : function(lst){
	return lst.first();
  },
  
  cdaar : function(lst){
	return lst.first().first().rest();
  },
  
  cdadr : function(lst){
	return lst.rest().first().rest();
  },
  
  cdar : function(lst){
	return lst.first().rest();
  },
  
  cddar : function(lst){
	return lst.first().rest().rest();
  },
  
  cdddr : function(lst){
	return lst.rest().rest().rest();
  },
  
  cddr : function(lst){
	return lst.rest().rest();
  },
  
  cdr : function(lst){
	return lst.rest();
  },
  
  cons_question_: function(lst){
	return lst instanceof plt.types.Cons;
  },
  
  sixth : function(lst){
	return lst.rest().rest().rest().rest().rest().first();
  },
  
  seventh: function(lst){
	return lst.rest().rest().rest().rest().rest().rest().first();
  },
  
  eighth : function(lst){
	return lst.rest().rest().rest().rest().rest().rest().rest().first();
  },
  
  length : function(lst){
	var ret = plt.types.Rational.ZERO;
	for (; !lst.isEmpty(); lst = lst.rest())
		ret = plt.Kernel.add1(ret);
	return ret;
  },
  
  list : function(items){
	var ret = plt.types.Empty.EMPTY;
	for (var i = items.length - 1; i >=0; i--)
		ret = plt.types.Cons.makeInstance(items[i], ret);
	return ret;
  },
  
  list_star_ : function(items, otherItems){
      var lastListItem = otherItems.pop();
      otherItems.unshift(items);
      return plt.Kernel.append(plt.Kernel.list(otherItems), [lastListItem]);
  },
  
  list_dash_ref : function(lst, x){
	var i = plt.types.Rational.ZERO;
	for (; plt.Kernel._lessthan_(i, x,[]); i = plt.Kernel.add1(i))
		lst = lst.rest();
	return lst.first();
  },
  
  member : function(item, lst){
	while (!lst.isEmpty()){
		if (plt.Kernel.equal_question_(item, lst.first()))
			return true;
		lst = lst.rest();
	}
	
	return false;
  },
  
  memq : function(item, lst){
	while (!lst.isEmpty()){
		if (plt.Kernel.eq_question_(item, lst.first()))
			return lst;
		lst = lst.rest();
	}
	
	return false;
  },
  
  eqv_question_ : function(x, y){
	return x == y;
  },
  
  memv : function(item, lst){
	while (!lst.isEmpty()){
		if (plt.Kernel.eqv_question_(item, lst.first()))
			return lst;
		lst = lst.rest();
	}
	
	return false;
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
		return false;
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
	for (var i = 0; i < rest.length; i++)
		rest[i] = rest[i].toUpperCase();
	return plt.Kernel.string_equal__question_(first, second, rest);
  },
  
  string_dash_ci_lessthan__equal__question_ : function(first, second, rest){
	first = first.toUpperCase();
	second = second.toUpperCase();
	for (var i = 0; i < rest.length; i++)
		rest[i] = rest[i].toUpperCase();
	return plt.Kernel.string_lessthan__equal__question_(first, second, rest);
  },
  
  string_dash_ci_lessthan__question_ : function(first, second, rest){
	first = first.toUpperCase();
	second = second.toUpperCase();
	for (var i = 0; i < rest.length; i++)
		rest[i] = rest[i].toUpperCase();
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
  
  string_question_ : function(str){
      return typeof(str) == 'string';
  },
  
  substring : function(str, begin, end){
	return str.toString().substring(begin.toInteger(), end.toInteger());
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
	var str = c.val.toString();
	return (str >= "a" && str <= "z") || (str >= "A" && str <= "Z");
  },
  
  char_equal__question_ : function(first, second, rest){
	return chainTest(function(x, y){return x.isEqual(y);}, first, second, rest);
  },
  
  char_lessthan__question_ : function(first, second, rest){
	return chainTest(function(x, y){return x.val.toString() < y.val.toString()}, first, second, rest);
  },
  
  char_lessthan__equal__question_ : function(first, second, rest){
	return chainTest(function(x, y){return x.val.toString() <= y.val.toString()}, first, second, rest);
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
	for (var i = 0; i < rest.length; i++)
		rest[i] = plt.types.Char.makeInstance(rest[i].val.toUpperCase());
	return plt.Kernel.char_equal__question_(first, second, rest);
  },
  
  char_dash_ci_lessthan__question_ : function(first, second, rest){
	first = plt.types.Char.makeInstance(first.val.toUpperCase());
	second = plt.types.Char.makeInstance(second.val.toUpperCase());
	for (var i = 0; i < rest.length; i++)
		rest[i] = plt.types.Char.makeInstance(rest[i].val.toUpperCase());
	return plt.Kernel.char_lessthan__question_(first, second, rest);
  },

  char_dash_ci_lessthan__equal__question_ : function(first, second, rest){
	first = plt.types.Char.makeInstance(first.val.toUpperCase());
	second = plt.types.Char.makeInstance(second.val.toUpperCase());
	for (var i = 0; i < rest.length; i++)
		rest[i] = plt.types.Char.makeInstance(rest[i].val.toUpperCase());
	return plt.Kernel.char_lessthan__equal__question_(first, second, rest);
  },
  
  char_dash_ci_greaterthan__question_ : function(first, second, rest){
	return !plt.Kernel.char_dash_ci_lessthan__equal__question_(first,second,rest);
  },
  
  char_dash_ci_greaterthan__equal__question_ : function(first, second, rest){
	return !plt.Kernel.char_dash_ci_lessthan__question_(first,second,rest);
  },
  
  char_dash_downcase : function(ch){
	var down = ch.val.toString().toLowerCase();
	return plt.types.Char.makeInstance(down);
  },
  
  char_dash_lower_dash_case_question_ : function(ch){
	return plt.Kernel.char_dash_alphabetic_question_(ch) && plt.Kernel.equal_question_(ch, plt.Kernel.char_dash_downcase(ch));
  },
  
  char_dash_numeric_question_ : function(ch){
	var str = ch.val.toString();
	return (str >= "0" && str <= "9");
  },
  
  char_dash_upcase : function(ch){
	var up = ch.val.toString().toUpperCase();
	return plt.types.Char.makeInstance(up);
  },
  
  char_dash_upper_dash_case_question_ : function(ch){
	return plt.Kernel.char_dash_alphabetic_question_(ch) && plt.Kernel.equal_question_(ch, plt.Kernel.char_dash_upcase(ch));
  },
  
  char_dash_whitespace_question_ : function(ch){
	return plt.Kernel.equal_question_(ch, plt.types.Char.makeInstance(" "));
  },
  
  list_dash__greaterthan_string : function(lst){
	var ret = "";
	while (!lst.isEmpty()){
		ret += lst.first().val.toString();
		lst = lst.rest();
	}
	return plt.types.String.makeInstance(ret);
  },
  
  make_dash_string : function(n, ch){
	var ret = "";
	var c = ch.val.toString();
	var i = plt.types.Rational.ZERO;
	for (;  plt.Kernel._lessthan_(i, n, []); i = plt.Kernel.add1(i))
		ret += c;
	return plt.types.String.makeInstance(ret);
  },
  
  string_dash__greaterthan_list : function(str){
	var s = str.toString();
	var ret = plt.types.Empty.EMPTY;
	for (var i = s.length - 1; i >= 0; i--){
	    ret = plt.types.Cons.makeInstance
		(plt.types.Char.makeInstance(s.charAt(i)),
		 ret);
	}
	return ret;
  },
  
  HEREEEEEEEEEEEEEEEEE : function(){}


	
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
	    return defaultVal;
	}
    };

    plt.Kernel._kernelHashMap = function(ht, f) {
	var result = plt.types.Empty.EMPTY;
	var key;
	for (key in ht.hash) {
	    var val = ht.hash[key];
	    result = plt.Kernel.cons(f.apply(null, [[key, val]]),
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
	    results = plt.Kernel.cons(f.apply(null, [args]),
					  results);
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
	result = f.apply(null, [args]);
      }
      return result;
    };

    plt.Kernel.build_dash_list = function(n, f) {
	var result = plt.types.Empty.EMPTY;
	for(var i = 0; i < n.toInteger(); i++) {
	    result = plt.Kernel.cons(f.apply(null, [[plt.types.Rational.makeInstance(i, 1)]]),
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

 
 
    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // Types
 
    plt.types = plt.types || {};
 
 
 
    // Posns
 
    function posn(x,y) { this.x = x;
       this.y = y; }
    posn.prototype = new plt.Kernel.Struct();
    posn.prototype.isEqual = function(other) {
        if (other instanceof posn) {
            return (((plt.Kernel.equal_question_((posn_dash_y(this)),(posn_dash_y(other)))))&&((((plt.Kernel.equal_question_((posn_dash_x(this)),(posn_dash_x(other)))))&&(plt.types.Logic.TRUE))));
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
 
    plt.Kernel.make_dash_posn = make_dash_posn;
    plt.Kernel.posn_question_ = posn_question_;
    plt.Kernel.posn_dash_x = posn_dash_x;
    plt.Kernel.posn_dash_y = posn_dash_y;
 
 
    plt.Kernel.error = function(msg, args) {
	die(msg + ": " + args);
    }
 
 
    plt.types.Logic = {
  TRUE : true,
  FALSE : false
    };
 
 
    function die(msg) {
  // We're trying to error out so that we get a stack track from firebug.
//  console.log(msg);
//  console.trace();
  throw new TypeError(msg.toString());
    }
 
 
 
    // Strings
    // For the moment, we just reuse Javascript strings.
    plt.types.String = String;
    plt.types.String.makeInstance = function(s) {
	return s;
    };
	
	plt.types.String.prototype.isEqual = function(other){
		return this.toString() == other.toString();
	};
	
	// Chars
	plt.types.Char = function(val){
		this.val = val;
	};
	
	plt.types.Char.makeInstance = function(val){
		return new plt.types.Char(val);
	};

	plt.types.Char.prototype.toString = function() {
	    return "#\\" + this.val;
	};

	plt.types.Char.prototype.getValue = function() {
	    return this.val;
	};

	plt.types.Char.prototype.isEqual = function(other){
		return other instanceof plt.types.Char && this.val.toString() == other.val.toString();
	};
	
    // Symbols


    plt.types.Symbol = function(val) {
	this.val = val;
    };

    var symbolCache = {};
 
    // makeInstance: string -> Symbol.
    plt.types.Symbol.makeInstance = function(val) {
	// To ensure that we can eq? symbols with equal values.
	if (!(val in symbolCache)) {
	    symbolCache[val] = new plt.types.Symbol(val);
	}
	return symbolCache[val];
    };
 
    plt.types.Symbol.prototype.isEqual = function(other) {
  return other instanceof plt.types.Symbol &&
      this.val == other.val;
    };
 
    plt.types.Symbol.prototype.toString = function() {
  return this.val;
    };
 
 
 
    plt.types.Empty = function() {};
    plt.types.Empty.EMPTY = new plt.types.Empty();


    plt.types.Empty.prototype.isEqual = function(other) {
      return other instanceof plt.types.Empty;
    };

    plt.types.Empty.prototype.first = function() {
  die("first can't be applied on empty.");
    };
    plt.types.Empty.prototype.rest = function() {
  die("rest can't be applied on empty.");
    };
    plt.types.Empty.prototype.isEmpty = function() {
  return true;
    };
    plt.types.Empty.prototype.toString = function() { return "empty"; };

 
    plt.types.Empty.prototype.append = function(b){
      return b;
    }
 
    plt.types.Cons = function(f, r) {
  this.f = f;
  this.r = r;
    };
 
    plt.types.Cons.makeInstance = function(f, r) {
  return new plt.types.Cons(f, r);
    };


    plt.types.Cons.prototype.isEqual = function(other) {
      if (! (other instanceof plt.types.Cons)) {
	return false;
      }
      return (plt.Kernel.equal_question_(this.first(), other.first()) &&
	      plt.Kernel.equal_question_(this.rest(), other.rest()));
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
    
    plt.types.Cons.prototype.append = function(b){
	if (this.isEmpty())
	    return b;
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
    
    plt.types.Cons.prototype.toString = function() {
	var texts = [];
	var p = this;
	while (! p.isEmpty()) {
	    texts.push(p.first().toString());
	    p = p.rest();
	}
	return "(" + texts.join(", ") + ")";
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
 
    plt.types.Rational = function(n, d) {
  var divisor = gcd(Math.abs(n), Math.abs(d));
  this.n = n / divisor;
  this.d = d / divisor;
    };
 
    plt.types.Rational.prototype.toString = function() {
  if (this.d == 1) {
      return this.n + "";
  } else {
      return this.n + "/" + this.d;
  }
    };
 
    plt.types.Rational.prototype.level = function() {
  return 0;
    };
 
 
    plt.types.Rational.prototype.lift = function(target) {
		if (target.level() == 1)
			return plt.types.FloatPoint.makeInstance(this.n / this.d);
		if (target.level() == 2)	
			return plt.types.Complex.makeInstance(this.n / this.d, 0);
		throw new Error("invalid level of Number");
    };
 
    plt.types.Rational.prototype.isEqual = function(other) {
  return other instanceof plt.types.Rational &&
      this.n == other.n &&
      this.d == other.d;
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
  return plt.types.Rational.makeInstance(this.n * other.d,
               this.d * other.n);
    };
 

    plt.types.Rational.prototype.toExact = function() { 
      return this;
    };
 
    plt.types.Rational.prototype.toInteger = function() {
  return Math.floor(this.n / this.d);  
    };
 
    plt.types.Rational.prototype.toFloat = function() {
  return this.n / this.d;
    };
 
	plt.types.Rational.prototype.toComplex = function(){
		return plt.types.Complex.makeInstance(this.n / this.d, 0);
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
	var result = Math.sqrt(this.n / this.d);
	if (result == Math.floor(result)) {
	    return plt.types.Rational.makeInstance(result, 1);
	} else {
	    return plt.types.FloatPoint.makeInstance(result);
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
		return plt.types.FloatPoint.makeInstance(Math.log(this.n / this.d));
	};
	
	plt.types.Rational.prototype.angle = function(){
		if (0 == this.n)
			throw new Error("angle: undefined for 0");
		if (this.n > 0)
			return plt.types.Rational.ZERO;
		else
			return plt.Kernel.pi;
	};
	
	plt.types.Rational.prototype.atan = function(){
		return plt.types.FloatPoint.makeInstance(Math.atan(this.n / this.d));
	};
	
	plt.types.Rational.prototype.cos = function(){
		return plt.types.FloatPoint.makeInstance(Math.cos(this.n / this.d));
	};
	
	plt.types.Rational.prototype.sin = function(){
		return plt.types.FloatPoint.makeInstance(Math.sin(this.n / this.d));
	};
	
	plt.types.Rational.prototype.expt = function(a){
		return plt.types.FloatPoint.makeInstance(Math.pow(this.n / this.d, a.n / a.d));
	};
	
	plt.types.Rational.prototype.exp = function(){
		return plt.types.FloatPoint.makeInstance(Math.exp(this.n / this.d));
	};
	
	plt.types.Rational.prototype.acos = function(){
		return plt.types.FloatPoint.makeInstance(Math.acos(this.n / this.d));
	};
	
	plt.types.Rational.prototype.asin = function(){
		return plt.types.FloatPoint.makeInstance(Math.asin(this.n / this.d));
	};
	
	plt.types.Rational.prototype.imag_dash_part = function(){
		return Rational.ZERO;
	};
	
	plt.types.Rational.prototype.real_dash_part = function(){
		return this;
	};
	
	plt.types.Rational.prototype.round = function(){
		return this;
	};
	
	plt.types.Rational.prototype.HEREEEEEEEEEEEEEEEEE = function(){};
	
	plt.types.Rational.prototype.half = function(){
		return plt.types.Rational.makeInstance(this.n, this.d * 2);
	};
	
	plt.types.Rational.prototype.minus = function(){
		return plt.types.Rational.makeInstance(0 - this.n, this.d);
	};
 
 
    var _rationalCache = {};
    plt.types.Rational.makeInstance = function(n, d) {
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
      return new plt.types.Rational(n, d);
  }
    };
 
    _rationalCache = {};
    (function() {
  var i;
  for(i = -500; i < 500; i++)
      _rationalCache[i] = new plt.types.Rational(i, 1);
    })();
    plt.types.Rational.NEGATIVE_ONE = _rationalCache[-1];
    plt.types.Rational.ZERO = _rationalCache[0];
    plt.types.Rational.ONE = _rationalCache[1];
 
 
 
 
 
 
    plt.types.FloatPoint = function(n) {
  this.n = n;
    };
 
    plt.types.FloatPoint.prototype.toExact = function() {
      return plt.types.Rational.makeInstance(Math.floor(this.n), 1);
    };


    plt.types.FloatPoint.prototype.level = function() {
  return 1;
    };
 
    plt.types.FloatPoint.prototype.lift = function(target) {
		return plt.types.Complex.makeInstance(this.n, 0);
    };
 
    plt.types.FloatPoint.prototype.toString = function() {
  return this.n.toString();
    };
 
    plt.types.FloatPoint.prototype.isEqual = function(other) {
  return other instanceof plt.types.FloatPoint &&
      this.n == other.n;
    };

    plt.types.FloatPoint.prototype.isReal = function() {
	return true;
    };
 
    plt.types.FloatPoint.prototype.add = function(other) {
  return plt.types.FloatPoint.makeInstance(this.n + other.n);
    };
 
    plt.types.FloatPoint.prototype.subtract = function(other) {
  return plt.types.FloatPoint.makeInstance(this.n - other.n);
    };
 
    plt.types.FloatPoint.prototype.multiply = function(other) {
  return plt.types.FloatPoint.makeInstance(this.n * other.n);
    };
 
    plt.types.FloatPoint.prototype.divide = function(other) {
  return plt.types.FloatPoint.makeInstance(this.n / other.n);
    };
 
 
    plt.types.FloatPoint.prototype.toInteger = function() {
  return Math.floor(this.n);  
    };
 
    plt.types.FloatPoint.prototype.toFloat = function() {
  return this.n;
    };
	
	plt.types.FloatPoint.prototype.toComplex = function(){
		return plt.types.Complex.makeInstance(this.n, 0);
	};
 
    plt.types.FloatPoint.prototype.floor = function() {
  return plt.types.Rational.makeInstance(Math.floor(this.n), 1);
    };
 
    plt.types.FloatPoint.prototype.ceiling = function() {
  return plt.types.Rational.makeInstance(Math.ceil(this.n), 1);
    };
 


    plt.types.FloatPoint.prototype.greaterThan = function(other) {
	return this.n > other.n;
    };
 
    plt.types.FloatPoint.prototype.greaterThanOrEqual = function(other) {
  return this.n >= other.n;
    };
 
    plt.types.FloatPoint.prototype.lessThan = function(other) {
  return this.n < other.n;
    };
 
    plt.types.FloatPoint.prototype.lessThanOrEqual = function(other) {
	return this.n <= other.n;
    };

 
    plt.types.FloatPoint.prototype.sqrt = function() {
	if (this.n < 0)
		return plt.types.Complex.makeInstance(0, Math.sqrt(0 - this.n));
	else
		return plt.types.FloatPoint.makeInstance(Math.sqrt(this.n));
    };
 
    plt.types.FloatPoint.prototype.abs = function() {
  return plt.types.FloatPoint.makeInstance(Math.abs(this.n));
    };
 
    plt.types.FloatPoint.makeInstance = function(n) {
  return new plt.types.FloatPoint(n);
    };
	
	plt.types.FloatPoint.prototype.log = function(){
		if (this.n < 0)
			return this.toComplex().log();
		else
			return plt.types.FloatPoint.makeInstance(Math.log(this.n));
	};
	
	plt.types.FloatPoint.prototype.angle = function(){
		if (0 == this.n)
			throw new Error("angle: undefined for 0");
		if (this.n > 0)
			return plt.types.Rational.ZERO;
		else
			return plt.Kernel.pi;
	};
	
	plt.types.FloatPoint.prototype.atan = function(){
		return plt.types.FloatPoint.makeInstance(Math.atan(this.n));
	};
	
	plt.types.FloatPoint.prototype.cos = function(){
		return plt.types.FloatPoint.makeInstance(Math.cos(this.n));
	};
	
	plt.types.FloatPoint.prototype.sin = function(){
		return plt.types.FloatPoint.makeInstance(Math.sin(this.n));
	};
	
	plt.types.FloatPoint.prototype.expt = function(a){
		return plt.types.FloatPoint.makeInstance(Math.pow(this.n, a.n));
	};
	
	plt.types.FloatPoint.prototype.exp = function(){
		return plt.types.FloatPoint.makeInstance(Math.exp(this.n));
	};
	
	plt.types.FloatPoint.prototype.acos = function(){
		return plt.types.FloatPoint.makeInstance(Math.acos(this.n));
	};
	
	plt.types.FloatPoint.prototype.asin = function(){
		return plt.types.FloatPoint.makeInstance(Math.asin(this.n));
	};
	
	plt.types.FloatPoint.prototype.imag_dash_part = function(){
		return plt.types.Rational.ZERO;
	};
	
	plt.types.FloatPoint.prototype.real_dash_part = function(){
		return this;
	};
	
	
	plt.types.FloatPoint.prototype.round = function(){
		if (plt.types.NumberTower.lessThan(this.subtract(FloatPoint.makeInstance(0.5)).floor(), this.floor()))
			return this.floor();
		else 
			return this.ceiling();
			
	};
	
	plt.types.FloatPoint.prototype.HEREEEEEEEEEEEEEEEEE = function(){};
	
	plt.types.FloatPoint.prototype.conjugate = plt.types.FloatPoint.prototype.abs;
	
	plt.types.FloatPoint.prototype.magnitude = plt.types.FloatPoint.prototype.abs;
	
	plt.types.FloatPoint.prototype.minus = function(){
		return plt.types.FloatPoint.makeInstance(0 - this.n);
	};
	
	plt.types.FloatPoint.prototype.half = function(){
		return plt.types.FloatPoint.makeInstance(this.n / 2);
	};	
	
	plt.types.FloatPoint.prototype.timesI = function(){
		return plt.types.Complex.makeInstance(0, this.n);
	};
 
    plt.Kernel.pi = plt.types.FloatPoint.makeInstance(Math.PI);
    plt.Kernel.e = plt.types.FloatPoint.makeInstance(Math.E);
 
	plt.types.Complex = function(r, i){
		this.r = plt.types.FloatPoint.makeInstance(r);
		this.i = plt.types.FloatPoint.makeInstance(i);
	};
 
	plt.types.Complex.makeInstance = function(r, i){
		return new plt.types.Complex(r, i);
	};
	
	plt.types.Complex.prototype.toExact = function() { 
	  if (! this.isReal()) {
	    throw new Error("inexact->exact: expects argument of type real number");
	  }
	  return this.r.toExact();
	};

	plt.types.Complex.prototype.level = function(){
		return 2;
	};
 
	plt.types.Complex.prototype.lift = function(target){
		throw new Error("Don't know how to lift Complex number");
	};
 
	plt.types.Complex.prototype.isEqual = function(other){
		return other instanceof plt.types.Complex  && this.r.isEqual(other.r) && this.i.isEqual(other.i);
    };

    plt.types.Complex.prototype.greaterThan = function(other) {
	if (! this.isReal() || ! other.isReal()) {
	    throw new Error(">: expects argument of type real number");
	}
	return this.r.greaterThan(other.r);
    };

    plt.types.Complex.prototype.greaterThanOrEqual = function(other) {
	if (! this.isReal() || ! other.isReal()) {
	    throw new Error(">: expects argument of type real number");
	}
	return this.r.greaterThanOrEqual(other.r);
    };

    plt.types.Complex.prototype.lessThan = function(other) {
	if (! this.isReal() || ! other.isReal()) {
	    throw new Error(">: expects argument of type real number");
	}
	return this.r.lessThan(other.r);
    };

    plt.types.Complex.prototype.lessThanOrEqual = function(other) {
	if (! this.isReal() || ! other.isReal()) {
	    throw new Error(">: expects argument of type real number");
	}
	return this.r.lessThanOrEqual(other.r);
    };


	
	plt.types.Complex.prototype.abs = function(){
		if (!plt.types.NumberTower.equal(this.i, plt.types.Rational.ZERO))
			throw new Error("abs: expects argument of type real number");
		return this.r.abs();
	};
	
	plt.types.Complex.prototype.toInteger = function(){
		if (!plt.types.NumberTower.equal(this.i, plt.types.Rational.ZERO))
			throw new Error("toInteger: expects argument of type real number");
		return this.r.toInteger();
	};
	
	plt.types.Complex.prototype.toFloat = function(){
		if (!plt.types.NumberTower.equal(this.i, plt.types.Rational.ZERO))
			throw new Error("toFloat: expects argument of type real number");
		return this.r.toFloat();
	};
	
	plt.types.Complex.prototype.toComplex = function(){
		return plt.types.Complex.makeInstance(this.r.n, this.i.n);
	};
	
	plt.types.Complex.prototype.add = function(other){
		return plt.types.Complex.makeInstance(this.r.n + other.r.n, this.i.n + other.i.n);
	};
	
	plt.types.Complex.prototype.subtract = function(other){
		return plt.types.Complex.makeInstance(this.r.n - other.r.n, this.i.n - other.i.n);
	};
	
	plt.types.Complex.prototype.multiply = function(other){
		var r = this.r.n * other.r.n - this.i.n * other.i.n;
		var i = this.r.n * other.i.n + this.i.n * other.r.n;
		return plt.types.Complex.makeInstance(r, i);
	};
	
	plt.types.Complex.prototype.divide = function(other){
		var con = other.conjugate();
		var up =  plt.types.NumberTower.multiply(this, con);
		var down = plt.types.NumberTower.multiply(other, con);
		return plt.types.Complex.makeInstance(up.r.n / down.r.n, up.i.n / down.r.n);
	};
	
	plt.types.Complex.prototype.conjugate = function(){
		return plt.types.Complex.makeInstance(this.r.n, 0 - this.i.n);
	};
	
	plt.types.Complex.prototype.magnitude = function(){
		var sum = plt.types.FloatPoint.makeInstance(this.r.n*this.r.n + this.i.n * this.i.n);
		return plt.types.FloatPoint.makeInstance(sum.sqrt().n);
	};
	
	plt.types.Complex.prototype.isReal = function(){
		return this.i.n == 0;
	};
	
	plt.types.Complex.prototype.sqrt = function(){
		if (this.isReal())
			return this.r.sqrt();
			
		// http://en.wikipedia.org/wiki/Square_root#Square_roots_of_negative_and_complex_numbers	
		var r_plus_x = plt.types.NumberTower.add(this.magnitude(), this.r);

		var r = r_plus_x.half().sqrt();
		var i = plt.types.NumberTower.divide(this.i, plt.types.NumberTower.multiply(r_plus_x, plt.types.FloatPoint.makeInstance(2)).sqrt());
		
		return plt.types.Complex.makeInstance(r.toFloat(), i.toFloat());
	};
	
	plt.types.Complex.prototype.log = function(){
		return plt.types.Complex.makeInstance(this.magnitude().log().toFloat(), this.angle().toFloat());
	};
	
	plt.types.Complex.prototype.angle = function(){
		if (this.isReal())
			return this.r.angle();
		if (0 == this.r.n){
			var tmp = plt.Kernel.pi.half();
			return this.i.n > 0 ? tmp : tmp.minus();
		} else {
			var tmp = plt.types.NumberTower.divide(this.i.abs(), this.r.abs()).atan();
			if (this.r.n > 0)
				return this.i.n > 0 ? tmp : tmp.minus();
			else
				return this.i.n > 0 ? plt.Kernel.pi.subtract(tmp) : tmp.subtract(plt.Kernel.pi);
		}
	};
	
	plt.types.Complex.prototype.atan = function(){
		if (this.isReal())
			return this.r.atan();
		var iz = this.timesI();
		var part1 = plt.types.Complex.makeInstance(1, iz.minus()).log();
		var part2 = plt.types.Complex.makeInstance(1, iz).log();
		return part1.subtract(part2).timesI().half();
	};
	
	plt.types.Complex.prototype.cos = function(){
		if (this.isReal())
			return this.r.cos();
		var iz = this.timesI();
		var iz_minus = iz.minus();
		
		return plt.types.NumberTower.add(iz.exp(), iz_minus.exp()).half();
	};
	
	plt.types.Complex.prototype.sin = function(){
		if (this.isReal())
			return this.r.sin();
		var iz = this.timesI();
		var iz_minus = iz.minus();
		var z2 = plt.types.Complex.makeInstance(0, 2);
		
		var exp_minus = plt.types.NumberTower.subtract(iz.exp(), iz_minus.exp());
		
		return plt.types.NumberTower.divide(exp_minus, z2);
	};
	
	plt.types.Complex.prototype.expt= function(y){
		var expo = y.multiply(this.log());
		return expo.exp();
	};
	
	plt.types.Complex.prototype.exp = function(){
		var part1 = this.r.exp();
		var part2 = plt.types.Complex.makeInstance(this.i.cos(), this.i.sin().timesI());
		
		return plt.types.NumberTower.multiply(part1, part2);
	};
	
	plt.types.Complex.prototype.acos = function(){
		if (this.isReal())
			return this.r.acos();
		var pi_half = plt.Kernel.pi.half();
		var iz = this.timesI();
		var root = plt.types.NumberTower.subtract(plt.types.Rational.ONE, this.multiply(this)).sqrt();
		var l = plt.types.NumberTower.add(iz, root).log().timesI();
		return plt.types.NumberTower.add(pi_half, l);
	};
	
	plt.types.Complex.prototype.asin = function(){
		if (this.isReal())
			return this.r.asin();
		var iz = this.timesI();
		var root = plt.types.NumberTower.subtract(plt.types.Rational.ONE, this.multiply(this)).sqrt();
		var ret = plt.types.NumberTower.add(iz, root).log().timesI().minus();
		// FIXME: missing return value!
		throw new Error("");
	};
	
	plt.types.Complex.prototype.ceiling = function(){
		if (!this.isReal())
			throw new Error("ceiling: can only be applied to real number");
		return this.r.ceiling();
	};
	
	plt.types.Complex.prototype.floor = function(){
		if (!this.isReal())
			throw new Error("floor: can only be applied to real number");
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
	
	plt.types.Complex.prototype.HEREEEEEEEEEEEEEEEEE = function(){};
	
	plt.types.Complex.prototype.timesI = function(){
		return this.multiply(plt.types.Complex.makeInstance(0, 1));
	};
	
	plt.types.Complex.prototype.minus = function(){
		return plt.types.Complex.makeInstance(0 - this.r.n, 0 - this.i.n);
	};
	
	plt.types.Complex.prototype.half = function(){
		return plt.types.Complex.makeInstance(this.r.n/2, this.i.n/2);
	};
 
    //////////////////////////////////////////////////////////////////////
    // NumberTower.
    // 
    // Currently only support Rational and Floating.
    plt.types.NumberTower = {};
 
 
    plt.types.NumberTower.toInteger = function(num) {
  return num.toInteger();
    };
 
    plt.types.NumberTower.toFloat = function(num) {
  return num.toFloat();
    };
 
    plt.types.NumberTower.abs = function(n) {
  return n.abs();
    };
 
    plt.types.NumberTower.toExact = function(x) {
      return x.toExact();
    };

    plt.types.NumberTower.add = function(x, y) {
  if (x.level() < y.level()) x = x.lift(y);
  if (y.level() < x.level()) y = y.lift(x);
  return x.add(y);
    };
 
    plt.types.NumberTower.subtract = function(x, y) {
  if (x.level() < y.level()) x = x.lift(y);
  if (y.level() < x.level()) y = y.lift(x);
  return x.subtract(y);
    };
 
    plt.types.NumberTower.multiply = function(x, y) {
  if (x.level() < y.level()) x = x.lift(y);
  if (y.level() < x.level()) y = y.lift(x);
  return x.multiply(y);
    };
 
    plt.types.NumberTower.divide = function(x, y) {
  if (x.level() < y.level()) x = x.lift(y);
  if (y.level() < x.level()) y = y.lift(x);
  return x.divide(y);
    };
 
    plt.types.NumberTower.equal = function(x, y) {
	if (x.level() < y.level()) x = x.lift(y);
  if (y.level() < x.level()) y = y.lift(x);
	
  return x.isEqual(y);
    };
 
    plt.types.NumberTower.approxEqual = function(x, y, delta) {
  // fixme: use delta
  return x.isEqual(y);
    };
	
    plt.types.NumberTower.greaterThanOrEqual = function(x, y){
	if (x.level() < y.level()) x = x.lift(y);
	if (y.level() < x.level()) y = y.lift(x);

	if (!(x.isReal() && y.isReal()))
	    throw new Error("greaterThanOrEqual: couldn't be applied to complex number");
	return x.greaterThanOrEqual(y);
    };
    
    plt.types.NumberTower.lessThanOrEqual = function(x, y){
	if (x.level() < y.level()) x = x.lift(y);
	if (y.level() < x.level()) y = y.lift(x);
	if (!(x.isReal() && y.isReal()))
	    throw new Error("lessThanOrEqual: couldn't be applied to complex number");
	return x.lessThanOrEqual(y);    	
    };
    
    plt.types.NumberTower.greaterThan = function(x, y){
	if (x.level() < y.level()) x = x.lift(y);
	if (y.level() < x.level()) y = y.lift(x);
	
	if (!(x.isReal() && y.isReal()))
	    throw new Error("greaterThan: couldn't be applied to complex number");
	return x.greaterThan(y);
	
    };
    
    plt.types.NumberTower.lessThan = function(x, y){
	if (x.level() < y.level()) x = x.lift(y);
	if (y.level() < x.level()) y = y.lift(x);

	if (!(x.isReal() && y.isReal()))
	    throw new Error("lessThan: couldn't be applied to complex number");
	return x.lessThan(y);
    };
 
    plt.types.NumberTower.modulo = function(m, n) {
  var result = 
      plt.types.Rational.makeInstance(m.toInteger() % n.toInteger(),
            1);
  if (plt.types.NumberTower.lessThan(result, plt.types.Rational.ZERO)) {
      return plt.types.NumberTower.add(result, n);
  }
  return result;
    };
 
    plt.types.NumberTower.sqr = function(x) {
  return plt.types.NumberTower.multiply(x, x);
    };
	
	plt.types.NumberTower.expt = function(x, y){
		if (x.level() < y.level()) x = x.lift(y);
		if (y.level() < x.level()) y = y.lift(x);
		return x.expt(y);
	};
 
 
 
 
})();
