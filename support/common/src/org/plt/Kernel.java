package org.plt;

// The Kernel class contains all of the builtins except for the world
// primitives, which lives under the j2me tree.

import java.util.Calendar;

import org.plt.checker.*;
import org.plt.types.*;
import net.dclausen.microfloat.*;
import org.plt.types.Bignum;

public class Kernel {
	private static Class stringClass;
	private static Class characterClass;
	private static Class listClass;
	private static Class pairClass;
	static {
		// Workaround bug in CLDC 1.0. Bug 4313427 doesn't allow us to use
		// java.lang.String.class directly.
		// http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4313427
		try {
			stringClass = Class.forName("java.lang.String");
			characterClass = Class.forName("java.lang.Character");
			listClass = Class.forName("org.plt.types.List");
			pairClass = Class.forName("org.plt.types.Pair");
		} catch (ClassNotFoundException e) {
			throw new SchemeException(e);
		}
	}

	static private java.util.Random randgen = new java.util.Random();

	static public org.plt.types.Number ZERO = Rational.ZERO;
	static public org.plt.types.Number ONE = Rational.ONE;
	static public org.plt.types.Number TWO = NumberTower.plus(ONE, ONE);
	static public org.plt.types.Number HALF = NumberTower.divide(ONE, TWO);
	static public org.plt.types.Number THREE = NumberTower.plus(ONE, TWO);
	static public org.plt.types.Number FOUR = NumberTower.plus(ONE, THREE);
	static public org.plt.types.Number FIVE = NumberTower.plus(ONE, FOUR);
	static public org.plt.types.Number SIX = 
	    NumberTower.multiply(TWO, THREE);

	// no-op: void -> void
	public static Object no_op() {
		return null;
	}

	public static Object no_op_worldEvent(Object world) {
		return world;
	}

	public static Object no_op_stopWhen(Object world) {
		return Logic.FALSE;
	}

	public static Object no_op_keyEvent(Object world, Object key) {
		return world;
	}


	public static Object no_op_messageEvent(Object world, Object aMessage) {
		return world;
	}

        public static Object no_op_locationChangeEvent(Object world, Object latitude, Object longitude) {
		return world;
	}



	// ////////////////////////////////////////////////////////////////////

	public static org.plt.types.Number pi = FloatPoint.PI;
	public static org.plt.types.Number e = FloatPoint.E;

	public static Object identity(Object x) {
		return x;
	}

	// Numerics

	// >=

	public static Logic _greaterthan__equal_(Object[] args) {
	    for(int i = 0; i < args.length - 1; i++) {
		if (! NumberTower.greaterThanEqual((org.plt.types.Number) args[i],
						   (org.plt.types.Number) args[i+1]))
		    return Logic.FALSE;
	    }
	    return Logic.TRUE;
	}

	// >
	public static Logic _greaterthan_(Object[] args) {

	    for(int i = 0; i < args.length - 1; i++) {
		if (! NumberTower.greaterThan((org.plt.types.Number) args[i],
					      (org.plt.types.Number) args[i+1]))
		    return Logic.FALSE;
	    }
	    return Logic.TRUE;
	}

	// <=
	public static Logic _lessthan__equal_(Object[] args) {
	    for(int i = 0; i < args.length - 1; i++) {
		if (! NumberTower.lessThanEqual((org.plt.types.Number) args[i],
						(org.plt.types.Number) args[i+1]))
		    return Logic.FALSE;
	    }
	    return Logic.TRUE;
	}

	// <
	public static Logic _lessthan_(Object[] args) {
	    for(int i = 0; i < args.length - 1; i++) {
		if (! NumberTower.lessThan((org.plt.types.Number) args[i],
					   (org.plt.types.Number) args[i+1]))
		    return Logic.FALSE;
	    }
	    return Logic.TRUE;
	}

	// =
	public static Logic _equal_(Object[] args) {
	    for(int i = 0; i < args.length - 1; i++) {
		if (! NumberTower.equal((org.plt.types.Number) args[i],
					   (org.plt.types.Number) args[i+1]))
		    return Logic.FALSE;
	    }
	    return Logic.TRUE;
	}

	// =~
	public static Logic _equal__tilde_(Object _n1, Object _n2, Object _n3) {
		return toLogic(NumberTower.approxEqual
			       ((org.plt.types.Number) _n1,
				(org.plt.types.Number) _n2,
				(org.plt.types.Number) _n3));
	}

	// +
	public static org.plt.types.Number _plus_(Object[] args) {
	    org.plt.types.Number currentSum = ZERO;
	    for(int i = 0; i < args.length; i++) {
		currentSum = NumberTower.plus
		    (currentSum,
		     (org.plt.types.Number) args[i]);
	    }
	    return currentSum;
	}

	// -
	public static org.plt.types.Number _dash_(Object[] args) {
	    org.plt.types.Number currentDifference = (org.plt.types.Number )args[0];
	    for(int i = 1; i < args.length; i++) {
		currentDifference = NumberTower.minus
		    (currentDifference,
		     (org.plt.types.Number) args[i]);
	    }
	    return currentDifference;
	}

	// *
    public static org.plt.types.Number _star_(Object[] args) {
	org.plt.types.Number currentProduct = ONE;
	for (int i = 0; i < args.length; i++) {
	    currentProduct = 
		NumberTower.multiply((org.plt.types.Number) args[i],
				     currentProduct);
	}
	return currentProduct;
    }

	// /
    public static org.plt.types.Number _slash_(Object[] args) {
	org.plt.types.Number currentDivision = 
	    (org.plt.types.Number) args[0];
	for (int i = 1; i < args.length; i++) {
	    currentDivision = NumberTower.divide(currentDivision,
						 (org.plt.types.Number) args[i]);
	}
	return currentDivision;
    }

	public static org.plt.types.Number abs(Object n) {
		return ((org.plt.types.Number) n).abs();
	}

	public static org.plt.types.Number acos(Object n) {
		return ((org.plt.types.Number) n).acos();
	}

	public static org.plt.types.Number sqrt(Object _n) {
		org.plt.types.Number n = (org.plt.types.Number) _n;
		return ((org.plt.types.Number) n).sqrt();
	}

	public static org.plt.types.Number modulo(Object _n1, Object _n2) {
		org.plt.types.Number n1 = (org.plt.types.Number) _n1;
		org.plt.types.Number n2 = (org.plt.types.Number) _n2;

		if (NumberTower.coerseLeft(n1, ONE) != null) {
			n1 = NumberTower.coerseLeft(n1, ONE);
		}
		if (NumberTower.coerseLeft(n2, ONE) != null) {
			n2 = NumberTower.coerseLeft(n2, ONE);
		}

		return n1.modulo(n2);
	}

	public static org.plt.types.Number floor(Object _n1) {
		return ((org.plt.types.Number) _n1).floor();
	}

	public static org.plt.types.Number ceiling(Object _n1) {
		return ((org.plt.types.Number) _n1).ceiling();
	}

	public static org.plt.types.Number sin(Object _n1) {
		return ((org.plt.types.Number) _n1).sin();
	}

	public static org.plt.types.Number asin(Object _n1) {
		return ((org.plt.types.Number) _n1).asin();
	}

	public static org.plt.types.Number atan(Object _n1) {
		return ((org.plt.types.Number) _n1).atan();
	}

	public static org.plt.types.Number cos(Object _n1) {
		return ((org.plt.types.Number) _n1).cos();
	}

	public static String number_dash__greaterthan_string(Object _n1) {
		return ((org.plt.types.Number) _n1).toString();
	}

    public static Logic equal_question_(Object _o1, Object _o2) {
	if (_o1 instanceof org.plt.types.Number
	    && _o2 instanceof org.plt.types.Number) {
	    return toLogic(NumberTower.equal((org.plt.types.Number)_o1,
					     (org.plt.types.Number)_o2));
	}
	return toLogic(_o1.equals(_o2));
    }

	// ////////////////////////////////////////////////////////////////////
	public static org.plt.types.Number random(Object n) {
		int result = randgen.nextInt() % ((org.plt.types.Number) n).toInt();
		// Oddity, but modulo can return negative if dividend is negative.
		if (result < 0) {
			return new Rational(result + ((org.plt.types.Number) n).toInt(), 1);
		} else {
			return new Rational(result, 1);
		}
	}

	public static Logic zero_question_(java.lang.Object n) {
		return toLogic(((org.plt.types.Number) n).isZero());
	}

	public static org.plt.types.Number max(Object[] args) {
	    org.plt.types.Number currentMax = 
		NumberTower.greaterThanEqual((org.plt.types.Number) args[0],
					     (org.plt.types.Number) args[1])
		? (org.plt.types.Number) args[0] : (org.plt.types.Number) args[1];

	    for(int i = 2; i < args.length; i++) {
		currentMax = 
		    NumberTower.greaterThanEqual(currentMax,
						 (org.plt.types.Number) args[i])
		    ? currentMax : (org.plt.types.Number) args[i];

	    }
	    return currentMax;
	}

	public static org.plt.types.Number min(Object[] args) {
	    org.plt.types.Number currentMin = 
		NumberTower.lessThanEqual((org.plt.types.Number) args[0],
					  (org.plt.types.Number) args[1])
		? (org.plt.types.Number) args[0] : (org.plt.types.Number) args[1];

	    for(int i = 2; i < args.length; i++) {
		currentMin = 
		    NumberTower.lessThanEqual(currentMin,
					      (org.plt.types.Number) args[i])
		    ? currentMin : (org.plt.types.Number) args[i];
		
	    }
	    return currentMin;
	}

	public static org.plt.types.Number sqr(Object n) {
	    return NumberTower.multiply((org.plt.types.Number)n,
					(org.plt.types.Number)n);
	}

	public static org.plt.types.Number add1(Object n) {
	    return NumberTower.plus((org.plt.types.Number) n, ONE);
	}

	public static org.plt.types.Number sub1(Object n) {
	    return NumberTower.minus((org.plt.types.Number) n, ONE);
	}

	// ////////////////////////////////////////////////////////////////////
	public static Logic string_equal__question_(Object[] args) {
		for (int i = 0; i < args.length - 1; i++) {
		    String s1 = (String) args[i];
		    String s2 = (String) args[i+1];
		    if (string_question_(s1).isFalse())
			throw new SchemeException(
						  "string=?: expects type <string> as 1st argument, given: "
						  + s1 + "; other arguments were: " + s2);
		    if (string_question_(s1).isFalse())
			throw new SchemeException(
						  "string=?: expects type <string> as 2nd argument, given: "
						  + s2 + "; other arguments were: " + s1);
		    if (! s1.equals(s2)) {
			return Logic.FALSE;
		    }
		    
		}
		return Logic.TRUE;
	}


	public static Logic struct_question_(Object obj) {
		return toLogic(obj instanceof org.plt.types.Struct);
	}

	// ////////////////////////////////////////////////////////////////////
	// Posn stuff
	public static Posn make_dash_posn(Object x, Object y) {
		return new Posn(x, y);
	}

	public static Object posn_dash_x(Object p) {
		return ((Posn) p).getX();
	}

	public static Object posn_dash_y(Object p) {
		return ((Posn) p).getY();
	}

	public static Logic posn_question_(Object p) {
		return toLogic(p instanceof org.plt.types.Posn);
	}

	// ////////////////////////////////////////////////////////////////////

	// ////////////////////////////////////////////////////////////////////

	public static Object first(Object l) {
		return ((org.plt.types.List) l).first();
	}

	public static Object second(Object _l) {
		org.plt.types.List l = (org.plt.types.List) _l;
		return l.rest().first();
	}

	public static Object third(Object _l) {
		org.plt.types.List l = (org.plt.types.List) _l;
		return l.rest().rest().first();
	}

	public static Object fourth(Object _l) {
		org.plt.types.List l = (org.plt.types.List) _l;
		return l.rest().rest().rest().first();
	}

	public static Object fifth(Object _l) {
		org.plt.types.List l = (org.plt.types.List) _l;
		return l.rest().rest().rest().rest().first();
	}

	public static Object sixth(Object _l) {
		org.plt.types.List l = (org.plt.types.List) _l;
		return l.rest().rest().rest().rest().rest().first();
	}

	public static Object seventh(Object _l) {
		org.plt.types.List l = (org.plt.types.List) _l;
		return l.rest().rest().rest().rest().rest().rest().first();
	}

	public static Object eighth(Object _l) {
		org.plt.types.List l = (org.plt.types.List) _l;
		return l.rest().rest().rest().rest().rest().rest().rest().first();
	}

	public static org.plt.types.List reverse(Object _l) {
		org.plt.types.List l = (org.plt.types.List) _l;
		org.plt.types.List rev = org.plt.types.Empty.EMPTY;
		while (!l.isEmpty()) {
			rev = cons(l.first(), rev);
			l = l.rest();
		}
		return rev;
	}

	public static org.plt.types.List rest(Object l) {
		return ((org.plt.types.List) l).rest();
	}

	public static Object car(Object o) {
		return first(o);
	}

	public static org.plt.types.List cdr(Object o) {
		return rest(o);
	}

	public static Object caaar(Object o) {
		return car(car(car(o)));
	}

	public static Object caadr(Object o) {
		return car(car(cdr(o)));
	}

	public static Object caar(Object o) {
		return car(car(o));
	}

	public static Object cadar(Object o) {
		return car(cdr(car(o)));
	}

	public static Object cadddr(Object o) {
		return car(cdr(cdr(cdr(o))));
	}

	public static Object caddr(Object o) {
		return car(cdr(cdr(o)));
	}

	public static Object cadr(Object o) {
		return car(cdr(o));
	}

	public static org.plt.types.List cdaar(Object o) {
		return cdr(car(car(o)));
	}

	public static org.plt.types.List cdadr(Object o) {
		return cdr(car(cdr(o)));
	}

	public static org.plt.types.List cdar(Object o) {
		return cdr(car(o));
	}

	public static org.plt.types.List cddar(Object o) {
		return cdr(cdr(car(o)));
	}

	public static org.plt.types.List cdddr(Object o) {
		return cdr(cdr(cdr(o)));
	}

	public static org.plt.types.List cddr(Object o) {
		return cdr(cdr(o));
	}

	public static Logic empty_question_(Object l) {
		return toLogic(((org.plt.types.List) l).isEmpty());
	}

	public static org.plt.types.List cons(Object x, Object xs) {
		return new Pair(x, (org.plt.types.List) xs);
	}

	// ////////////////////////////////////////////////////////////////////

	// ////////////////////////////////////////////////////////////////////
	public static Logic symbol_equal__question_(Object s1, Object s2) {
		return toLogic(((Symbol) s1).equals(s2));
	}

	public static String symbol_dash__greaterthan_string(Object o) {
		return ((Symbol) o).toString();
	}

	public static Logic not(Object l) {
		return ((Logic) l).negate();
	}

	// ////////////////////////////////////////////////////////////////////

	public static Object error(Object s, Object msg) {
		throw new SchemeException(s + ": " + msg);
	}

	public static Object error(Object s) {
		throw new SchemeException("" + s);
	}

	// ////////////////////////////////////////////////////////////////////

	// Converts from boolean to Logics.
	private static Logic toLogic(boolean b) {
		return b ? Logic.TRUE : Logic.FALSE;
	}

	public static org.plt.types.Number tan(Object _n1) {
		return NumberTower.divide(sin(_n1), cos(_n1));
	}

	public static org.plt.types.Number sinh(Object _n1) {
	    return NumberTower.divide(NumberTower.minus
			   (exp((org.plt.types.Number) _n1), 
			    exp(NumberTower.minus(ZERO,
						  (org.plt.types.Number)_n1))),
			   TWO);
	}

	public static org.plt.types.Number cosh(Object _n1) {
	    return NumberTower.divide(NumberTower.plus(exp(_n1), exp(NumberTower.minus(ZERO, (org.plt.types.Number)_n1))), TWO);
	}

	public static org.plt.types.Logic even_question_(Object n) {
		if (integer_question_(n).isFalse())
			throw new SchemeException(
					"even?: expects argument of type <integer>; given " + n);
		return zero_question_(modulo(n, TWO));
	}

	public static org.plt.types.Logic odd_question_(Object n) {
		if (integer_question_(n).isFalse())
			throw new SchemeException(
					"odd?: expects argument of type <integer>; given: " + n);
		return not(even_question_(n));
	}

	public static org.plt.types.Number expt(Object n1, Object n2) {
		if (number_question_(n1).isFalse())
			throw new SchemeException(
					"expt: expects type <number> as 1st argument, given: " + n1
							+ "; other arguments were: " + n2);
		if (number_question_(n2).isFalse())
			throw new SchemeException(
					"expt: expects type <number> as 2nd argument, given: " + n2
							+ "; other arguments were: " + n1);

		return (FloatPoint.fromNumber((org.plt.types.Number) n1))
				.expt((org.plt.types.Number) n2);
	}

	public static org.plt.types.Number exp(Object exponent) {
		if (number_question_(exponent).isFalse())
			throw new SchemeException(
					"exp: expects argument of type <number>; given " + exponent);

		return expt(e, exponent);
	}

	public static org.plt.types.Number log(Object n1) {
		if (number_question_(n1).isFalse())
			throw new SchemeException(
					"log: expects argument of type <number>; given: " + n1);

		return (FloatPoint.fromNumber((org.plt.types.Number) n1)).log();
	}

	public static org.plt.types.Logic positive_question_(Object n1) {
		if (number_question_(n1).isFalse())
			throw new SchemeException(
					"positive?: expects argument of type <real number>; given: "
							+ n1);

		return toLogic(NumberTower.greaterThan((org.plt.types.Number)n1, ZERO));
	}

	public static org.plt.types.Logic negative_question_(Object n1) {
		if (number_question_(n1).isFalse())
			throw new SchemeException(
					"negative?: expects argument of type <real number>; given: "
							+ n1);

		return toLogic(NumberTower.lessThan((org.plt.types.Number)n1, ZERO));
	}

	public static org.plt.types.Number sgn(Object n1) {
		if (number_question_(n1).isFalse())
			throw new SchemeException("sgn: expects argument of type <number>");

		if (positive_question_(n1).isTrue())
			return (org.plt.types.Number) ONE;

		if (negative_question_(n1).isTrue())
			return (org.plt.types.Number) (NumberTower.minus(ZERO, ONE));

		return ZERO;
	}

	public static org.plt.types.Logic boolean_question_(Object n1) {
		return toLogic(n1 instanceof org.plt.types.Logic);
	}

	public static org.plt.types.Logic false_question_(Object n1) {
		return ((org.plt.types.Logic) n1).negate();
	}

	public static org.plt.types.Logic boolean_equal__question_(Object n1,
			Object n2) {
		if (boolean_question_(n1).isFalse())
			throw new SchemeException(
					"boolean=?: expects type <boolean> as 1st argument, given: "
							+ n1 + "; other arguments were: " + n2);
		if (boolean_question_(n2).isFalse())
			throw new SchemeException(
					"boolean=?: expects type <boolean> as 2nt argument, given: "
							+ n2 + "; other arguments were: " + n1);

		return toLogic(((org.plt.types.Logic) n1).isTrue() == ((org.plt.types.Logic) n2)
				.isTrue());
	}

	public static org.plt.types.Logic symbol_question_(Object n) {
		return toLogic(n instanceof org.plt.types.Symbol);
	}

	public static org.plt.types.Number gcd(Object[] args) {
	    org.plt.types.Number currentGcd = gcd2((org.plt.types.Number) args[0], 
						   (org.plt.types.Number) args[1]);
	    for(int i = 2; i < args.length; i++) {
		currentGcd = gcd2(currentGcd, gcd2(currentGcd, 
						   (org.plt.types.Number) args[i]));
	    }
	    return currentGcd;
	}


    private static org.plt.types.Number gcd2(org.plt.types.Number a,
					     org.plt.types.Number b) {
	if (integer_question_(a).isFalse())
	    throw new SchemeException(
				      "gcd: expects type <integer> as 1st argument; giving: " + a);
	if (integer_question_(b).isFalse())
	    throw new SchemeException(
				      "gcd: expects type <integer> as 2nd argument; giving: " + b);

	if (negative_question_(a).isTrue())
	    a = NumberTower.minus(ZERO, (org.plt.types.Number) a);
	if (negative_question_(b).isTrue())
	    b = NumberTower.minus(ZERO, (org.plt.types.Number) b);

	while (! NumberTower.equal(b, ZERO)) {
	    org.plt.types.Number t = (org.plt.types.Number) b;
	    b = ((org.plt.types.Number) a).modulo((org.plt.types.Number) b);
	    a = t;
	}

	return (org.plt.types.Number) a;
    }



	public static org.plt.types.Number lcm(Object[] args) {
	    org.plt.types.Number commonDivisor = gcd(args);
	    org.plt.types.Number product = _star_(args);
	    return abs(NumberTower.divide(product, commonDivisor));
	}
    

	public static org.plt.types.Logic pair_question_(Object n) {
		return toLogic(n instanceof org.plt.types.Pair);
	}

	public static org.plt.types.Logic cons_question_(Object n) {
		return toLogic(n instanceof org.plt.types.Pair);
	}

	public static org.plt.types.Logic number_question_(Object n) {
		return toLogic(n instanceof org.plt.types.Number);
	}

	public static org.plt.types.Logic rational_question_(Object n) {
		return toLogic(n instanceof org.plt.types.Rational);
	}

	public static org.plt.types.Number quotient(Object a, Object b) {
		if (integer_question_(a).isFalse())
			throw new SchemeException(
					"quotient: expects type <integer> as 1st argument, given: "
							+ a);
		if (integer_question_(b).isFalse())
			throw new SchemeException(
					"quotient: expects type <integer> as 2nd argument, given: "
							+ b);

		return floor(NumberTower.divide((org.plt.types.Number)a,
						(org.plt.types.Number)b));
	}

	public static org.plt.types.Number remainder(Object a, Object b) {
		if (integer_question_(a).isFalse())
			throw new SchemeException(
					"remainder: expects type <integer> as 1st argument, given: "
							+ a);
		if (integer_question_(b).isFalse())
			throw new SchemeException(
					"remainder: expects type <integer> as 2nd argument, given: "
							+ b);

		return NumberTower.minus((org.plt.types.Number)a, NumberTower.multiply
			      (quotient((org.plt.types.Number)a,
					(org.plt.types.Number)b),
			       (org.plt.types.Number) b));
	}

	public static org.plt.types.Number numerator(Object n) {
		if (rational_question_(n).isFalse())
			throw new SchemeException(
					"numerator: expects argument of type <rational number>, giving: "
							+ n);
		return new Rational(((org.plt.types.Rational) n).numerator(),
				Bignum.ONE);
	}

	public static org.plt.types.Number denominator(Object n) {
		if (rational_question_(n).isFalse())
			throw new SchemeException(
					"denominator: expects argument of type <rational number>, giving: "
							+ n);

		return new Rational(((org.plt.types.Rational) n).denominator(),
				Bignum.ONE);
	}

	public static org.plt.types.Logic integer_question_(Object n) {
	    return toLogic(NumberTower.equal(Kernel.denominator(n), ONE));
	}

	public static org.plt.types.Logic null_question_(Object n) {
		return toLogic(((org.plt.types.List) n).isEmpty());
	}

	public static org.plt.types.Number length(Object n) {
		ArgumentChecker.checkAtomType(n, "org.plt.types.List", "length");

		// if (cons_question_(n).isFalse())
		// throw new SchemeException(
		// "length: expects argument of type <proper list>; given: "
		// + n);

		org.plt.types.Number len = ZERO;

		while (((org.plt.types.List) n).isEmpty() == false) {
			len = add1(len);
			n = ((org.plt.types.List) n).rest();
		}

		return len;
	}

	public static Object list_dash_ref(Object lst, Object i) {
		if (negative_question_(i).isTrue())
			throw new SchemeException(
					"list-ref: expects type <non-negative exact integer> as 2nd argument, given: "
							+ i + ";  other arguments were: " + lst);

		org.plt.types.Number len = length(lst);
		if (NumberTower.greaterThanEqual((org.plt.types.Number) i,
						 (org.plt.types.Number) len))
			throw new SchemeException("list-ref: index " + i
					+ " too large for " + lst);

		org.plt.types.Number index = ZERO;
		while (NumberTower.lessThan(index, (org.plt.types.Number) i)) {
			index = add1(index);
			lst = ((org.plt.types.List) lst).rest();
		}

		return ((org.plt.types.List) lst).first();
	}

	public static org.plt.types.Number round(Object n) {
		if (real_question_(n).isFalse())
			throw new SchemeException(
					"round: expects argument of type <real number>; given: "
							+ n);

		if (negative_question_(n).isFalse()) {
		    if (NumberTower.lessThan(NumberTower.minus((org.plt.types.Number)n, 
							       floor(n)),
					     HALF))
				return floor(n);
			else
				return ceiling(n);
		} else {
			return NumberTower.minus(ZERO, round(abs(n)));
		}
	}

	public static org.plt.types.Logic real_question_(Object n) {
		return (n instanceof org.plt.types.Number ? toLogic(((org.plt.types.Number) n)
				.isReal())
				: Logic.FALSE);
	}

	public static org.plt.types.Logic string_question_(Object n) {
		return toLogic(n instanceof String);
	}

	private static org.plt.types.Logic string_greaterthan__question_(
			Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "string>?");
		ArgumentChecker.checkArrayType(arr, stringClass, "string>?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (((String) n1).compareTo((String) n2) > 0);
					}
				}));
	}

	public static org.plt.types.Logic string_greaterthan__question_(Object n1,
			Object n2) {
		Object[] arr = { n1, n2 };
		return string_greaterthan__question_(arr);
	}

	private static org.plt.types.Logic string_greaterthan__equal__question_(
			Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "string>=?");
		ArgumentChecker.checkArrayType(arr, stringClass, "string>=?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (((String) n1).compareTo((String) n2) >= 0);
					}
				}));
	}

	public static org.plt.types.Logic string_greaterthan__equal__question_(
			Object n1, Object n2) {
		Object[] arr = { n1, n2 };
		return string_greaterthan__equal__question_(arr);
	}

	private static org.plt.types.Logic string_lessthan__question_(Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "string<?");
		ArgumentChecker.checkArrayType(arr, stringClass, "string<?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (((String) n1).compareTo((String) n2) < 0);
					}
				}));
	}

	public static org.plt.types.Logic string_lessthan__question_(Object n1,
			Object n2) {
		Object[] arr = { n1, n2 };
		return string_lessthan__question_(arr);
	}

	private static org.plt.types.Logic string_lessthan__equal__question_(
			Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "string<=?");
		ArgumentChecker.checkArrayType(arr, stringClass, "string<=?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (((String) n1).compareTo((String) n2) <= 0);
					}
				}));
	}

	public static org.plt.types.Logic string_lessthan__equal__question_(
			Object n1, Object n2) {
		Object[] arr = { n1, n2 };
		return string_lessthan__equal__question_(arr);
	}

	private static org.plt.types.Logic natural_question_(Object n) {
		return toLogic(integer_question_(n).isTrue()
				&& negative_question_(n).isFalse());
	}

	public static Object substring(Object s, Object begin, Object end) {
		if (string_question_(s).isFalse())
			throw new SchemeException(
					"substring: expects type <string> as 1st argument, given: "
							+ s + "; other arguments were: " + begin + " "
							+ end);
		if (natural_question_(begin).isFalse())
			throw new SchemeException(
					"substring: expects type <non-negative exact integer> as 2nd argument, given: "
							+ begin + "; other arguments were: " + s + " "
							+ end);
		if (natural_question_(end).isFalse())
			throw new SchemeException(
					"substring: expects type <non-negative exact integer> as 3rd argument, given: "
							+ end + "; other arguments were: " + s + " "
							+ begin);

		return ((String) s).substring(((Rational) begin).toInt(),
				((Rational) end).toInt());
	}

	public static Object string_dash_ref(Object s, Object i) {
		if (string_question_(s).isFalse())
			throw new SchemeException(
					"string-ref: expects type <string> as 1st argument, given: "
							+ s + "; other arguments were: " + i);
		if (natural_question_(i).isFalse())
			throw new SchemeException(
					"string-ref: expects type <non-negative exact integer> as 2nd argument, given: "
							+ i + "; other arguments were: " + s);
		if (NumberTower.greaterThan((org.plt.types.Number)i,
					    sub1(string_dash_length(s))))
			throw new SchemeException("string-ref: index " + i
					+ " out of range [0, " + sub1(string_dash_length(s))
					+ "] for string: " + s);

		return new Character(((String) s).charAt(((Rational) i).toInt()));
	}

	public static org.plt.types.Number string_dash_length(Object s) {
		if (string_question_(s).isFalse())
			throw new SchemeException(
					"string-length: expects argument of type <string>; given "
							+ s);
		return new Rational(((String) s).length(), 1);
	}

	public static Object string_dash_copy(Object s) {
		if (string_question_(s).isFalse())
			throw new SchemeException(
					"string-copy: expects argument of type <string>; given "
							+ s);

		return new String((String) s);
	}

	public static Object string_dash__greaterthan_number(Object s) {
		if (string_question_(s).isFalse())
			throw new SchemeException(
					"string->number: expects argument of type <string>; given "
							+ s);

		try {
			return new Rational(Integer.parseInt((String) s), 1);
		} catch (NumberFormatException e1) {
			try {
				return FloatPoint.fromString((String) s);
			} catch (NumberFormatException e2) {
				return Logic.FALSE;
			}
		}
	}

	public static org.plt.types.Logic eq_question_(Object n1, Object n2) {
		return toLogic(n1 == n2);
	}

	public static org.plt.types.Logic eqv_question_(Object n1, Object n2) {
		return toLogic(n1 == n2);
	}

	public static org.plt.types.Logic char_question_(Object n) {
		return toLogic(n instanceof Character);
	}

	public static org.plt.types.Logic char_equal__question_(Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "char=?");
		ArgumentChecker.checkArrayType(arr, characterClass, "char=?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (((Character) n1).charValue() == ((Character) n2)
								.charValue());
					}
				}));
	}


	public static org.plt.types.Logic char_lessthan__question_(Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "char<?");
		ArgumentChecker.checkArrayType(arr, characterClass, "char<?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (((Character) n1).charValue() < ((Character) n2)
								.charValue());
					}
				}));
	}


	public static org.plt.types.Logic char_lessthan__equal__question_(
			Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "char<=?");
		ArgumentChecker.checkArrayType(arr, characterClass, "char<=?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (((Character) n1).charValue() <= ((Character) n2)
								.charValue());
					}
				}));
	}


	public static org.plt.types.Logic char_greaterthan__question_(Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "char>?");
		ArgumentChecker.checkArrayType(arr, characterClass, "char>?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (((Character) n1).charValue() > ((Character) n2)
								.charValue());
					}
				}));
	}


    public static org.plt.types.Logic char_greaterthan__equal__question_(
			Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "char>=?");
		ArgumentChecker.checkArrayType(arr, characterClass, "char>=?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (((Character) n1).charValue() >= ((Character) n2)
								.charValue());
					}
				}));
	}

	public static org.plt.types.Logic char_dash_upper_dash_case_question_(
			Object n) {
		ArgumentChecker.checkAtomType(n, "java.lang.Character",
				"char-upper-case?");

		return toLogic(Character.isUpperCase(((Character) n).charValue()));
	}

	public static org.plt.types.Logic char_dash_lower_dash_case_question_(
			Object n) {
		ArgumentChecker.checkAtomType(n, "java.lang.Character",
				"char-lower-case?");

		return toLogic(Character.isLowerCase(((Character) n).charValue()));
	}

	public static org.plt.types.Logic char_dash_numeric_question_(Object n) {
		ArgumentChecker
				.checkAtomType(n, "java.lang.Character", "char-numeric?");

		return toLogic(Character.isDigit(((Character) n).charValue()));
	}

	public static Object char_dash_upcase(Object n) {
		ArgumentChecker.checkAtomType(n, "java.lang.Character", "char-upcase");

		return new Character(Character.toUpperCase(((Character) n).charValue()));
	}

	public static Object char_dash_downcase(Object n) {
		ArgumentChecker
				.checkAtomType(n, "java.lang.Character", "char-downcase");

		return new Character(Character.toLowerCase(((Character) n).charValue()));
	}

	public static org.plt.types.Logic char_dash_whitespace_question_(Object n) {
		ArgumentChecker.checkAtomType(n, "java.lang.Character",
				"char-whitespace?");

		return toLogic(((Character) n).charValue() == ' ');
	}

	public static org.plt.types.Logic char_dash_alphabetic_question_(Object n) {
		ArgumentChecker.checkAtomType(n, "java.lang.Character",
				"char-alphabetic?");

		char v = ((Character) n).charValue();

		return toLogic((v >= 'a' && v <= 'z') || (v >= 'A' && v <= 'Z'));
	}

	public static org.plt.types.Logic char_dash_ci_equal__question_(
			Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "char-ci=?");
		ArgumentChecker.checkArrayType(arr, characterClass, "char-ci=?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (Character.toLowerCase(((Character) n1)
								.charValue()) == Character
								.toLowerCase(((Character) n2).charValue()));
					}
				}));
	}


	public static org.plt.types.Logic char_dash_ci_greaterthan__question_(
			Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "char-ci>?");
		ArgumentChecker.checkArrayType(arr, characterClass, "char-ci>?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (Character.toLowerCase(((Character) n1)
								.charValue()) > Character
								.toLowerCase(((Character) n2).charValue()));
					}
				}));
	}

	public static org.plt.types.Logic char_dash_ci_greaterthan__equal__question_(
			Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "char-ci>=?");
		ArgumentChecker.checkArrayType(arr, characterClass, "char-ci>=?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (Character.toLowerCase(((Character) n1)
								.charValue()) >= Character
								.toLowerCase(((Character) n2).charValue()));
					}
				}));
	}


	public static org.plt.types.Logic char_dash_ci_lessthan__question_(
			Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "char-ci<?");
		ArgumentChecker.checkArrayType(arr, characterClass, "char-ci<?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (Character.toLowerCase(((Character) n1)
								.charValue()) < Character
								.toLowerCase(((Character) n2).charValue()));
					}
				}));
	}

	public static org.plt.types.Logic char_dash_ci_lessthan__equal__question_(
			Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "char-ci<=?");
		ArgumentChecker.checkArrayType(arr, characterClass, "char-ci<=?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (Character.toLowerCase(((Character) n1)
								.charValue()) <= Character
								.toLowerCase(((Character) n2).charValue()));
					}
				}));
	}


	public static org.plt.types.Logic string_dash_ci_equal__question_(
			Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "string-ci=?");
		ArgumentChecker.checkArrayType(arr, stringClass, "string-ci=?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (((String) n1).toLowerCase().compareTo(
								((String) n2).toLowerCase()) == 0);
					}
				}));
	}


	public static org.plt.types.Logic string_dash_ci_greaterthan__question_(
			Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "string-ci>?");
		ArgumentChecker.checkArrayType(arr, stringClass, "string-ci>?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (((String) n1).toLowerCase().compareTo(
								((String) n2).toLowerCase()) > 0);
					}
				}));
	}


	public static org.plt.types.Logic string_dash_ci_greaterthan__equal__question_(
			Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "string-ci>=?");
		ArgumentChecker.checkArrayType(arr, stringClass, "string-ci>=?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (((String) n1).toLowerCase().compareTo(
								((String) n2).toLowerCase()) >= 0);
					}
				}));
	}


	public static org.plt.types.Logic string_dash_ci_lessthan__question_(
			Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "string-ci<?");
		ArgumentChecker.checkArrayType(arr, stringClass, "string-ci<?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (((String) n1).toLowerCase().compareTo(
								((String) n2).toLowerCase()) < 0);
					}
				}));
	}


	public static org.plt.types.Logic string_dash_ci_lessthan__equal__question_(
			Object[] arr) {
		ArgumentChecker.checkArraySize(arr, 2, "string-ci<=?");
		ArgumentChecker.checkArrayType(arr, stringClass, "string-ci<=?");

		return toLogic(ArgumentChecker.checkArrayType(arr,
				new RelationChecker() {
					public boolean hasRelation(Object n1, Object n2) {
						return (((String) n1).toLowerCase().compareTo(
								((String) n2).toLowerCase()) <= 0);
					}
				}));
	}


	public static org.plt.types.Symbol string_dash__greaterthan_symbol(Object n) {
		ArgumentChecker.checkAtomType(n, "java.lang.String", "string->symbol");

		return Symbol.makeInstance((String) n);
	}

	public static org.plt.types.List string_dash__greaterthan_list(Object n) {
		ArgumentChecker.checkAtomType(n, "java.lang.String", "string->list");

		org.plt.types.List ret = Empty.EMPTY;

		for (int i = ((String) n).length() - 1; i >= 0; i--)
			ret = new Pair(new Character(((String) n).charAt(i)), ret);

		return ret;
	}

	public static Object string_dash_append(Object[] arr) {
		ArgumentChecker.checkArrayType(arr, stringClass, "string-append");

		StringBuffer buf = new StringBuffer();
		for (int i = 0; i < arr.length; i++) {
			buf.append(arr[i]);
		}
		return buf.toString();
	}

	public static Object string(Object[] arr) {
		ArgumentChecker.checkArrayType(arr, characterClass, "string");

		char[] ret = new char[arr.length];
		for (int i = 0; i < arr.length; i++)
			ret[i] = ((Character) arr[i]).charValue();

		return new String(ret);
	}

	public static Object make_dash_string(Object n1, Object n2) {
		if (natural_question_(n1).isFalse())
			throw new SchemeException(
					"make-string: expects type <non-negative exact integer> as 1st argument, given: "
							+ n1 + "; other arguments were: " + n2);
		if (char_question_(n2).isFalse())
			throw new SchemeException(
					"make-string: expects type <character> as 2nd argument, given: "
							+ n2 + "; other arguments were: " + n1);

		char[] ret = new char[((org.plt.types.Number) n1).toInt()];
		for (int i = 0; i < ret.length; i++)
			ret[i] = ((Character) n2).charValue();

		return new String(ret);
	}

	public static Object list_dash__greaterthan_string(Object n) {
		ArgumentChecker.checkListType(n, characterClass, "list->string");

		char[] ret = new char[Kernel.length(n).toInt()];
		for (int i = 0; i < ret.length; i++) {
			ret[i] = ((Character) ((org.plt.types.List) n).first()).charValue();
			n = ((org.plt.types.List) n).rest();
		}

		return new String(ret);
	}

	public static org.plt.types.Number char_dash__greaterthan_integer(Object n) {
		ArgumentChecker
				.checkAtomType(n, "java.lang.Character", "char->integer");

		return new Rational((int) ((Character) n).charValue(), 1);
	}

	public static Object integer_dash__greaterthan_char(Object n) {
		// ArgumentCheck.checkAtomType(n, "org.plt.types.Number")
		ArgumentChecker.checkAtomType(n, new PropertyChecker() {
			public boolean satisfied(Object n) {
				return (n instanceof org.plt.types.Number)
						&& ((org.plt.types.Number) n).isInteger();
			}
		}, "exact integer", "integer->char");

		return new Character((char) (((org.plt.types.Number) n).toInt()));
	}

	public static org.plt.types.Logic member(Object n1, Object n2) {
		if ((n2 instanceof org.plt.types.List) == false)
			throw new SchemeException(
					"member: second argument must be of type <list>, given: "
							+ n1 + " and " + n2);

		while (((org.plt.types.List) n2).isEmpty() == false
				&& Kernel
						.equal_question_(n1, ((org.plt.types.List) n2).first())
						.isFalse())
			n2 = ((org.plt.types.List) n2).rest();

		return toLogic(((org.plt.types.List) n2).isEmpty() == false);
	}

	public static Object memq(Object n1, Object n2) {
		if ((n2 instanceof org.plt.types.List) == false)
			throw new SchemeException(
					"memq: second argument must be of type <list>, given: "
							+ n1 + " and " + n2);

		while (((org.plt.types.List) n2).isEmpty() == false
				&& Kernel.eq_question_(n1, ((org.plt.types.List) n2).first())
						.isFalse())
			n2 = ((org.plt.types.List) n2).rest();

		if (((org.plt.types.List) n2).isEmpty() == false)
			return n2;
		else
			return Logic.FALSE;
	}

	public static Object memv(Object n1, Object n2) {
		if ((n2 instanceof org.plt.types.List) == false)
			throw new SchemeException(
					"memv: second argument must be of type <list>, given: "
							+ n1 + " and " + n2);

		while (((org.plt.types.List) n2).isEmpty() == false
				&& Kernel.eqv_question_(n1, ((org.plt.types.List) n2).first())
						.isFalse())
			n2 = ((org.plt.types.List) n2).rest();

		if (((org.plt.types.List) n2).isEmpty() == false)
			return n2;
		else
			return Logic.FALSE;
	}

	public static org.plt.types.List append(Object n1, Object n2) {
		Object[] args = { n1, n2 };
		ArgumentChecker.checkArrayType(args, listClass, "append");

		org.plt.types.List rev = reverse(n1);
		while (!rev.isEmpty()) {
			n2 = cons(rev.first(), n2);
			rev = rev.rest();
		}

		return (org.plt.types.List) n2;
	}

	public static Object assq(Object n1, Object n2) {
		ArgumentChecker.checkListType(n2, pairClass, "assq", 2);
		while (((org.plt.types.List) n2).isEmpty() == false
				&& Kernel.eq_question_(
						n1,
						(((org.plt.types.Pair) ((org.plt.types.List) n2)
								.first())).first()).isFalse())
			n2 = ((org.plt.types.List) n2).rest();

		if (((org.plt.types.List) n2).isEmpty() == false)
			return ((org.plt.types.Pair) n2).first();
		else
			return Logic.FALSE;
	}

	public static org.plt.types.Number current_dash_seconds() {
		return new Rational(
				(int) (Calendar.getInstance().getTime().getTime() / 1000), 1);

	}

	public static org.plt.types.Logic complex_question_(Object n) {
		return number_question_(n);
	}

	public static org.plt.types.Number real_dash_part(Object n) {
		ArgumentChecker.checkAtomType(n, "org.plt.types.Number", "real_part");

		if (n instanceof Complex)
			return ((Complex) n).getRealPart();
		else
			return (org.plt.types.Number) n;
	}

	public static org.plt.types.Number imag_dash_part(Object n) {
		ArgumentChecker.checkAtomType(n, "org.plt.types.Number", "imag-part");

		if (n instanceof Complex)
			return ((Complex) n).getImgPart();
		else
			return FloatPoint.ZERO;
	}

	public static void exit() {
		System.exit(0);
	}

	public static org.plt.types.Logic equal_tilde__question_(Object n1,
			Object n2, Object n3) {
		ArgumentChecker.checkAtomType(n3, new PropertyChecker() {
			public boolean satisfied(Object n) {
				return (real_question_(n).isTrue() && negative_question_(n)
						.isFalse());
			}
		}, "non-negative-real", "equal~?", 3);

		if (real_question_(n1).isTrue() && real_question_(n2).isTrue())
			return _equal__tilde_(n1, n2, n3);

		return equal_question_(n1, n2);
	}

	public static org.plt.types.Logic exact_question_(Object n) {
		ArgumentChecker.checkAtomType(n, "org.plt.types.Number", "exact?");

		if (rational_question_(n).isTrue())
			return Logic.TRUE;
		if (n instanceof Complex
				&& ((Complex) n).getRealPart() instanceof Rational)
			return Logic.TRUE;
		if (n instanceof Complex
				&& ((Complex) n).getRealPart() instanceof Rational
				&& ((Complex) n).getImgPart() instanceof Rational)
			return Logic.TRUE;

		return Logic.FALSE;
	}

	public static org.plt.types.Logic inexact_question_(Object n) {
		ArgumentChecker.checkAtomType(n, "org.plt.types.Number", "inexact?");

		return exact_question_(n).negate();
	}

	public static org.plt.types.Number exact_dash__greaterthan_inexact(Object n) {
		ArgumentChecker.checkAtomType(n, "org.plt.types.Number",
				"exact->inexact");

		if (inexact_question_(n).isTrue())
			return (org.plt.types.Number) n;
		else
			return ((Rational) n).toFloatPoint();
	}

	public static org.plt.types.Number inexact_dash__greaterthan_exact(Object n) {
		ArgumentChecker.checkAtomType(n, "org.plt.types.Number",
				"inexact->exact");

		if (exact_question_(n).isTrue())
			return (org.plt.types.Number) n;
		else
			return new Rational(((FloatPoint) n).toInt(), 1);
	}

	public static org.plt.types.Number angle(Object n) {
		ArgumentChecker.checkAtomType(n, "org.plt.types.Number", "angle");

		return ((org.plt.types.Number) n).angle();
	}

	public static org.plt.types.Number conjugate(Object n) {
		ArgumentChecker.checkAtomType(n, "org.plt.types.Number", "conjugate");

		return ((org.plt.types.Number) n).conjugate();
	}

	public static org.plt.types.Number magnitude(Object n) {
		ArgumentChecker.checkAtomType(n, "org.plt.types.Number", "magnitude");

		return ((org.plt.types.Number) n).magnitude();
	}


    public static org.plt.types.Logic eof_dash_object_question_(Object n) {
	if (n instanceof EofObject) {
	    return Logic.TRUE;
	} else {
	    return Logic.FALSE;
	}
    }
}
