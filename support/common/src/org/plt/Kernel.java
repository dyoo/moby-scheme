package org.plt;

// The Kernel class contains all of the builtins except for the world
// primitives, which lives under the j2me tree.

import org.plt.types.*;
import net.dclausen.microfloat.*;
import org.plt.types.Bignum;

public class Kernel {

	static private java.util.Random randgen = new java.util.Random();

	static public org.plt.types.Number ZERO = Rational.ZERO;
	static public org.plt.types.Number ONE = Rational.ONE;
	static public org.plt.types.Number TWO = _plus_(ONE, ONE);
	static public org.plt.types.Number HALF = _slash_(ONE, TWO);
	static public org.plt.types.Number THREE = _plus_(ONE, TWO);
	static public org.plt.types.Number FOUR = _plus_(ONE, THREE);
	static public org.plt.types.Number FIVE = _plus_(ONE, FOUR);
	static public org.plt.types.Number SIX = _star_(TWO, THREE);

	private static void arrayTypeCheck(Object[] args, Class cl,
			String funName) {
	    for (int i = 0; i < args.length; i++) {
		if (cl.isInstance(args[i]) == false) {
		    String err = funName + ": expects type <" + cl.getName()
			+ "> as argument number " + (i + 1) + ", given: "
			+ args[i] + "; other arguments were ";
		    for (int j = 0; j < args.length; j++)
			if (j != i)
			    err += (args[j] + " ");
		    throw new SchemeException(err);
		}
	    }

	}

	private static void arraySizeCheck(Object[] args, int sizeLowerBound,
			String funName) {
		if (args.length < sizeLowerBound)
			throw new SchemeException(funName + ": expects at least "
					+ sizeLowerBound + " arguments, given " + args.length);
	}

	private static void itemTypeCheck(Object n, String desiredType,
			String funName) {
		try {
			Class cl = Class.forName(desiredType);
			if (cl.isInstance(n) == false) {
				String err = funName + ": expects argument of type <"
						+ desiredType + ">; given " + n;
				throw new SchemeException(err);
			}
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		}
	}

	private static interface RelationChecker {
		public boolean satisfied(Object n1, Object n2);
	}

	/**
	 * assumption: arr.length >= 2
	 */
	private static boolean arrayRelationCheck(Object[] arr, RelationChecker rc) {
		Object prev = arr[0];

		for (int i = 1; i < arr.length; i++) {
			if (rc.satisfied(prev, arr[i]) == false)
				return false;
			prev = arr[i];
		}

		return true;
	}

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

	// ////////////////////////////////////////////////////////////////////

	public static org.plt.types.Number pi = FloatPoint.PI;
	public static org.plt.types.Number e = FloatPoint.E;

	public static Object identity(Object x) {
		return x;
	}

	// Numerics

	// >=

	public static Logic _greaterthan__equal_(Object _n1, Object _n2) {
		return toLogic(NumberTower.greaterThanEqual((org.plt.types.Number) _n1,
				(org.plt.types.Number) _n2));
	}

	// >
	public static Logic _greaterthan_(Object _n1, Object _n2) {
		return toLogic(NumberTower.greaterThan((org.plt.types.Number) _n1,
				(org.plt.types.Number) _n2));
	}

	// <=

	public static Logic _lessthan__equal_(Object _n1, Object _n2) {
		return toLogic(NumberTower.lessThanEqual((org.plt.types.Number) _n1,
				(org.plt.types.Number) _n2));
	}

	// <
	public static Logic _lessthan_(Object _n1, Object _n2) {
		return toLogic(NumberTower.lessThan((org.plt.types.Number) _n1,
				(org.plt.types.Number) _n2));
	}

	// =
	public static Logic _equal_(Object _n1, Object _n2) {
		return toLogic(NumberTower.equal((org.plt.types.Number) _n1,
				(org.plt.types.Number) _n2));
	}

	// =~
	public static Logic _equal__tilde_(Object _n1, Object _n2, Object _n3) {
		return toLogic(NumberTower.approxEqual((org.plt.types.Number) _n1,
				(org.plt.types.Number) _n2, (org.plt.types.Number) _n3));
	}

	// +
	public static org.plt.types.Number _plus_(Object _n1, Object _n2) {
		return NumberTower.plus((org.plt.types.Number) _n1,
				(org.plt.types.Number) _n2);
	}

	// -
	public static org.plt.types.Number _dash_(Object _n1, Object _n2) {
		return NumberTower.minus((org.plt.types.Number) _n1,
				(org.plt.types.Number) _n2);
	}

	// *
	public static org.plt.types.Number _star_(Object _n1, Object _n2) {
		return NumberTower.multiply((org.plt.types.Number) _n1,
				(org.plt.types.Number) _n2);
	}

	// /
	public static org.plt.types.Number _slash_(Object _n1, Object _n2) {
		return NumberTower.divide((org.plt.types.Number) _n1,
				(org.plt.types.Number) _n2);
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
			return _equal_(_o1, _o2);
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

	public static org.plt.types.Number max(Object n1, Object n2) {
		if (_greaterthan__equal_(n1, n2)

		.isTrue()) {
			return (org.plt.types.Number) n1;
		}
		return (org.plt.types.Number) n2;
	}

	public static org.plt.types.Number min(Object n1, Object n2) {
		if (_lessthan__equal_(n1, n2).isTrue()) {
			return (org.plt.types.Number) n1;
		}
		return (org.plt.types.Number) n2;
	}

	public static org.plt.types.Number sqr(Object n) {
		return _star_(n, n);
	}

	public static org.plt.types.Number add1(Object n) {
		return _plus_(n, ONE);
	}

	public static org.plt.types.Number sub1(Object n) {
		return _dash_(n, ONE);
	}

	// ////////////////////////////////////////////////////////////////////
	public static Logic string_equal__question_(Object s1, Object s2) {
		if (string_question_(s1).isFalse())
			throw new SchemeException(
					"string=?: expects type <string> as 1st argument, given: "
							+ s1 + "; other arguments were: " + s2);
		if (string_question_(s1).isFalse())
			throw new SchemeException(
					"string=?: expects type <string> as 2nd argument, given: "
							+ s2 + "; other arguments were: " + s1);

		return toLogic(((String) s1).equals(s2));
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
		return _slash_(sin(_n1), cos(_n1));
	}

	public static org.plt.types.Number sinh(Object _n1) {
		return _slash_(_dash_(exp(_n1), exp(_dash_(ZERO, _n1))), TWO);
	}

	public static org.plt.types.Number cosh(Object _n1) {
		return _slash_(_plus_(exp(_n1), exp(_dash_(ZERO, _n1))), TWO);
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

		return _greaterthan_(n1, ZERO);
	}

	public static org.plt.types.Logic negative_question_(Object n1) {
		if (number_question_(n1).isFalse())
			throw new SchemeException(
					"negative?: expects argument of type <real number>; given: "
							+ n1);

		return _lessthan_(n1, ZERO);
	}

	public static org.plt.types.Number sgn(Object n1) {
		if (number_question_(n1).isFalse())
			throw new SchemeException("sgn: expects argument of type <number>");

		if (positive_question_(n1).isTrue())
			return (org.plt.types.Number) ONE;

		if (negative_question_(n1).isTrue())
			return (org.plt.types.Number) (_dash_(ZERO, ONE));

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

	public static org.plt.types.Number gcd(Object a, Object b) {
		if (integer_question_(a).isFalse())
			throw new SchemeException(
					"gcd: expects type <integer> as 1st argument; giving: " + a);
		if (integer_question_(b).isFalse())
			throw new SchemeException(
					"gcd: expects type <integer> as 2nd argument; giving: " + b);

		if (negative_question_(a).isTrue())
			a = _dash_(ZERO, a);
		if (negative_question_(b).isTrue())
			b = _dash_(ZERO, b);

		while (_equal_(b, ZERO).isFalse()) {
			org.plt.types.Number t = (org.plt.types.Number) b;
			b = ((org.plt.types.Number) a).modulo((org.plt.types.Number) b);
			a = t;
		}

		return (org.plt.types.Number) a;
	}

	public static org.plt.types.Number lcm(Object a, Object b) {
		if (integer_question_(a).isFalse())
			throw new SchemeException(
					"lcm: expects type <integer> as 1st argument; giving: " + a);
		if (integer_question_(b).isFalse())
			throw new SchemeException(
					"lcm: expects type <integer> as 2nd argument; giving: " + b);
		if (negative_question_(a).isTrue())
			a = _dash_(ZERO, a);
		if (negative_question_(b).isTrue())
			b = _dash_(ZERO, b);

		org.plt.types.Number acc = ONE;
		org.plt.types.Number cd;

		while (_equal_(cd = gcd(a, b), ONE).isFalse()) {
			acc = _star_(acc, cd);
			a = _slash_(a, cd);
			b = _slash_(b, cd);
		}

		return _star_(acc, _star_(a, b));
	}

	public static org.plt.types.Logic pair_question_(Object n) {
		return toLogic(n instanceof org.plt.types.List);
	}

	public static org.plt.types.Logic cons_question_(Object n) {
		return toLogic(n instanceof org.plt.types.List);
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

		return floor(_slash_(a, b));
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

		return _dash_(a, _star_(quotient(a, b), b));
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
		return Kernel._equal_(Kernel.denominator(n), ONE);
	}

	public static org.plt.types.Logic null_question_(Object n) {
		return toLogic(((org.plt.types.List) n).isEmpty());
	}

	public static org.plt.types.Number length(Object n) {
		if (cons_question_(n).isFalse())
			throw new SchemeException(
					"length: expects argument of type <proper list>; given: "
							+ n);

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
		if (_greaterthan__equal_(i, len).isTrue())
			throw new SchemeException("list-ref: index " + i
					+ " too large for " + lst);

		org.plt.types.Number index = ZERO;
		while (_lessthan_(index, i).isTrue()) {
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
			if (_lessthan_(_dash_(n, floor(n)), HALF).isTrue())
				return floor(n);
			else
				return ceiling(n);
		} else {
			return _dash_(ZERO, round(abs(n)));
		}
	}

	public static org.plt.types.Logic real_question_(Object n) {
		return toLogic(n instanceof FloatPoint || n instanceof Rational);
	}

	public static org.plt.types.Logic string_question_(Object n) {
		return toLogic(n instanceof String);
	}

	private static org.plt.types.Logic string_greaterthan__question_(
			Object[] arr) {
		arraySizeCheck(arr, 2, "string>?");
		arrayTypeCheck(arr, java.lang.String.class, "string>?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
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
		arraySizeCheck(arr, 2, "string>=?");
		arrayTypeCheck(arr, java.lang.String.class, "string>=?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
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
		arraySizeCheck(arr, 2, "string<?");
		arrayTypeCheck(arr, java.lang.String.class, "string<?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
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
		arraySizeCheck(arr, 2, "string<=?");
		arrayTypeCheck(arr, java.lang.String.class, "string<=?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
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
		if (_greaterthan_(i, sub1(string_dash_length(s))).isTrue())
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
		// (eq? (string->number "3") (string->number (string-copy "3")))
		// returns true. However, (eq? (string->number "3.1") (string->number
		// "3.1")) returns false
		if (n1 instanceof Rational && n2 instanceof Rational)
			return equal_question_((Rational) n1, (Rational) n2);
		else
			return toLogic(n1 == n2);
	}

	public static org.plt.types.Logic char_question_(Object n) {
		return toLogic(n instanceof Character);
	}

	private static org.plt.types.Logic char_equal__question_(Object[] arr) {
		arraySizeCheck(arr, 2, "char=?");
		arrayTypeCheck(arr, java.lang.Character.class, "char=?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
				return (((Character) n1).charValue() == ((Character) n2)
						.charValue());
			}
		}));
	}

	public static org.plt.types.Logic char_equal__question_(Object n1, Object n2) {
		Object[] arr = { n1, n2 };
		return char_equal__question_(arr);
	}

	private static org.plt.types.Logic char_lessthan__question_(Object[] arr) {
		arraySizeCheck(arr, 2, "char<?");
		arrayTypeCheck(arr, java.lang.Character.class, "char<?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
				return (((Character) n1).charValue() < ((Character) n2)
						.charValue());
			}
		}));
	}

	public static org.plt.types.Logic char_lessthan__question_(Object n1,
			Object n2) {
		Object[] arr = { n1, n2 };
		return char_lessthan__question_(arr);
	}

	private static org.plt.types.Logic char_lessthan__equal__question_(
			Object[] arr) {
		arraySizeCheck(arr, 2, "char<=?");
		arrayTypeCheck(arr, java.lang.Character.class, "char<=?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
				return (((Character) n1).charValue() <= ((Character) n2)
						.charValue());
			}
		}));
	}

	public static org.plt.types.Logic char_lessthan__equal__question_(
			Object n1, Object n2) {
		Object[] arr = { n1, n2 };
		return char_lessthan__equal__question_(arr);
	}

	private static org.plt.types.Logic char_greaterthan__question_(Object[] arr) {
		arraySizeCheck(arr, 2, "char>?");
		arrayTypeCheck(arr, java.lang.Character.class, "char>?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
				return (((Character) n1).charValue() > ((Character) n2)
						.charValue());
			}
		}));
	}

	public static org.plt.types.Logic char_greaterthan__question_(Object n1,
			Object n2) {
		Object[] arr = { n1, n2 };
		return char_greaterthan__question_(arr);
	}

	private static org.plt.types.Logic char_greaterthan__equal__question_(
			Object[] arr) {
		arraySizeCheck(arr, 2, "char>=?");
		arrayTypeCheck(arr, java.lang.Character.class, "char>=?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
				return (((Character) n1).charValue() >= ((Character) n2)
						.charValue());
			}
		}));
	}

	public static org.plt.types.Logic char_greaterthan__equal__question_(
			Object n1, Object n2) {
		Object[] arr = { n1, n2 };
		return char_greaterthan__equal__question_(arr);
	}

	public static org.plt.types.Logic char_dash_upper_dash_case_question_(
			Object n) {
		itemTypeCheck(n, "java.lang.Character", "char-upper-case?");

		return toLogic(Character.isUpperCase(((Character) n).charValue()));
	}

	public static org.plt.types.Logic char_dash_lower_dash_case_question_(
			Object n) {
		itemTypeCheck(n, "java.lang.Character", "char-lower-case?");

		return toLogic(Character.isLowerCase(((Character) n).charValue()));
	}

	public static org.plt.types.Logic char_dash_numeric_question_(Object n) {
		itemTypeCheck(n, "java.lang.Character", "char-numeric?");

		return toLogic(Character.isDigit(((Character) n).charValue()));
	}

	public static Object char_dash_upcase(Object n) {
		itemTypeCheck(n, "java.lang.Character", "char-upcase");

		return new Character(Character.toUpperCase(((Character) n).charValue()));
	}

	public static Object char_dash_downcase(Object n) {
		itemTypeCheck(n, "java.lang.Character", "char-downcase");

		return new Character(Character.toLowerCase(((Character) n).charValue()));
	}

	public static org.plt.types.Logic char_dash_whitespace_question_(Object n) {
		itemTypeCheck(n, "java.lang.Character", "char-whitespace?");

		return toLogic(Character.isWhitespace(((Character) n).charValue()));
	}

	public static org.plt.types.Logic char_dash_alphabetic_question_(Object n) {
		itemTypeCheck(n, "java.lang.Character", "char-alphabetic?");

		return toLogic(Character.isLetter(((Character) n).charValue()));
	}

	private static org.plt.types.Logic char_dash_ci_equal__question_(
			Object[] arr) {
		arraySizeCheck(arr, 2, "char-ci=?");
		arrayTypeCheck(arr, java.lang.Character.class, "char-ci=?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
			    return (Character.toLowerCase(((Character) n1).charValue()) == 
				    Character.toLowerCase(((Character) n2).charValue()));
			}
		}));
	}

	public static org.plt.types.Logic char_dash_ci_equal__question_(Object n1,
			Object n2) {
		Object[] arr = { n1, n2 };
		return char_dash_ci_equal__question_(arr);
	}

	private static org.plt.types.Logic char_dash_ci_greaterthan__question_(
			Object[] arr) {
		arraySizeCheck(arr, 2, "char-ci>?");
		arrayTypeCheck(arr, java.lang.Character.class, "char-ci>?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
			    return (Character.toLowerCase(((Character) n1).charValue()) > Character
				    .toLowerCase(((Character) n2).charValue()));
			}
		}));
	}

	public static org.plt.types.Logic char_dash_ci_greaterthan__question_(
			Object n1, Object n2) {
		Object[] arr = { n1, n2 };
		return char_dash_ci_greaterthan__question_(arr);
	}

	private static org.plt.types.Logic char_dash_ci_greaterthan__equal__question_(
			Object[] arr) {
		arraySizeCheck(arr, 2, "char-ci>=?");
		arrayTypeCheck(arr, java.lang.Character.class, "char-ci>=?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
			    return (Character.toLowerCase(((Character) n1).charValue()) >= Character
				    .toLowerCase(((Character) n2).charValue()));
			}
		}));
	}

	public static org.plt.types.Logic char_dash_ci_greaterthan__equal__question_(
			Object n1, Object n2) {
		Object[] arr = { n1, n2 };
		return char_dash_ci_greaterthan__equal__question_(arr);
	}

	private static org.plt.types.Logic char_dash_ci_lessthan__question_(
			Object[] arr) {
		arraySizeCheck(arr, 2, "char-ci<?");
		arrayTypeCheck(arr, java.lang.Character.class, "char-ci<?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
			    return (Character.toLowerCase(((Character) n1).charValue()) < Character
				    .toLowerCase(((Character) n2).charValue()));
			}
		}));
	}

	public static org.plt.types.Logic char_dash_ci_lessthan__question_(
			Object n1, Object n2) {
		Object[] arr = { n1, n2 };
		return char_dash_ci_lessthan__question_(arr);
	}

	private static org.plt.types.Logic char_dash_ci_lessthan__equal__question_(
			Object[] arr) {
		arraySizeCheck(arr, 2, "char-ci<=?");
		arrayTypeCheck(arr, java.lang.Character.class, "char-ci<=?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
			    return (Character.toLowerCase(((Character) n1).charValue()) <= Character
				    .toLowerCase(((Character) n2).charValue()));
			}
		}));
	}

	public static org.plt.types.Logic char_dash_ci_lessthan__equal__question_(
			Object n1, Object n2) {
		Object[] arr = { n1, n2 };
		return char_dash_ci_lessthan__equal__question_(arr);
	}

	private static org.plt.types.Logic string_dash_ci_equal__question_(
			Object[] arr) {
		arraySizeCheck(arr, 2, "string-ci=?");
		arrayTypeCheck(arr, java.lang.String.class, "string-ci=?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
				return (((String) n1).toLowerCase().compareTo(
						((String) n2).toLowerCase()) == 0);
			}
		}));
	}

	public static org.plt.types.Logic string_dash_ci_equal__question_(
			Object n1, Object n2) {
		Object[] arr = { n1, n2 };
		return string_dash_ci_equal__question_(arr);
	}

	private static org.plt.types.Logic string_dash_ci_greaterthan__question_(
			Object[] arr) {
		arraySizeCheck(arr, 2, "string-ci>?");
		arrayTypeCheck(arr, java.lang.String.class, "string-ci>?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
				return (((String) n1).toLowerCase().compareTo(
						((String) n2).toLowerCase()) > 0);
			}
		}));
	}

	public static org.plt.types.Logic string_dash_ci_greaterthan__question_(
			Object n1, Object n2) {
		Object[] arr = { n1, n2 };
		return string_dash_ci_greaterthan__question_(arr);
	}

	private static org.plt.types.Logic string_dash_ci_greaterthan__equal__question_(
			Object[] arr) {
		arraySizeCheck(arr, 2, "string-ci>=?");
		arrayTypeCheck(arr, java.lang.String.class, "string-ci>=?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
				return (((String) n1).toLowerCase().compareTo(
						((String) n2).toLowerCase()) >= 0);
			}
		}));
	}

	public static org.plt.types.Logic string_dash_ci_greaterthan__equal__question_(
			Object n1, Object n2) {
		Object[] arr = { n1, n2 };
		return string_dash_ci_greaterthan__equal__question_(arr);
	}

	private static org.plt.types.Logic string_dash_ci_lessthan__question_(
			Object[] arr) {
		arraySizeCheck(arr, 2, "string-ci<?");
		arrayTypeCheck(arr, java.lang.String.class, "string-ci<?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
				return (((String) n1).toLowerCase().compareTo(
						((String) n2).toLowerCase()) < 0);
			}
		}));
	}

	public static org.plt.types.Logic string_dash_ci_lessthan__question_(
			Object n1, Object n2) {
		Object[] arr = { n1, n2 };
		return string_dash_ci_lessthan__question_(arr);
	}

	private static org.plt.types.Logic string_dash_ci_lessthan__equal__question_(
			Object[] arr) {
		arraySizeCheck(arr, 2, "string-ci<=?");
		arrayTypeCheck(arr, java.lang.String.class, "string-ci<=?");

		return toLogic(arrayRelationCheck(arr, new RelationChecker() {
			public boolean satisfied(Object n1, Object n2) {
				return (((String) n1).toLowerCase().compareTo(
						((String) n2).toLowerCase()) <= 0);
			}
		}));
	}

	public static org.plt.types.Logic string_dash_ci_lessthan__equal__question_(
			Object n1, Object n2) {
		Object[] arr = { n1, n2 };
		return string_dash_ci_lessthan__equal__question_(arr);
	}

	public static org.plt.types.Symbol string_dash__greaterthan_symbol(Object n) {
		itemTypeCheck(n, "java.lang.String", "string->symbol");

		return Symbol.makeInstance((String) n);
	}

	public static Object[] string_dash__greaterthan_list(Object n) {
		itemTypeCheck(n, "java.lang.String", "string->list");

		Character[] ret = new Character[((String) n).length()];

		for (int i = 0; i < ((String) n).length(); i++)
		    ret[i] = new Character(((String) n).charAt(i));

		return ret;
	}

	private static Object string_dash_append(Object[] arr) {
		arrayTypeCheck(arr, java.lang.String.class, "string-append");

		StringBuffer buf = new StringBuffer();
                for (int i = 0; i < arr.length; i++) {
		    buf.append(arr[i]);
                }
                return buf.toString();
	}

	public static Object string_dash_append(Object n1, Object n2) {
		Object[] arr = { n1, n2 };
		return string_dash_append(arr);
	}

	public static Object string(Object[] arr) {
		arrayTypeCheck(arr, java.lang.Character.class, "string");

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

	public static Object list_dash__greaterthan_string(Object[] arr) {
		arrayTypeCheck(arr, java.lang.Character.class, "list->string");

		char[] ret = new char[arr.length];

		for (int i = 0; i < ret.length; i++)
		    ret[i] = ((Character) arr[i]).charValue();

		return new String(ret);
	}
}
