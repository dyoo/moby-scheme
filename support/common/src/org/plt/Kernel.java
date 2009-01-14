package org.plt;

// The Kernel class contains all of the builtins except for the world
// primitives, which lives under the j2me tree.

import org.plt.types.*;

public class Kernel {

	static private java.util.Random randgen = new java.util.Random();

	static public org.plt.types.Number TWO = _plus_(Rational.ONE, Rational.ONE);
	static public org.plt.types.Number THREE = _plus_(Rational.ONE, TWO);
	static public org.plt.types.Number FOUR = _plus_(Rational.ONE, THREE);
	static public org.plt.types.Number FIVE = _plus_(Rational.ONE, FOUR);
	static public org.plt.types.Number SIX = _star_(TWO, THREE);

	// no-op: void -> void
	public static Object no_op() {
		return null;
	}

	public static Object no_op_worldEvent(Object x) {
		return x;
	}

	public static Object no_op_stopWhen(Object x) {
		return Logic.FALSE;
	}

	public static Object no_op_keyEvent(Object x, Object key) {
		return x;
	}

	// ////////////////////////////////////////////////////////////////////

	public static org.plt.types.Number pi = FloatPoint.PI;
	public static org.plt.types.Number e = FloatPoint.E;

	public static Object identity(Object o) {
		return o;
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

		if (NumberTower.coerseLeft(n1, Rational.ONE) != null) {
			n1 = NumberTower.coerseLeft(n1, Rational.ONE);
		}
		if (NumberTower.coerseLeft(n2, Rational.ONE) != null) {
			n2 = NumberTower.coerseLeft(n2, Rational.ONE);
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
		return _plus_(n, Rational.ONE);
	}

	public static org.plt.types.Number sub1(Object n) {
		return _dash_(n, Rational.ONE);
	}

	// ////////////////////////////////////////////////////////////////////
	public static Logic string_equal__question_(Object s1, Object s2) {
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
		return _slash_(_dash_(exp(_n1), exp(_dash_(Rational.ZERO, _n1))), TWO);
	}

	public static org.plt.types.Number cosh(Object _n1) {
		return _slash_(_plus_(exp(_n1), exp(_dash_(Rational.ZERO, _n1))), TWO);
	}

	public static org.plt.types.Logic even_question_(Object n) {
	    if (integer_question_(n).isTrue()) {
		while (_equal_(n, Rational.ZERO).isFalse()
		       && _equal_(abs(n), Rational.ONE).isFalse())
		    n = _dash_(abs(n), TWO);

		return _equal_(n, Rational.ZERO);
	    } else {
		throw new SchemeException("not an integer");
	    }
	}

	public static org.plt.types.Logic odd_question_(Object n) {
		return not(even_question_(n));
	}

	public static org.plt.types.Number expt(Object base, Object exponent) {
		if (_greaterthan__equal_(exponent, Rational.ZERO).isTrue()) {
			if (_equal_(exponent, Rational.ZERO).isTrue())
				return Rational.ONE;

			if (even_question_(exponent).isTrue()) {
				org.plt.types.Number half = expt(base, _slash_(exponent, TWO));
				return _star_(half, half);
			} else {
				org.plt.types.Number half = expt(base, _slash_(sub1(exponent),
						TWO));
				return _star_(base, _star_(half, half));
			}
		} else {
			return _slash_(Rational.ONE, expt(base, abs(exponent)));
		}
	}

	public static org.plt.types.Number exp(Object exponent) {
		return expt(e, exponent);
	}

	public static org.plt.types.Number log(Object n1) {
		return ((org.plt.types.FloatPoint) n1).log();
	}

	public static org.plt.types.Logic positive_question_(Object n1) {
		return _greaterthan_(n1, Rational.ZERO);
	}

	public static org.plt.types.Logic negative_question_(Object n1) {
		return _lessthan_(n1, Rational.ZERO);
	}

	public static org.plt.types.Number sgn(Object n1) {
		if (positive_question_(n1).isTrue())
			return (org.plt.types.Number) Rational.ONE;

		if (negative_question_(n1).isTrue())
			return (org.plt.types.Number) (_dash_(Rational.ZERO, Rational.ONE));

		return Rational.ZERO;
	}

	public static org.plt.types.Logic boolean_question_(Object n1) {
		return toLogic(n1 instanceof org.plt.types.Logic);
	}

	public static org.plt.types.Logic false_question_(Object n1) {
		return ((org.plt.types.Logic) n1).negate();
	}

	public static org.plt.types.Logic boolean_equal__question_(Object n1,
			Object n2) {
		return toLogic(((org.plt.types.Logic) n1).isTrue() == ((org.plt.types.Logic) n2)
				.isTrue());
	}

	public static org.plt.types.Logic symbol_question_(Object n) {
		return toLogic(n instanceof org.plt.types.Symbol);
	}

	public static org.plt.types.Number gcd(Object a, Object b) {
		if (negative_question_(a).isTrue())
			a = _dash_(Rational.ZERO, a);
		if (negative_question_(b).isTrue())
			b = _dash_(Rational.ZERO, b);

		while (_equal_(b, Rational.ZERO).isFalse()) {
			org.plt.types.Number t = (org.plt.types.Number) b;
			b = ((org.plt.types.Number) a).modulo((org.plt.types.Number) b);
			a = t;
		}

		return (org.plt.types.Number) a;
	}

	public static org.plt.types.Number lcm(Object a, Object b) {
		if (negative_question_(a).isTrue())
			a = _dash_(Rational.ZERO, a);
		if (negative_question_(b).isTrue())
			b = _dash_(Rational.ZERO, b);

		org.plt.types.Number acc = Rational.ONE;
		org.plt.types.Number cd;

		while (_equal_(cd = gcd(a, b), Rational.ONE).isFalse()) {
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
		return floor(_slash_(a, b));
	}

	public static org.plt.types.Number remainder(Object a, Object b) {
		return _dash_(a, _star_(quotient(a, b), b));
	}

	public static org.plt.types.Number numerator(Object n) {
		return new Rational(((org.plt.types.Rational) n).numerator(), 1);
	}

	public static org.plt.types.Number denominator(Object n) {
		return new Rational(((org.plt.types.Rational) n).denominator(), 1);
	}

	public static org.plt.types.Logic integer_question_(Object n) {
		return Kernel._equal_(Kernel.denominator(n), Rational.ONE);
	}

	public static org.plt.types.Logic null_question_(Object n) {
		return toLogic(((org.plt.types.List) n).isEmpty());
	}

	public static org.plt.types.Number length(Object n) {
		org.plt.types.Number len = Rational.ZERO;

		while (((org.plt.types.List) n).isEmpty() == false) {
			len = add1(len);
			n = ((org.plt.types.List) n).rest();
		}

		return len;
	}

	public static Object list_dash_ref(Object lst, Object i) {
		if (negative_question_(i).isTrue())
			error(Rational.ZERO,
					"list-ref: expects type <non-negative exact integer> as 2nd argument, given: "
							+ i + ";  other arguments were: " + lst);

		org.plt.types.Number len = length(lst);
		if (_greaterthan__equal_(i, len).isTrue())
			error(Rational.ZERO, "list-ref: index " + i + " too large for " + lst);
		
		org.plt.types.Number index = Rational.ZERO;
		while (_lessthan_(index, i).isTrue()){
			index = add1(index);
			lst = ((org.plt.types.List) lst).rest();
		}
		
		return ((org.plt.types.List) lst).first();
	}
	
//	public static org.plt.types.List list()
}
