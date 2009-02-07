package org.plt.types;

import net.dclausen.microfloat.MicroDouble;

public class FloatPoint implements Number {
	long f;

	private FloatPoint(long n) {
		this.f = n;
	}

	public boolean isReal() {
		return true;
	}

	public Number toReal() {
		return new FloatPoint(f);
	}

	public static FloatPoint ZERO = FloatPoint.fromString("0");
	public static FloatPoint MINUS_ONE = FloatPoint.fromString("-1");

	public static FloatPoint fromInt(int n) {
		return new FloatPoint(MicroDouble.intToDouble(n));
	}

	public static FloatPoint fromString(String s) {
		return new FloatPoint(MicroDouble.parseDouble(s));
	}

	public static FloatPoint fromNumber(Number n) {
		if (n instanceof Rational)
			return ((Rational) n).toFloatPoint();
		else
			return (FloatPoint) n;
	}

	public static FloatPoint PI = new FloatPoint(MicroDouble.PI);
	public static FloatPoint E = new FloatPoint(MicroDouble.E);

	public int toInt() {
		return MicroDouble.intValue(this.f);
	}

	public boolean isInteger() {
		return MicroDouble.truncate(this.f) == this.f;
	}

	public boolean isZero() {
		return MicroDouble.isZero(this.f);
	}

	public boolean numericGreaterThan(Number other) {
		return MicroDouble.gt(this.f, ((FloatPoint) other).f);
	}

	public boolean numericEqual(Number other) {
		return MicroDouble.eq(this.f, ((FloatPoint) other).f);
	}

	public boolean numericLessThan(Number other) {
		return MicroDouble.lt(this.f, ((FloatPoint) other).f);
	}

	public Number numericMultiply(Number _other) {
		FloatPoint other = (FloatPoint) _other;
		return new FloatPoint(MicroDouble.mul(this.f, other.f));
	}

	public Number numericDivide(Number _other) {
		FloatPoint other = (FloatPoint) _other;
		return new FloatPoint(MicroDouble.div(this.f, other.f));
	}

	public Number numericPlus(Number _other) {
		FloatPoint other = (FloatPoint) _other;
		return new FloatPoint(MicroDouble.add(this.f, other.f));
	}

	public Number numericMinus(Number _other) {
		FloatPoint other = (FloatPoint) _other;
		return new FloatPoint(MicroDouble.sub(this.f, other.f));
	}

	public Number abs() {
		return new FloatPoint(MicroDouble.abs(this.f));
	}

	public Number sqrt() {
		if (numericLessThan(ZERO)) {
			if (isInteger()) {
				Number minus = Rational.ZERO.numericMinus(new Rational(this
						.toInt(), 1));
				return new Complex(Rational.ZERO, minus.sqrt());
			} else
				return new Complex(ZERO, MINUS_ONE.numericMultiply(this).sqrt());
		}

		return new FloatPoint(MicroDouble.sqrt(this.f));
	}

	public Number sin() {
		return new FloatPoint(MicroDouble.sin(this.f));
	}

	public Number cos() {
		return new FloatPoint(MicroDouble.cos(this.f));
	}

	public Number tan() {
		return new FloatPoint(MicroDouble.tan(this.f));
	}

	public Number atan() {
		return new FloatPoint(MicroDouble.atan(this.f));
	}

	public Number asin() {
		return new FloatPoint(MicroDouble.asin(this.f));
	}

	public Number acos() {
		return new FloatPoint(MicroDouble.acos(this.f));
	}

	public Number log() {
		return new FloatPoint(MicroDouble.log(this.f));
	}

	public Number modulo(Number _other) {
		FloatPoint other = (FloatPoint) _other;
		return new FloatPoint(MicroDouble.mod(this.f, other.f));
	}

	public Number expt(Number exponent) {

		return new FloatPoint(MicroDouble.pow(this.f, fromNumber(exponent).f));
	}

	public static Number exp(Number exponent) {
		if (exponent.isReal())
			return FloatPoint.E.expt(exponent.toReal());

		// e ^ z = e ^ real_part(z) * e ^ (cos(img_part(z) +
		// i*sin(img_part(z))))
		Number ret1 = exp(((Complex) exponent).getRealPart());
		Number ret2 = exp(((Complex) exponent).getImgPart().cos().numericPlus(
				new Complex(ZERO, ((Complex) exponent).getImgPart().sin())));
		return ret1.numericMultiply(ret2);
	}

	public Number floor() {
		return new Rational(MicroDouble.intValue(MicroDouble.floor(this.f)), 1);
	}

	public Number ceiling() {
		return new Rational(MicroDouble.intValue(MicroDouble.ceil(this.f)), 1);
	}

	public String toString() {
		return MicroDouble.toString(this.f);
	}

	public boolean equals(Object other) {
		if (other instanceof FloatPoint) {
			return this.f == ((FloatPoint) other).f;
		} else {
			return false;
		}
	}

	public Number angle() {
		if (isZero())
			throw new SchemeException("angle: undefined for 0");
		if (numericGreaterThan(FloatPoint.ZERO))
			return FloatPoint.ZERO;
		else
			return FloatPoint.PI;
	}

	public Number conjugate() {
		return new FloatPoint(f);
	}

	public Number magnitude() {
		return abs();
	}
}
