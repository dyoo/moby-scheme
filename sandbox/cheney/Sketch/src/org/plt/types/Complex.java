package org.plt.types;

import org.plt.checker.*;

public class Complex implements Number {

	// either Rational or FloatPoint
	private Number real_part;
	// either Rational or FloatPoint
	private Number imag_part;

	private Number HALF = new Rational(1, 2);

	public Number getRealPart() {
		return real_part;
	}

	public boolean isReal() {
		return imag_part.isZero();
	}

	public Number toReal() {
		ArgumentChecker.checkAtomType(this, new PropertyChecker() {
			public boolean satisfied(Object n) {
				return ((org.plt.types.Number) n).isReal();
			}
		}, "real number", "toReal");

		return real_part;
	}

	public Number getImgPart() {
		return imag_part;
	}

	private static Number minus(Number n) {
		return NumberTower.multiply(new Rational(-1, 1), n);
	}

	private static Number half(Number n) {
		return NumberTower.multiply(new Rational(1, 2), n);
	}

	private static Number multiplyByI(Number n) {
		return NumberTower
				.multiply(new Complex(Rational.ZERO, Rational.ONE), n);
	}

	private static Number sqr(Number n) {
		return n.numericMultiply(n);
	}

	public Complex(int real, int imag) {
		real_part = new Rational(real, 1);
		imag_part = new Rational(imag, 1);
	}

	public Complex(Number real, Number imag) {
		ArgumentChecker.checkAtomType(real, new PropertyChecker() {
			public boolean satisfied(Object n) {
				return ((org.plt.types.Number) n).isReal();
			}
		}, "real number", "Complex constructor", 1);

		ArgumentChecker.checkAtomType(imag, new PropertyChecker() {
			public boolean satisfied(Object n) {
				return ((org.plt.types.Number) n).isReal();
			}
		}, "real number", "Complex constructor", 2);

		real_part = real.toReal();
		imag_part = imag.toReal();
	}

	public Number angle() {
		if (isReal()) {
			if (toReal().isZero())
				throw new SchemeException("angle: undefined for 0");
			if (NumberTower.greaterThan(toReal(), Rational.ZERO))
				return Rational.ZERO;
			else
				return FloatPoint.PI;
		} else {
			if (real_part.isZero()) {
				if (NumberTower.greaterThan(imag_part, Rational.ZERO))
					return half(FloatPoint.PI);
				else
					return minus(half(FloatPoint.PI));
			} else {
				Number tmp = NumberTower.divide(imag_part.abs(),
						real_part.abs()).atan();
				if (NumberTower.greaterThan(real_part, Rational.ZERO))
					return NumberTower.greaterThan(imag_part, Rational.ZERO) ? tmp
							: minus(tmp);
				else
					return NumberTower.greaterThan(imag_part, Rational.ZERO) ? NumberTower
							.minus(FloatPoint.PI, tmp)
							: NumberTower.minus(tmp, FloatPoint.PI);
			}
		}
	}

	public Number conjugate() {
		return (isReal() ? new Complex(real_part, imag_part) : new Complex(
				real_part, minus(imag_part)));
	}

	public Number magnitude() {
		return isReal() ? real_part.abs() : NumberTower.plus(sqr(real_part),
				sqr(imag_part)).sqrt();
	}

	public int toInt() {
		if (isReal())
			return real_part.toInt();
		else
			throw new SchemeException(
					"toInt couldn't be applied to Complex number whose imaginary part is non-zero");
	}

	public boolean isInteger() {
		return (isReal() ? toReal().isInteger() : false);
	}

	public boolean isZero() {
		return (isReal() ? toReal().isZero() : false);
	}

	public boolean numericGreaterThan(Number other) {
		if (isReal()) {
			ArgumentChecker.checkAtomType(other, new PropertyChecker() {
				public boolean satisfied(Object n) {
					return ((org.plt.types.Number) n).isReal();
				}
			}, "real number", "numericGreaterThan");

			return NumberTower.greaterThan(real_part, other.toReal());
		}

		throw new SchemeException(
				"numericGreaterThan couldn't be applied to Complex number whose imaginary part is non-zero");
	}

	public boolean numericEqual(Number other) {
		if (isReal() && other.isReal())
			return NumberTower.equal(toReal(), other.toReal());
		if (isReal() || other.isReal())
			return false;

		return NumberTower.equal(real_part, ((Complex) other).getRealPart())
				&& NumberTower.equal(imag_part, ((Complex) other).getImgPart());
	}

	public boolean numericLessThan(Number other) {
		if (isReal()) {
			ArgumentChecker.checkAtomType(other, new PropertyChecker() {
				public boolean satisfied(Object n) {
					return ((org.plt.types.Number) n).isReal();
				}
			}, "real number", "numericLessThan");

			return NumberTower.lessThan(real_part, other.toReal());
		}

		throw new SchemeException(
				"numericLessThan couldn't be applied to Complex number whose imaginary part is non-zero");
	}

	public Number numericPlus(Number other) {
		if (isReal() && other.isReal())
			return NumberTower.plus(toReal(), other.toReal());
		if (isReal())
			return new Complex(NumberTower.plus(toReal(), ((Complex) other)
					.getRealPart()), ((Complex) other).getImgPart());
		if (other.isReal())
			return new Complex(NumberTower.plus(real_part, other.toReal()),
					imag_part);
		return new Complex(NumberTower.plus(real_part, ((Complex) other)
				.getRealPart()), NumberTower.plus(imag_part, ((Complex) other)
				.getImgPart()));
	}

	public Number numericMinus(Number other) {
		if (isReal() && other.isReal())
			return NumberTower.minus(toReal(), other.toReal());
		if (isReal())
			return new Complex(NumberTower.minus(toReal(),
					((Complex) other).real_part), ((Complex) other)
					.getImgPart());
		if (other.isReal())
			return new Complex(NumberTower.minus(real_part, other.toReal()),
					imag_part);

		return new Complex(NumberTower.minus(real_part,
				((Complex) other).real_part), NumberTower.minus(imag_part,
				((Complex) other).imag_part));
	}

	public Number numericMultiply(Number other) {
		if (isReal() && other.isReal())
			return NumberTower.multiply(toReal(), other.toReal());
		if (isReal())
			return new Complex(NumberTower.multiply(toReal(), ((Complex) other)
					.getRealPart()), NumberTower.multiply(toReal(),
					((Complex) other).getImgPart()));
		if (other.isReal())
			return new Complex(NumberTower.multiply(real_part, other.toReal()),
					NumberTower.multiply(imag_part, other.toReal()));

		Number real = NumberTower.minus(NumberTower.multiply(real_part,
				((Complex) other).real_part), NumberTower.multiply(imag_part,
				((Complex) other).getImgPart()));

		Number imag = NumberTower.plus(NumberTower.multiply(real_part,
				((Complex) other).getImgPart()), NumberTower.multiply(
				imag_part, ((Complex) other).real_part));

		return new Complex(real, imag);
	}

	public Number numericDivide(Number other) {
		if (isReal() && other.isReal())
			return NumberTower.divide(toReal(), other.toReal());
		if (isReal())
			return NumberTower.divide(NumberTower.multiply(toReal(),
					((Complex) other).conjugate()), sqr(((Complex) other)
					.magnitude()));
		if (other.isReal())
			return new Complex(NumberTower.divide(real_part, other.toReal()),
					NumberTower.divide(imag_part, other.toReal()));

		return NumberTower.divide(NumberTower.multiply(this, ((Complex) other)
				.conjugate()), sqr(((Complex) other).magnitude()));
	}

	public Number abs() {
		if (isReal())
			return toReal().abs();

		throw new SchemeException(
				"abs: expects argument of type <real number>; given " + this);
	}

	public Number ln() {
		if (isReal())
			return FloatPoint.fromNumber(toReal()).log();

		Number part1 = FloatPoint.fromNumber(this.magnitude()).log();
		Number part2 = multiplyByI(this.angle());
		return NumberTower.plus(part1, part2);
	}

	public Number acos() {
		if (isReal())
			return toReal().acos();

		Number pi_half = half(FloatPoint.PI);
		Number iz = multiplyByI(this);
		Number root = NumberTower.minus(FloatPoint.fromString("1"),
				NumberTower.multiply(this, this)).sqrt();
		Number l = multiplyByI(((Complex) NumberTower.plus(iz, root)).ln());

		return NumberTower.plus(pi_half, l);
	}

	public Number asin() {
		if (isReal())
			return toReal().asin();

		Number iz = multiplyByI(this);
		Number root = NumberTower.minus(FloatPoint.fromString("1"),
				NumberTower.multiply(this, this)).sqrt();

		return minus(multiplyByI(((Complex) NumberTower.plus(iz, root)).ln()));
	}

	public Number sqrt() {
		if (isReal())
			return toReal().sqrt();

		Number part1 = this.magnitude().sqrt();
		Number part2 = half(NumberTower.divide(imag_part, real_part).atan());
		Number part3 = NumberTower.plus(part2.cos(), multiplyByI(part2.sin()));

		return NumberTower.multiply(part1, part2);
	}

	public Number modulo(Number other) {
		if (isReal()) {
			ArgumentChecker.checkAtomType(toReal(), new PropertyChecker() {
				public boolean satisfied(Object n) {
					return ((org.plt.types.Number) n).isInteger();
				}
			}, "integer", "modulo", 1);

			ArgumentChecker.checkAtomType(other, new PropertyChecker() {
				public boolean satisfied(Object n) {
					return ((org.plt.types.Number) n).isInteger();
				}
			}, "integer", "modulo", 2);

			return toReal().modulo(other);
		}

		throw new SchemeException(
				"modulo: expects type <integer> as 1st argument, given: "
						+ this);
	}

	public Number floor() {
		if (isReal())
			return toReal().floor();

		throw new SchemeException(
				"floor: expects argument of type <real number>; given " + this);
	}

	public Number ceiling() {
		if (isReal())
			return toReal().ceiling();

		throw new SchemeException(
				"ceiling: expects argument of type <real number>; given "
						+ this);
	}

	public Number cos() {
		if (isReal())
			return toReal().cos();

		Number iz = multiplyByI(this);
		Number iz_minus = minus(iz);

		return half(NumberTower.plus(FloatPoint.exp(iz), FloatPoint
				.exp(iz_minus)));
	}

	public Number sin() {
		if (isReal())
			return toReal().sin();

		Number iz = multiplyByI(this);
		Number iz_minus = minus(iz);
		Number z2 = multiplyByI(FloatPoint.fromString("2"));

		return NumberTower.divide(NumberTower.minus(FloatPoint.exp(iz),
				FloatPoint.exp(iz_minus)), z2);
	}

	public Number atan() {
		if (isReal())
			return toReal().atan();

		Number iz = multiplyByI(this);
		Number part1 = new Complex(Rational.ONE, minus(iz)).ln();
		Number part2 = new Complex(Rational.ONE, iz).ln();

		return half(multiplyByI(NumberTower.minus(part1, part2)));
	}
}
