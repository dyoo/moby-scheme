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
		return new Rational(-1, 1).numericMultiply(n);
	}

	private static Number half(Number n) {
		return new Rational(1, 2).numericMultiply(n);
	}

	private static Number multiplyByI(Number n) {
		return new Complex(Rational.ZERO, Rational.ONE).numericMultiply(n);
	}

	private static Number sqr(Number n) {
		return n.numericMultiply(n);
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
			if (toReal().numericGreaterThan(Rational.ZERO))
				return Rational.ZERO;
			else
				return FloatPoint.PI;
		} else {
			if (real_part.isZero()) {
				if (imag_part.numericGreaterThan(Rational.ZERO))
					return half(FloatPoint.PI);
				else
					return minus(half(FloatPoint.PI));
			} else {
				Number tmp = imag_part.numericDivide(real_part.abs()).atan();
				if (real_part.numericGreaterThan(Rational.ZERO))
					return (imag_part.numericGreaterThan(Rational.ZERO) ? tmp
							: minus(tmp));
				else
					return (imag_part.numericGreaterThan(Rational.ZERO) ? FloatPoint.PI
							.numericMinus(tmp)
							: minus(FloatPoint.PI.numericMinus(tmp)));
			}
		}
	}

	public Number conjugate() {
		return (isReal() ? new Complex(real_part, imag_part) : new Complex(
				real_part, minus(imag_part)));
	}

	public Number magnitude() {
		return (isReal() ? real_part.abs() : sqr(real_part).numericPlus(
				sqr(imag_part)).sqrt());
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

			return real_part.numericGreaterThan(other.toReal());
		}

		throw new SchemeException(
				"numericGreaterThan couldn't be applied to Complex number whose imaginary part is non-zero");
	}

	public boolean numericEqual(Number other) {
		if (isReal() && other.isReal())
			return toReal().numericEqual(other.toReal());
		if (isReal() || other.isReal())
			return false;

		return (real_part.numericEqual(((Complex) other).getRealPart()) && imag_part
				.numericEqual(((Complex) other).getImgPart()));
	}

	public boolean numericLessThan(Number other) {
		if (isReal()) {
			ArgumentChecker.checkAtomType(other, new PropertyChecker() {
				public boolean satisfied(Object n) {
					return ((org.plt.types.Number) n).isReal();
				}
			}, "real number", "numericLessThan");

			return real_part.numericLessThan(other.toReal());
		}

		throw new SchemeException(
				"numericLessThan couldn't be applied to Complex number whose imaginary part is non-zero");
	}

	public Number numericPlus(Number other) {
		if (isReal() && other.isReal())
			return toReal().numericPlus(other.toReal());
		if (isReal())
			return new Complex(toReal().numericPlus(
					((Complex) other).getRealPart()), ((Complex) other)
					.getImgPart());
		if (other.isReal())
			return new Complex(real_part.numericPlus(other.toReal()), imag_part);

		return new Complex(real_part.numericPlus(((Complex) other)
				.getRealPart()), imag_part.numericPlus(((Complex) other)
				.getImgPart()));
	}

	public Number numericMinus(Number other) {
		if (isReal() && other.isReal())
			return toReal().numericMinus(other.toReal());
		if (isReal())
			return new Complex(toReal().numericMinus(
					((Complex) other).real_part), ((Complex) other)
					.getImgPart());
		if (other.isReal())
			return new Complex(real_part.numericMinus(other.toReal()),
					imag_part);

		return new Complex(real_part.numericMinus(((Complex) other).real_part),
				imag_part.numericMinus(((Complex) other).getImgPart()));
	}

	public Number numericMultiply(Number other) {
		if (isReal() && other.isReal())
			return toReal().numericMultiply(other.toReal());
		if (isReal())
			return new Complex(toReal().numericMultiply(
					((Complex) other).getRealPart()), toReal().numericMultiply(
					((Complex) other).getImgPart()));
		if (other.isReal())
			return new Complex(real_part.numericMultiply(other.toReal()),
					imag_part.numericMultiply(other.toReal()));

		Number real = real_part.numericMultiply(((Complex) other).real_part)
				.numericMinus(
						imag_part.numericMultiply(((Complex) other)
								.getImgPart()));
		Number imag = real_part.numericMultiply(((Complex) other).getImgPart())
				.numericPlus(
						imag_part.numericMultiply(((Complex) other).real_part));
		return new Complex(real, imag);
	}

	public Number numericDivide(Number other) {
		if (isReal() && other.isReal())
			return toReal().numericDivide(other.toReal());
		if (isReal())
			return toReal().numericMultiply(((Complex) other).conjugate())
					.numericDivide(sqr(((Complex) other).magnitude()));
		if (other.isReal())
			return new Complex(real_part.numericDivide(other.toReal()),
					imag_part.numericDivide(other.toReal()));

		return this.numericMultiply(((Complex) other).conjugate())
				.numericDivide(sqr(((Complex) other).magnitude()));
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
		return part1.numericPlus(part2);
	}

	public Number acos() {
		if (isReal())
			return toReal().acos();

		Number pi_half = half(FloatPoint.PI);
		Number iz = multiplyByI(this);
		Number root = FloatPoint.fromString("1").numericMinus(
				this.numericMultiply(this)).sqrt();
		Number l = multiplyByI(((Complex) (iz.numericPlus(root))).ln());

		return pi_half.numericPlus(l);
	}

	public Number asin() {
		if (isReal())
			return toReal().asin();

		Number iz = multiplyByI(this);
		Number root = FloatPoint.fromString("1").numericMinus(
				this.numericMultiply(this)).sqrt();

		return minus(multiplyByI(((Complex) (iz.numericPlus(root))).ln()));
	}

	public Number sqrt() {
		if (isReal())
			return toReal().sqrt();

		Number part1 = this.magnitude().sqrt();
		Number part2 = half(imag_part.numericDivide(real_part).atan());
		Number part3 = part2.cos().numericPlus(multiplyByI(part2.sin()));

		return part1.numericMultiply(part2);
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

		return half(FloatPoint.exp(iz).numericPlus(FloatPoint.exp(iz_minus)));
	}

	public Number sin() {
		if (isReal())
			return toReal().sin();

		Number iz = multiplyByI(this);
		Number iz_minus = minus(iz);
		Number z2 = multiplyByI(FloatPoint.fromString("2"));

		return FloatPoint.exp(iz).numericMinus(FloatPoint.exp(iz_minus))
				.numericDivide(z2);
	}

	public Number atan() {
		if (isReal())
			return toReal().atan();

		Number iz = multiplyByI(this);
		Number part1 = new Complex(Rational.ONE, minus(iz)).ln();
		Number part2 = new Complex(Rational.ONE, iz).ln();

		return half(multiplyByI(part1.numericMinus(part2)));
	}
}
