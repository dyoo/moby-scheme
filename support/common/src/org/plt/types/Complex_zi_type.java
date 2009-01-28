package org.plt.types;

import org.plt.*;
import org.plt.checker.*;

/**
 * Complex_zi_type: complex whose imaginary part is zero
 * 
 * @author zhezhang
 * 
 */

public class Complex_zi_type implements Complex {

	// either Rational or FloatPoint
	private Number real_part;

	public static Number ZERO = FloatPoint.fromString("0");
	public static Number ONE = FloatPoint.fromString("1");
	public static Number TWO = FloatPoint.fromString("2");

	public Complex_zi_type(Number real) {
		ArgumentChecker.checkAtomType(real, new PropertyChecker() {
			public boolean satisfied(Object n) {
				return (n instanceof Rational || n instanceof FloatPoint);
			}
		}, "real number", "Complex_zi_type");
	}

	public Number angle() {
		if (real_part.isZero())
			throw new SchemeException("angle: undefined for 0");

		if (real_part.numericGreaterThan(ZERO))
			return ZERO;
		else
			return FloatPoint.PI;
	}

	public Number conjugate() {
		return real_part;
	}

	public Number magnitude() {
		return real_part.abs();
	}

	public int toInt() {
		return real_part.toInt();
	}

	public boolean isInteger() {
		return real_part.isInteger();
	}

	public boolean isZero() {
		return real_part.isZero();
	}

	public boolean numericGreaterThan(Number other) {
		ArgumentChecker.checkAtomType(other, new PropertyChecker() {
			public boolean satisfied(Object n) {
				return (n instanceof FloatPoint || n instanceof Rational);
			}
		}, "real number", "numericGreaterThan");

		return real_part.numericGreaterThan(other);
	}

	public boolean numericEqual(Number other) {
		ArgumentChecker.checkAtomType(other, new PropertyChecker() {
			public boolean satisfied(Object n) {
				return (n instanceof FloatPoint || n instanceof Rational);
			}
		}, "real number", "numericEqual");

		return real_part.numericEqual(other);
	}

	public boolean numericLessThan(Number other) {
		ArgumentChecker.checkAtomType(other, new PropertyChecker() {
			public boolean satisfied(Object n) {
				return (n instanceof FloatPoint || n instanceof Rational);
			}
		}, "real number", "numericLessThan");

		return real_part.numericLessThan(other);
	}

	public Number numericPlus(Number other) {
		ArgumentChecker.checkAtomType(other, new PropertyChecker() {
			public boolean satisfied(Object n) {
				return (n instanceof FloatPoint || n instanceof Rational);
			}
		}, "real number", "numericPlus");

		return real_part.numericPlus(other);
	}

	public Number numericMinus(Number other) {
		ArgumentChecker.checkAtomType(other, new PropertyChecker() {
			public boolean satisfied(Object n) {
				return (n instanceof FloatPoint || n instanceof Rational);
			}
		}, "real number", "numericMinus");

		return real_part.numericMinus(other);
	}

	public Number numericMultiply(Number other) {
		ArgumentChecker.checkAtomType(other, new PropertyChecker() {
			public boolean satisfied(Object n) {
				return (n instanceof FloatPoint || n instanceof Rational);
			}
		}, "real number", "numericMultiply");

		return real_part.numericMultiply(other);
	}

	public Number numericDivide(Number other) {
		ArgumentChecker.checkAtomType(other, new PropertyChecker() {
			public boolean satisfied(Object n) {
				return (n instanceof FloatPoint || n instanceof Rational);
			}
		}, "real number", "numericDivide");

		return real_part.numericDivide(other);
	}

	public Number abs() {
		return real_part.abs();
	}

	public Number acos() {
		return real_part.acos();
	}

	public Number asin() {
		return real_part.asin();
	}

	public Number sqrt() {
		return real_part.sqrt();
	}

	public Number modulo(Number other) {
		ArgumentChecker.checkAtomType(real_part, new PropertyChecker() {
			public boolean satisfied(Object n) {
				return ((org.plt.types.Number) n).isInteger();
			}
		}, "integer", "modulo", 1);

		ArgumentChecker.checkAtomType(other, new PropertyChecker() {
			public boolean satisfied(Object n) {
				return ((org.plt.types.Number) n).isInteger();
			}
		}, "integer", "modulo", 2);

		return real_part.modulo(other);
	}

	public Number floor() {
		return real_part.floor();
	}

	public Number ceiling() {
		return real_part.ceiling();
	}

	public Number cos() {
		return real_part.cos();
	}

	public Number sin() {
		return real_part.sin();
	}

	public Number atan() {
		return real_part.atan();
	}
}
