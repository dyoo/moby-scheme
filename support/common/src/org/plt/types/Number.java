package org.plt.types;

public interface Number {
	int toInt();

	boolean isReal();

	Number toReal();

	boolean isInteger();

	boolean isZero();

	boolean numericGreaterThan(Number other);

	boolean numericEqual(Number other);

	boolean numericLessThan(Number other);

	Number numericPlus(Number other);

	Number numericMinus(Number other);

	Number numericMultiply(Number other);

	Number numericDivide(Number other);

	Number abs();

	Number acos();

	Number asin();

	Number sqrt();

	Number modulo(Number other);

	Number floor();

	Number ceiling();

	Number cos();

	Number sin();

	Number atan();
}
