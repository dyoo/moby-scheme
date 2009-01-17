package org.plt.types;

import java.math.BigInteger;

public class Rational implements Number {
    BigInteger n, d;

    public static Rational ZERO = new Rational(0, 1);
    public static Rational ONE = new Rational(1, 1);

    public Rational(int n, int d) {
	this(BigInteger.valueOf(n), BigInteger.valueOf(d));
    }

    public Rational(BigInteger n, BigInteger d) {
	if (d.equals(BigInteger.ZERO)) {
	    throw new IllegalArgumentException("denominator can't be zero.");
	}
	if (d.compareTo(BigInteger.ZERO) < 0) {
	    n = n.negate();
	    d = d.negate();
	}

	BigInteger divisor = n.gcd(d);
	this.n = n.divide(divisor);
	this.d = d.divide(divisor);
    }

    public BigInteger numerator() {
	return this.n;
    }

    public BigInteger denominator() {
	return this.d;
    }
    
    public int toInt() {
	return (this.n.divide(this.d)).intValue();
    }

    public FloatPoint toFloatPoint() {
	// FIXME: if we overflow here, we have to do something smarter!
	return (FloatPoint) FloatPoint.fromInt(n.intValue()).numericDivide(FloatPoint.fromInt(d.intValue()));
    }

    public boolean numericGreaterThan(Number _other) {
	Rational other = (Rational) _other;
	return ((n.multiply(other.denominator()))
		.compareTo(d.multiply(other.numerator()))
		> 0);
    }

    public boolean numericLessThan(Number _other) {
	Rational other = (Rational) _other;
	return ((n.multiply(other.denominator()))
		.compareTo(d.multiply(other.numerator())) 
		< 0);
    }

    public boolean numericEqual(Number _other) {
	Rational other = (Rational) _other;
	return ((n.multiply(other.denominator()))
		.compareTo(d.multiply(other.numerator())) 
		== 0);
    }

    public Number numericPlus(Number _other) {
	Rational other = (Rational) _other;
	BigInteger newNumerator =
	    n.multiply(other.denominator()).add(d.multiply(other.numerator()));
	BigInteger newDenominator = d.multiply(other.denominator());
	return new Rational(newNumerator, newDenominator);
    }

    public Number numericMinus(Number _other) {
	Rational other = (Rational) _other;
	BigInteger newNumerator =
	    n.multiply(other.denominator()).subtract(d.multiply(other.numerator()));
	BigInteger newDenominator = d.multiply(other.denominator());


	return new Rational(newNumerator, newDenominator);
    }


    public Number numericMultiply(Number _other) {
	Rational other = (Rational) _other;
	return new Rational(n.multiply(other.numerator()),
			    d.multiply(other.denominator()));
    }

    public Number numericDivide(Number _other) {
	Rational other = (Rational) _other;
	return new Rational(n.multiply(other.denominator()),
			    d.multiply(other.numerator()));
    }


    public Number abs() {
	return new Rational(n.abs(), d.abs());
    }


    public Number sqrt() {
	return ((FloatPoint) this.toFloatPoint()).sqrt();
    }


    public Number modulo(Number other) {
	if (!this.isInteger()) {
	    throw new RuntimeException
		("modulo expects integer as first argument");
	}
	if (!(other.isInteger()) || !(other instanceof Rational)) {
	    throw new RuntimeException
		("modulo expects integer as second argument");
	}
        BigInteger result = this.n.mod(this.d);
	if (result.compareTo(BigInteger.ZERO) < 0) {
	    return new Rational(result.add(((Rational)other).numerator()),
				BigInteger.ONE);
	} else {
	    return new Rational(result, BigInteger.ONE);
	}
    }


    public boolean isInteger() {
	return this.d.equals(BigInteger.ONE);
    }


    public boolean isZero() {
	return this.n.equals(BigInteger.ZERO);
    }


    public Number floor() {
	if (this.n.mod(this.d).equals(BigInteger.ZERO)) {
	    return new Rational(this.n.divide(this.d), BigInteger.ONE);
	}
	else if (this.n.compareTo(BigInteger.ZERO) > 0) {
	    return new Rational(this.n.divide(this.d), BigInteger.ONE);
	} else {
	    return new Rational(this.n.divide(this.d).subtract(BigInteger.ONE),
				BigInteger.ONE);
	}
    }

    public Number ceiling() {
	if (this.n.mod(this.d).equals(BigInteger.ZERO)) {
	    return new Rational(this.n.divide(this.d), BigInteger.ONE);
	}
	else if (this.n.compareTo(BigInteger.ZERO) > 0) {
	    return new Rational((this.n.add(this.d)).divide(this.d), 
				BigInteger.ONE);
	} else {
	    return new Rational(this.n.divide(this.d), BigInteger.ONE);
	}
    }



    public Number acos() {
	return maybeRationalize(this.toFloatPoint().acos());
    }

    public Number sin() {
	return maybeRationalize(this.toFloatPoint().sin());
    }

    public Number asin() {
	return maybeRationalize(this.toFloatPoint().asin());
    }

    public Number atan() {
	return maybeRationalize(this.toFloatPoint().atan());
    }


    public Number cos() {
	return maybeRationalize(this.toFloatPoint().cos());
    }


    // maybeRationalize will either return a rationalzed version of n
    // if we don't lose information, or otherwise just return the input as is.
    // Fixme: we should think about overflow.
    public static Number maybeRationalize(Number n) {
	if (n.isInteger()) {
	    return new Rational(n.toInt(), 1);
	} else {
	    return n;
	}
    }
    

    public String toString() {
	if (d.equals(BigInteger.ONE)) {
	    return "" + n;
	}
	return "" + n + "/" + d;
    }

    public boolean equals(Object _other) {
	if (_other instanceof Rational) {
	    Rational other = (Rational) _other;
	    return (this.numerator().equals(other.numerator()) &&
		    this.denominator().equals(other.denominator()));
	} else {
	    return false;
	}
    }
}
