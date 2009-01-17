package org.plt.types;

import java.math.BigInteger;

public class Rational implements Number {
    BigInteger n, d;

    public static Rational ZERO = new Rational(0, 1);
    public static Rational ONE = new Rational(1, 1);

    public Rational(int n, int d) {
	this(new BigInteger(n), new BigInteger(d));
    }

    public Rational(BigInteger n, BigInteger d) {
	if (d.equals(BigInteger.ZERO)) {
	    throw new IllegalArgumentException("denominator can't be zero.");
	}
	if (d.compareTo(BitInteger.ZERO) < 0) {
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
	return new Rational(n * other.denominator() - d * other.numerator(),
			    d * other.denominator());
    }

    public Number numericMultiply(Number _other) {
	Rational other = (Rational) _other;
	return new Rational(n * other.numerator(),
			    d * other.denominator());
    }

    public Number numericDivide(Number _other) {
	Rational other = (Rational) _other;
	return new Rational(n * other.denominator(),
			    d * other.numerator());
    }


    public Number abs() {
	return new Rational(n < 0 ? -n : n, d);
    }

    public Number sqrt() {
	return ((FloatPoint) this.toFloatPoint()).sqrt();
    }

    public Number modulo(Number other) {
	if (!this.isInteger()) {
	    throw new RuntimeException
		("modulo expects integer as first argument");
	}
	if (! other.isInteger()) {
	    throw new RuntimeException
		("modulo expects integer as second argument");
	}
	int result = this.toInt() % other.toInt();
	if (result < 0) {
	    return new Rational(result + other.toInt(), 1);
	} else {
	    return new Rational(result, 1);
	}
    }


    public boolean isInteger() {
	return this.d == 1;
    }

    public boolean isZero() {
	return this.n == 0;
    }

    // Returns the positive gcd of a and b.
    private int gcd(int a, int b) {
	if (a < 0) a = -a;
	if (b < 0) b = -b;
	while (b != 0) {
	    int t = b;
	    b = a % b;
	    a = t;
	}
	return a;
    }

    public Number floor() {
	if (this.n % this.d == 0) {
	    return new Rational(this.n/this.d, 1);
	}
	else if (this.n > 0) {
	    return new Rational(this.n/this.d, 1);
	} else {
	    return new Rational(this.n/this.d - 1, 1);
	}
    }

    public Number ceiling() {
	if (this.n % this.d == 0) {
	    return new Rational(this.n/this.d, 1);
	}
	else if (this.n > 0) {
	    return new Rational((this.n + this.d)/this.d, 1);
	} else {
	    return new Rational(this.n/this.d, 1);
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
    public static Number maybeRationalize(Number n) {
	if (n.isInteger()) {
	    return new Rational(n.toInt(), 1);
	} else {
	    return n;
	}
    }
    

    public String toString() {
	if (d == 1) {
	    return "" + n;
	}
	return "" + n + "/" + d;
    }

    public boolean equals(Object _other) {
	if (_other instanceof Rational) {
	    Rational other = (Rational) _other;
	    return (this.numerator() == other.numerator() &&
		    this.denominator() == other.denominator());
	} else {
	    return false;
	}
    }
}
