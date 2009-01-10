package plt.types;

// Kludge: I really should take advantage of the tower structure
// rather than hardcode the if/then statements...

public class NumberTower {
    // Coerses the left value to be the type of the right hand
    // side, or returns null if the coersion can't be made.
    public static Number coerseLeft(Number left, Number right) {
	if (left instanceof Rational) {
	    if (right instanceof Rational) {
		return left;
	    } else if (right instanceof FloatPoint) {
		return ((Rational)left).toFloatPoint();
	    } else {
		return null;
	    }
	} else if (left instanceof FloatPoint) {
	    if (right instanceof Rational) {
		if (((FloatPoint)left).isInteger()) {
		    return new Rational(left.toInt(), 1);
		}
		return null;
	    } else if (right instanceof FloatPoint) {
		return left;
	    } else {
		return null;
	    }
	} else {
	    return null;
	}
    }

    public static boolean lessThan(Number n1, Number n2) {
	if (coerseLeft(n1, n2) != null) {
	    n1 = coerseLeft(n1, n2);
	}
	if (coerseLeft(n2, n2) != null) {
	    n2 = coerseLeft(n2, n1);
	}
	return n1.numericLessThan(n2);
    }

    public static boolean greaterThan(Number n1, Number n2) {
	if (coerseLeft(n1, n2) != null) {
	    n1 = coerseLeft(n1, n2);
	}
	if (coerseLeft(n2, n2) != null) {
	    n2 = coerseLeft(n2, n1);
	}
	return n1.numericGreaterThan(n2);
    }


    public static boolean greaterThanEqual(Number n1, Number n2) {
	return greaterThan(n1, n2) || equal(n1, n2);
    }


    public static boolean lessThanEqual(Number n1, Number n2) {
	return lessThan(n1, n2) || equal(n1, n2);
    }


    public static boolean equal(Number n1, Number n2) {
	if (coerseLeft(n1, n2) != null) {
	    n1 = coerseLeft(n1, n2);
	}
	if (coerseLeft(n2, n2) != null) {
	    n2 = coerseLeft(n2, n1);
	}
	return n1.numericEqual(n2);
    }


    public static boolean approxEqual(Number n1, Number n2, Number n3) {
	if (lessThan(n3, Rational.ZERO)) {
	    throw new IllegalArgumentException
		("third argument to =~ must be non-negative");
	}
	return lessThanEqual(minus(n1, n2).abs(),
			     n3);
    }

    public static Number plus(Number n1, Number n2) {
	if (coerseLeft(n1, n2) != null) {
	    n1 = coerseLeft(n1, n2);
	}
	if (coerseLeft(n2, n2) != null) {
	    n2 = coerseLeft(n2, n1);
	}
	return n1.numericPlus(n2);
    }

    public static Number minus(Number n1, Number n2) {
	if (coerseLeft(n1, n2) != null) {
	    n1 = coerseLeft(n1, n2);
	}
	if (coerseLeft(n2, n2) != null) {
	    n2 = coerseLeft(n2, n1);
	}
	return n1.numericMinus(n2);
    }

    public static Number multiply(Number n1, Number n2) {
	if (coerseLeft(n1, n2) != null) {
	    n1 = coerseLeft(n1, n2);
	}
	if (coerseLeft(n2, n2) != null) {
	    n2 = coerseLeft(n2, n1);
	}
	return n1.numericMultiply(n2);
    }

    public static Number divide(Number n1, Number n2) {
	if (coerseLeft(n1, n2) != null) {
	    n1 = coerseLeft(n1, n2);
	}
	if (coerseLeft(n2, n2) != null) {
	    n2 = coerseLeft(n2, n1);
	}
	return n1.numericDivide(n2);
    }


    
}
