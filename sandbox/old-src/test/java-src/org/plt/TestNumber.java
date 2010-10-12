package org.plt;

import org.plt.Kernel;
import org.plt.types.*;
import org.junit.Test;
import static org.junit.Assert.*;

public class TestNumber {
    @Test public void testNumberToString() {
	assertEquals(new Rational(10, 1).toString(), "10");
	assertEquals(FloatPoint.fromString("3.14").ceiling().toString(), "4");
	assertEquals(FloatPoint.fromString("2.718").floor().toString(), "2");
    }


    @Test public void testMultipication() {
	assertEquals(new Rational(1, 1).numericMultiply(new Rational(1, 1)),
		     new Rational(1, 1));
	assertEquals(new Rational(-1, 1).numericMultiply(new Rational(-1, 1)),
		     new Rational(1, 1));
	assertEquals(new Rational(1, 1).numericMultiply(new Rational(-1, 1)),
		     new Rational(-1, 1));
	assertEquals(new Rational(-1, 1).numericMultiply(new Rational(1, 1)),
		     new Rational(-1, 1));

	assertEquals(new Rational(2, 1).numericMultiply(new Rational(20, 1)),
		     new Rational(40, 1));


    }


}
