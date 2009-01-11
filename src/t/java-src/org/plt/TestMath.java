package org.plt;
import org.plt.types.*;
import org.junit.Test;
import static org.junit.Assert.*;

public class TestMath {
    @Test public void testAdd() {
	Rational half = new Rational(1, 2);
	assertEquals(new Rational(1, 1),
		     half.numericPlus(half));
    }

    @Test public void testSymbol() {
	assertEquals(Symbol.makeInstance("x"),
		     Symbol.makeInstance("x"));
    }

    @Test public void testModulo() {
	// In Scheme, modulo is positive regardless of the dividend.
	assertEquals(new Rational(1, 1),
		     new Rational(-5, 1).modulo(new Rational(2, 1)));
    }


    @Test public void testCeiling() {
	// ceiling(1/2) ==> 1
	assertEquals(new Rational(1, 1),
		     new Rational(1, 2).ceiling());

	// ceiling(1)  ==> 1
	assertEquals(new Rational(1, 1),
		     new Rational(1, 1).ceiling());

	// ceiling(-1/2) ==> 0
	assertEquals(new Rational(0, 1),
		     new Rational(-1, 2).ceiling());
	
	// ceiling(-1) ==> -1
	assertEquals(new Rational(-1, 1),
		     new Rational(-1, 1).ceiling());
	
	// ceiling(5/3) ==> 2
	assertEquals(new Rational(2, 1),
		     new Rational(5, 3).ceiling());

    }


    @Test public void testFloor() {
	// floor(1/2) ==> 0
	assertEquals(new Rational(0, 1),
		     new Rational(1, 2).floor());

	// floor(1)  ==> 1
	assertEquals(new Rational(1, 1),
		     new Rational(1, 1).floor());

	// floor(-1/2) ==> -1
	assertEquals(new Rational(-1, 1),
		     new Rational(-1, 2).floor());
	
	// floor(-1) ==> -1
	assertEquals(new Rational(-1, 1),
		     new Rational(-1, 1).floor());

	// floor(5/3) ==> 1
	assertEquals(Rational.ONE,
		     new Rational(5, 3).floor());
    }


    @Test public void testAbs() {
	assertEquals(new Rational(5, 4),
		     new Rational(5, 4).abs());

	assertEquals(new Rational(5, 4),
		     new Rational(-5, 4).abs());


	assertEquals(FloatPoint.fromString("3.2"),
		     FloatPoint.fromString("3.2").abs());

	assertEquals(FloatPoint.fromString("3.2"),
		     FloatPoint.fromString("-3.2").abs());
    }


    @Test public void testAcos() {
	assertEquals(new Rational(0, 1),
		     Rational.ONE.acos());
    }


    @Test public void testAsin() {
	assertEquals(new Rational(0, 1),
		     Rational.ZERO.asin());
    }

    @Test public void testAtan() {
	assertEquals(new Rational(0, 1),
		     Rational.ZERO.atan());
    }

}
