package org.plt;

import org.plt.Kernel;
import org.plt.types.*;
import org.junit.Test;
import static org.junit.Assert.*;

public class TestKernel {

	@Test
	public void testEmpty() {
		assertTrue(Kernel.empty_question_(Empty.EMPTY).isTrue());
	}

	// Fill me in with more test cases!
	@Test
	public void testTan() {
		assertTrue(Kernel._equal_(Kernel.tan(Rational.ZERO), Rational.ZERO)
				.isTrue());
	}

	@Test
	public void testSinh() {
		assertTrue(Kernel._equal_(Kernel.sinh(Rational.ZERO), Rational.ZERO)
				.isTrue());
	}

	@Test
	public void testCosh() {
		assertTrue(Kernel._equal_(Kernel.cosh(Rational.ZERO), Rational.ONE)
				.isTrue());
	}

	@Test
	public void testEven_question_() {
		assertTrue(Kernel.even_question_(Rational.ZERO).isTrue());
	}

	@Test
	public void testOdd_question_() {
		assertTrue(Kernel.odd_question_(Rational.ONE).isTrue());
	}

	@Test
	public void testExpt() {
		assertTrue(Kernel._equal_(Kernel.expt(Kernel.TWO, Rational.ONE),
				Kernel.TWO).isTrue());
	}

	@Test
	public void testExp() {
		assertTrue(Kernel._equal_(Kernel.exp(Rational.ZERO), Rational.ONE)
				.isTrue());
	}

	@Test
	public void testLog() {
		assertTrue(Kernel._equal_(Kernel.log(FloatPoint.fromInt(1)),
				FloatPoint.fromInt(0)).isTrue());
	}

	@Test
	public void testPositive_question_() {
		assertTrue(Kernel.positive_question_(Rational.ONE).isTrue());
	}

	@Test
	public void testNegative_question_() {
		assertTrue(Kernel.negative_question_(Rational.ONE).isFalse());
	}

	@Test
	public void testSgn() {
		assertTrue(Kernel._equal_(Kernel.sgn(Rational.ONE), Rational.ONE)
				.isTrue());
	}

	@Test
	public void testBoolean_question_() {
		assertTrue(Kernel.boolean_question_(Logic.TRUE).isTrue());
	}

	@Test
	public void testFalse_question_() {
		assertTrue(Kernel.false_question_(Logic.FALSE).isTrue());
	}

	@Test
	public void testBoolean_equal__question_() {
		assertTrue(Kernel.boolean_equal__question_(Logic.FALSE, Logic.FALSE)
				.isTrue());
	}

	@Test
	public void testSymbol_question_() {
		assertTrue(Kernel.symbol_question_(1).isFalse());
	}

	@Test
	public void testGCD() {
		assertTrue(Kernel._equal_(Kernel.gcd(Kernel.TWO, Kernel.THREE),
				Rational.ONE).isTrue());
	}

	@Test
	public void testLCM() {
		assertTrue(Kernel._equal_(Kernel.lcm(Kernel.TWO, Kernel.THREE),
				Kernel.SIX).isTrue());
	}

	@Test
	public void testPair_question_() {
		assertTrue(Kernel.pair_question_(Rational.ONE).isFalse());
	}

	@Test
	public void testNumber_question_() {
		assertTrue(Kernel.number_question_(Kernel.TWO).isTrue());
	}

	@Test
	public void testRational_question_() {
		assertTrue(Kernel.rational_question_(Kernel.TWO).isTrue());
	}

	@Test
	public void testQuotient() {
		assertTrue(Kernel._equal_(Kernel.quotient(Kernel.SIX, Kernel.TWO),
				Kernel.THREE).isTrue());
	}
}