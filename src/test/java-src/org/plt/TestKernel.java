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
		assertTrue(Kernel._equal_(Kernel.tan(Kernel.ZERO), Kernel.ZERO)
				.isTrue());
	}

	@Test
	public void testSinh() {
		assertTrue(Kernel._equal_(Kernel.sinh(Kernel.ZERO), Kernel.ZERO)
				.isTrue());
	}

	@Test
	public void testCosh() {
		assertTrue(Kernel._equal_(Kernel.cosh(Kernel.ZERO), Kernel.ONE)
				.isTrue());
	}

	@Test
	public void testEven_question_() {
		assertTrue(Kernel.even_question_(Kernel.ZERO).isTrue());
	}

	@Test
	public void testEvenHuhTerminatesOnBadInput() {
		try {
			Kernel.even_question_(new Rational(1, 2));
			fail();
		} catch (SchemeException e) {
		}
	}

	public void testEvenHuhTerminatesOnNegativeNumbers() {
		try {
			assertFalse(Kernel.even_question_(new Rational(-5, 1)).isTrue());
		} catch (SchemeException e) {
		}
	}

	@Test
	public void testOdd_question_() {
		assertTrue(Kernel.odd_question_(Kernel.ONE).isTrue());
	}

	@Test
	public void testExpt() {
		assertTrue(Kernel._equal_(Kernel.expt(Kernel.TWO, Kernel.ONE),
				Kernel.TWO).isTrue());
	}

	@Test
	public void testExp() {
		assertTrue(Kernel._equal_(Kernel.exp(Kernel.ZERO), Kernel.ONE).isTrue());
	}

	@Test
	public void testLog() {
		assertTrue(Kernel
				._equal_(Kernel.log(Kernel.ONE), FloatPoint.fromInt(0))
				.isTrue());
	}

	@Test
	public void testPositive_question_() {
		assertTrue(Kernel.positive_question_(Kernel.ONE).isTrue());
	}

	@Test
	public void testNegative_question_() {
		assertTrue(Kernel.negative_question_(Kernel.ONE).isFalse());
	}

	@Test
	public void testSgn() {
		assertTrue(Kernel._equal_(Kernel.sgn(Kernel.ONE), Kernel.ONE).isTrue());
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
				Kernel.ONE).isTrue());
	}

	@Test
	public void testLCM() {
		assertTrue(Kernel._equal_(Kernel.lcm(Kernel.TWO, Kernel.THREE),
				Kernel.SIX).isTrue());
	}

	@Test
	public void testPair_question_() {
		assertTrue(Kernel.pair_question_(Kernel.ONE).isFalse());
	}

	@Test
	public void testCons_question_() {
		assertTrue(Kernel.cons_question_(Kernel.ONE).isFalse());
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
		assertTrue(Kernel._equal_(Kernel.quotient(Kernel.SIX, Kernel.FOUR),
				Kernel.ONE).isTrue());
	}

	@Test
	public void testRemainder() {
		assertTrue(Kernel._equal_(Kernel.remainder(Kernel.SIX, Kernel.FOUR),
				Kernel.TWO).isTrue());
	}

	@Test
	public void testNumerator() {
		assertTrue(Kernel._equal_(Kernel.numerator(new Rational(12, 10)),
				Kernel.SIX).isTrue());
	}

	@Test
	public void testDenominator() {
		assertTrue(Kernel._equal_(Kernel.denominator(new Rational(12, 10)),
				Kernel.FIVE).isTrue());
	}

	@Test
	public void testInteger_question_() {
		assertTrue(Kernel.integer_question_(new Rational(12, 10)).isFalse());
	}

	@Test
	public void testNull_question_() {
		assertTrue(Kernel.null_question_(Empty.EMPTY).isTrue());
	}

	@Test
	public void testLength() {
		assertTrue(Kernel._equal_(
				Kernel.length(Kernel.cons(Kernel.THREE, Empty.EMPTY)),
				Kernel.ONE).isTrue());
	}

	@Test
	public void testList_ref_() {
		assertTrue(Kernel._equal_(
				Kernel.list_dash_ref(Kernel.cons(Kernel.THREE, Empty.EMPTY),
						Kernel.ZERO), Kernel.THREE).isTrue());
	}

	@Test
	public void testRound() {
		assertTrue(Kernel._equal_(Kernel.round(FloatPoint.fromString("3.1")),
				FloatPoint.fromString("3")).isTrue());
		assertTrue(Kernel._equal_(Kernel.round(FloatPoint.fromString("3.5")),
				FloatPoint.fromString("4")).isTrue());
		assertTrue(Kernel._equal_(Kernel.round(FloatPoint.fromString("-3.5")),
				FloatPoint.fromString("-4")).isTrue());
		assertTrue(Kernel._equal_(Kernel.round(FloatPoint.fromString("-3.1")),
				FloatPoint.fromString("-3")).isTrue());
	}

	@Test
	public void testReal_question_() {
		assertTrue(Kernel.real_question_(FloatPoint.fromString("3.1")).isTrue());
		assertTrue(Kernel.real_question_(Kernel.ONE).isTrue());
		assertTrue(Kernel.real_question_(Empty.EMPTY).isFalse());
	}

	@Test
	public void testString_greaterthan__question_() {
		assertTrue(Kernel.string_greaterthan__question_("b", "a").isTrue());
		assertTrue(Kernel.string_greaterthan__question_("a", "a").isFalse());
		assertTrue(Kernel.string_greaterthan__question_("a", "b").isFalse());
	}

	@Test
	public void testString_greaterthan__equal__question_() {
		assertTrue(Kernel.string_greaterthan__equal__question_("b", "a")
				.isTrue());
		assertTrue(Kernel.string_greaterthan__equal__question_("a", "a")
				.isTrue());
		assertTrue(Kernel.string_greaterthan__equal__question_("a", "b")
				.isFalse());
	}

	@Test
	public void testString_lessthan__question_() {
		assertTrue(Kernel.string_lessthan__question_("b", "a").isFalse());
		assertTrue(Kernel.string_lessthan__question_("a", "a").isFalse());
		assertTrue(Kernel.string_lessthan__question_("a", "b").isTrue());
	}

	@Test
	public void testString_lessthan__equal__question_() {
		assertTrue(Kernel.string_lessthan__equal__question_("b", "a").isFalse());
		assertTrue(Kernel.string_lessthan__equal__question_("a", "a").isTrue());
		assertTrue(Kernel.string_lessthan__equal__question_("a", "b").isTrue());
	}

	@Test
	public void testSubstring() {
		assertTrue(Kernel.string_equal__question_(
				Kernel.substring("world", Kernel.ZERO, Kernel.TWO), "wo")
				.isTrue());
	}

	// @Test
	// public void testString_dash_ref() {
	// assertTrue(Kernel.char_equal__question_('w',
	// Kernel.string_dash_ref("world", Kernel.ZERO)).isTrue());
	// }

	@Test
	public void testString_dash_length() {
		assertTrue(Kernel._equal_(Kernel.FIVE,
				Kernel.string_dash_length("world")).isTrue());
	}

	@Test
	public void testString_dash_copy() {
		assertTrue(Kernel.string_equal__question_(
				Kernel.string_dash_copy("hi"), "hi").isTrue());
	}

	@Test
	public void testString_dash__greaterthan_number() {
		assertTrue(Kernel._equal_(Kernel.string_dash__greaterthan_number("3"),
				Kernel.THREE).isTrue());
		assertTrue(Kernel._equal_(
				Kernel.string_dash__greaterthan_number("3.1"),
				FloatPoint.fromString("3.1")).isTrue());
		assertTrue(((Logic)(Kernel.string_dash__greaterthan_number("3.1a"))).isFalse());
	}
}