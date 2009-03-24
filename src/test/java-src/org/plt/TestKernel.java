package org.plt;

import org.plt.Kernel;
import org.plt.types.*;
import org.junit.Test;
import static org.junit.Assert.*;
import org.plt.checker.*;
import org.plt.lib.Parser;

public class TestKernel {

    @Test public void testSplitWhitespace() {
	assertEquals(Kernel.list(new Object[] { "42.272816", "-71.808205"}),
		     Parser.splitWhitespace("\n      42.272816 -71.808205\n    "));
    }

	@Test
	public void testEmpty() {
		assertTrue(Kernel.empty_question_(Empty.EMPTY).isTrue());
		assertTrue(Empty.EMPTY.equals(Empty.EMPTY));
	}

    @Test
	public void testEqualityOfLists() {
	assertEquals(Empty.EMPTY, Empty.EMPTY);
	assertEquals(new Pair(Empty.EMPTY, Empty.EMPTY),
		     new Pair(Empty.EMPTY, Empty.EMPTY));
    }


        @Test
        public void testSingleArgNegation() {
	    assertTrue(NumberTower.equal(Kernel._dash_(new Object[] { new Rational(1, 1) }),
					  new Rational(-1, 1)));
	}


	// Fill me in with more test cases!
	@Test
	public void testTan() {
	    assertTrue(Kernel._equal_(new Object[] {Kernel.tan(Kernel.ZERO), Kernel.ZERO})
				.isTrue());
	}

	@Test
	public void testSinh() {
	    assertTrue(Kernel._equal_(new Object[] {Kernel.sinh(Kernel.ZERO), Kernel.ZERO})
				.isTrue());
	}

	@Test
	public void testCosh() {
	    assertTrue(Kernel._equal_(new Object[] {Kernel.cosh(Kernel.ZERO), Kernel.ONE})
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
	    assertTrue(Kernel._equal_(new Object[] {Kernel.expt(Kernel.TWO, Kernel.ONE),
						    Kernel.TWO}).isTrue());
	}

	@Test
	public void testExp() {
	    assertTrue(Kernel._equal_(new Object[] {Kernel.exp(Kernel.ZERO), Kernel.ONE}).isTrue());
	}

	// @Test
	// public void testBigExp() {
	// assertEquals(Kernel.expt(new Rational(2, 1), new Rational(128, 1))
	// .toString(), "340282366920938463463374607431768211456");
	// }

	@Test
	public void testLog() {
		assertTrue(Kernel
			   ._equal_(new Object[]{Kernel.log(Kernel.ONE), FloatPoint.fromInt(0)})
			   .isTrue());
	}

	@Test
	public void testPositive_question_() {
		assertTrue(Kernel.positive_question_(Kernel.ONE).isTrue());
	}

	@Test
	public void testNegative_question_() {
		assertTrue(Kernel.negative_question_(new Complex(-1, 0)).isTrue());
		assertTrue(Kernel.negative_question_(Kernel.ONE).isFalse());
	}

	@Test
	public void testSgn() {
	    assertTrue(Kernel._equal_(new Object[] {Kernel.sgn(Kernel.ONE), Kernel.ONE}).isTrue());
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
	    assertTrue(Kernel._equal_(new Object[] {Kernel.gcd(new Object[] {Kernel.TWO, Kernel.THREE}),
						    Kernel.ONE}).isTrue());
	}

	@Test
	public void testLCM() {
	    assertTrue(Kernel._equal_(new Object[] {Kernel.lcm(new Object[] {Kernel.TWO, Kernel.THREE}),
						    Kernel.SIX}).isTrue());
	}

	@Test
	public void testPair_question_() {
		assertTrue(Kernel.pair_question_(Empty.EMPTY).isFalse());
	}

	@Test
	public void testCons_question_() {
		assertTrue(Kernel.cons_question_(Empty.EMPTY).isFalse());
	}

	@Test
	public void testNumber_question_() {
		assertTrue(Kernel.number_question_(Kernel.TWO).isTrue());
		// assertTrue(Kernel.number_question_(
		// Complex.makeComplex(Rational.ZERO, Rational.ONE)).isTrue());
		assertTrue(Kernel.number_question_(
				Kernel.sqrt(FloatPoint.fromString("-1"))).isTrue());
	}

	@Test
	public void testRational_question_() {
		assertTrue(Kernel.rational_question_(Kernel.TWO).isTrue());
	}

	@Test
	public void testQuotient() {
	    assertTrue(Kernel._equal_(new Object[] { Kernel.quotient(Kernel.SIX, Kernel.FOUR),
						     Kernel.ONE}).isTrue());
	}

	@Test
	public void testRemainder() {
	    assertTrue(Kernel._equal_(new Object[] {Kernel.remainder(Kernel.SIX, Kernel.FOUR),
						    Kernel.TWO}).isTrue());
	}

	@Test
	public void testNumerator() {
	    assertTrue(Kernel._equal_(new Object[] { Kernel.numerator(new Rational(12, 10)),
						     Kernel.SIX}).isTrue());
	}

	@Test
	public void testDenominator() {
	    assertTrue(Kernel._equal_(new Object[] { Kernel.denominator(new Rational(12, 10)),
						     Kernel.FIVE}).isTrue());
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
					  new Object[] { Kernel.length(Kernel.cons(Kernel.THREE, Empty.EMPTY)),
							 Kernel.ONE}).isTrue());
	}

	@Test
	public void testList_ref_() {
	    assertTrue(Kernel._equal_( new Object[] {
			Kernel.list_dash_ref(Kernel.cons(Kernel.THREE, Empty.EMPTY),
					     Kernel.ZERO), 
			Kernel.THREE}).isTrue());
	}

	@Test
	public void testRound() {
	    assertTrue(Kernel._equal_(new Object[] {Kernel.round(FloatPoint.fromString("3.1")),
						    FloatPoint.fromString("3")}).isTrue());
	    assertTrue(Kernel._equal_(new Object[] {Kernel.round(FloatPoint.fromString("3.5")),
						    FloatPoint.fromString("4")}).isTrue());
	    assertTrue(Kernel._equal_(new Object[] { Kernel.round(FloatPoint.fromString("-3.5")),
						     FloatPoint.fromString("-4")}).isTrue());
	    assertTrue(Kernel._equal_(new Object[] {Kernel.round(FloatPoint.fromString("-3.1")),
						    FloatPoint.fromString("-3")}).isTrue());
	}

	@Test
	public void testReal_question_() {
		assertTrue(Kernel.real_question_(new Complex(1, 0)).isTrue());
		assertTrue(Kernel.real_question_(Kernel.sqrt(new Rational(-1, 1)))
				.isFalse());
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
							  new Object[] {			  
							      Kernel.substring("world", Kernel.ZERO, Kernel.TWO), 
							      "wo"})
				.isTrue());
	}

	// @Test
	// public void testString_dash_ref() {
	// assertTrue(Kernel.char_equal__question_('w',
	// Kernel.string_dash_ref("world", Kernel.ZERO)).isTrue());
	// }

	@Test
	public void testString_dash_length() {
	    assertTrue(Kernel._equal_(new Object[] { Kernel.FIVE,
						     Kernel.string_dash_length("world")}).isTrue());
	}

	@Test
	public void testString_dash_copy() {
		assertTrue(Kernel.string_equal__question_(
							  new Object[] { Kernel.string_dash_copy("hi"), "hi"}).isTrue());
	}

	@Test
	public void testString_dash__greaterthan_number() {
	    assertTrue(Kernel._equal_(new Object[] { Kernel.string_dash__greaterthan_number("3"),
						     Kernel.THREE}).isTrue());
		assertTrue(Kernel._equal_(
					  new Object[] {
					      Kernel.string_dash__greaterthan_number("3.1"),
					      FloatPoint.fromString("3.1")}).isTrue());
		assertTrue(((Logic) (Kernel.string_dash__greaterthan_number("3.1a")))
				.isFalse());
	}

	@Test
	public void testEq_question_() {
		assertTrue(Kernel.eq_question_(Kernel.ONE, Kernel.ONE).isTrue());
		assertTrue(Kernel.eq_question_(Kernel.ONE, Kernel.TWO).isFalse());
		assertTrue(Kernel.eq_question_(
				Kernel.string_dash__greaterthan_number("3"),
				Kernel.string_dash__greaterthan_number("3")).isFalse());
		assertTrue(Kernel.eq_question_(
				Kernel.string_dash__greaterthan_number("3.1"),
				Kernel.string_dash__greaterthan_number("3.1")).isFalse());
		assertTrue(Kernel.eq_question_("3", Kernel.string_dash_copy("3"))
				.isFalse());
		assertTrue(Kernel.eq_question_("3", "3").isTrue());
	}

	@Test
	public void testChar_question_() {
		assertTrue(Kernel.char_question_('a').isTrue());
		assertTrue(Kernel.char_question_("a").isFalse());
	}

	@Test
	public void testChar_equal__question_() {
	    assertTrue(Kernel.char_equal__question_(new Object[] { 'a', 'a'} ).isTrue());
	    assertTrue(Kernel.char_equal__question_(new Object[] {'a', 'A'}).isFalse());
	    assertTrue(Kernel.char_equal__question_(new Object[] {'a', 'b'}).isFalse());
	    assertTrue(Kernel.char_equal__question_(new Object[] { new Character('a'),
								   new Character('a')}).isTrue());
	}

	@Test
	public void testChar_lessthan__question_() {
	    assertTrue(Kernel.char_lessthan__question_(new Object[] {'a', 'b'}).isTrue());
	    assertTrue(Kernel.char_lessthan__question_(new Object[] {'a', 'a'}).isFalse());
	    assertTrue(Kernel.char_lessthan__question_(new Object[] {'b', 'a'}).isFalse());
	}

	@Test
	public void testChar_lessthan__equal__question_() {
	    assertTrue(Kernel.char_lessthan__equal__question_(new Object[] {'a', 'b'}).isTrue());
	    assertTrue(Kernel.char_lessthan__equal__question_(new Object[] {'a', 'a'}).isTrue());
	    assertTrue(Kernel.char_lessthan__equal__question_(new Object[] {'b', 'a'}).isFalse());
	}

	@Test
	public void testChar_greaterthan_question_() {
	    assertTrue(Kernel.char_greaterthan__question_(new Object[] {'b', 'a'}).isTrue());
	    assertTrue(Kernel.char_greaterthan__question_(new Object[] {'b', 'b'}).isFalse());
	    assertTrue(Kernel.char_greaterthan__question_(new Object[] {'a', 'b'}).isFalse());
	}

	@Test
	public void testChar_greaterthan__equal__question_() {
	    assertTrue(Kernel.char_greaterthan__equal__question_(new Object[] {'b', 'a'}).isTrue());
	    assertTrue(Kernel.char_greaterthan__equal__question_(new Object[] {'a', 'b'})
				.isFalse());
	    assertTrue(Kernel.char_greaterthan__equal__question_(new Object[] {'b', 'b'}).isTrue());
	}

	@Test
	public void testChar_dash_upper_dash_case_question_() {
		assertTrue(Kernel.char_dash_upper_dash_case_question_('a').isFalse());
		assertTrue(Kernel.char_dash_upper_dash_case_question_('A').isTrue());
	}

	@Test
	public void testChar_dash_lower_dash_case_question_() {
		assertTrue(Kernel.char_dash_lower_dash_case_question_('A').isFalse());
		assertTrue(Kernel.char_dash_lower_dash_case_question_('a').isTrue());
	}

	@Test
	public void testChar_dash_numeric_question_() {
		assertTrue(Kernel.char_dash_numeric_question_('a').isFalse());
		assertTrue(Kernel.char_dash_numeric_question_('1').isTrue());
	}

	@Test
	public void testChar_dash_upcase() {
	    assertTrue(Kernel.char_equal__question_(new Object[] { Kernel.char_dash_upcase('1'),
								   '1'}).isTrue());
	    assertTrue(Kernel.char_equal__question_(new Object[] { Kernel.char_dash_upcase('A'),
								   'A'}).isTrue());
	    assertTrue(Kernel.char_equal__question_(new Object[] { Kernel.char_dash_upcase('a'),
								   'A'}).isTrue());
	}

	@Test
	public void testChar_dash_downcase() {
	    assertTrue(Kernel.char_equal__question_(new Object[] { Kernel.char_dash_downcase('1'),
								   '1'}).isTrue());
	    assertTrue(Kernel.char_equal__question_(new Object[] { Kernel.char_dash_downcase('a'),
								   'a'}).isTrue());
	    assertTrue(Kernel.char_equal__question_(new Object[] { Kernel.char_dash_downcase('A'),
								   'a'}).isTrue());
	}

	@Test
	public void testChar_dash_whitespace_question_() {
		assertTrue(Kernel.char_dash_whitespace_question_(' ').isTrue());
		assertTrue(Kernel.char_dash_whitespace_question_('a').isFalse());
	}

	@Test
	public void testChar_dash_alphabetic_question_() {
		assertTrue(Kernel.char_dash_alphabetic_question_('1').isFalse());
		assertTrue(Kernel.char_dash_alphabetic_question_('a').isTrue());
	}

	@Test
	public void testChar_dash_ci_equal__question_() {
	    assertTrue(Kernel.char_dash_ci_equal__question_(new Object[] {'a', 'a'}).isTrue());
	    assertTrue(Kernel.char_dash_ci_equal__question_(new Object[] {'a', 'A'}).isTrue());
	    assertTrue(Kernel.char_dash_ci_equal__question_(new Object[] {'a', 'b'}).isFalse());
	    assertTrue(Kernel.char_dash_ci_equal__question_(new Object[] { new Character('a'),
									   new Character('a')}).isTrue());
	}

	@Test
	public void testChar_dash_ci_greaterthan__question_() {
	    assertTrue(Kernel.char_dash_ci_greaterthan__question_(new Object[] {'B', 'a'})
				.isTrue());
	    assertTrue(Kernel.char_dash_ci_greaterthan__question_(new Object[] {'b', 'A'})
				.isTrue());
	    assertTrue(Kernel.char_dash_ci_greaterthan__question_(new Object[] {'a', 'b'})
				.isFalse());
	    assertTrue(Kernel.char_dash_ci_greaterthan__question_(new Object[] {'a', 'A'})
				.isFalse());
	}

	@Test
	public void testChar_dash_ci_greaterthan__equal__question_() {
	    assertTrue(Kernel.char_dash_ci_greaterthan__equal__question_(new Object[] {'B', 'a'})
				.isTrue());
	    assertTrue(Kernel.char_dash_ci_greaterthan__equal__question_(new Object[] {'b', 'A'})
				.isTrue());
	    assertTrue(Kernel.char_dash_ci_greaterthan__equal__question_(new Object[] {'a', 'b'})
				.isFalse());
	    assertTrue(Kernel.char_dash_ci_greaterthan__equal__question_(new Object[] {'a', 'A'})
				.isTrue());
	}

	@Test
	public void testChar_dash_ci_lessthan__question_() {
	    assertTrue(Kernel.char_dash_ci_lessthan__question_(new Object[] {'B', 'a'}).isFalse());
	    assertTrue(Kernel.char_dash_ci_lessthan__question_(new Object[] {'b', 'A'}).isFalse());
	}

	@Test
	public void testChar_dash_ci_lessthan__equal__question_() {
	    assertTrue(Kernel.char_dash_ci_lessthan__equal__question_(new Object[] {'B', 'a'})
				.isFalse());
	    assertTrue(Kernel.char_dash_ci_lessthan__equal__question_(new Object[] {'b', 'A'})
				.isFalse());

	}

	@Test
	public void testString_dash_ci_equal__question_() {
	    assertTrue(Kernel.string_dash_ci_equal__question_(new Object[] {"Hi", "hI"}).isTrue());
	    assertTrue(Kernel.string_dash_ci_equal__question_(new Object[] {"hi", "hi"}).isTrue());
	    assertTrue(Kernel.string_dash_ci_equal__question_(new Object[] {"hi", "ha"}).isFalse());
	}

	@Test
	public void testString_dash_ci_greaterthan__question_() {
	    assertTrue(Kernel.string_dash_ci_greaterthan__question_(new Object[] {"Hi", "hi"})
				.isFalse());
	    assertTrue(Kernel.string_dash_ci_greaterthan__question_(new Object[] {"H", "a"})
				.isTrue());
	    assertTrue(Kernel.string_dash_ci_greaterthan__question_(new Object[] {"h", "a"})
				.isTrue());
	}

	@Test
	public void testString_dash_ci_greaterthan__equal__question_() {
	    assertTrue(Kernel.string_dash_ci_greaterthan__equal__question_(new Object[] { "Hi",
											  "hI"}).isTrue());
		assertTrue(Kernel
			   .string_dash_ci_greaterthan__equal__question_(new Object[] { "H", "a"})
				.isTrue());
	}

	@Test
	public void testString_dash_ci_lessthan__question_() {
	    assertTrue(Kernel.string_dash_ci_lessthan__question_(new Object[] {"h", "M"}).isTrue());
	    assertTrue(Kernel.string_dash_ci_lessthan__question_(new Object[] {"h", "H"})
				.isFalse());
	    assertTrue(Kernel.string_dash_ci_lessthan__question_(new Object[] {"b", "a"})
				.isFalse());
	}

	@Test
	public void testString_dash_ci_lessthan__equal__question_() {
	    assertTrue(Kernel.string_dash_ci_lessthan__equal__question_(new Object[] {"h", "M"})
				.isTrue());
	    assertTrue(Kernel.string_dash_ci_lessthan__equal__question_(new Object[] {"h", "H"})
				.isTrue());
	    assertTrue(Kernel.string_dash_ci_lessthan__equal__question_(new Object[] {"b", "a"})
				.isFalse());
	}

	@Test
	public void testString_dash__greaterthan_symbol() {
		assertTrue(Symbol.makeInstance("hi").equals(Symbol.makeInstance("hi")));
		assertTrue(Symbol.makeInstance("hi").equals(Symbol.makeInstance("ha")) == false);
	}

	private boolean arrayEqual(Object[] arr1, Object[] arr2) {
		if (arr1.length != arr2.length)
			return false;

		for (int i = 0; i < arr1.length; i++)
			if (arr1[i].equals(arr2[i]) == false)
				return false;

		return true;
	}

	@Test
	public void testString_dash_greaterthan_list() {

		org.plt.types.List lst1 = new Pair('h', new Pair('i', Empty.EMPTY));
		org.plt.types.List lst2 = new Pair('h', new Pair('a', Empty.EMPTY));

		assertTrue(Kernel.equal_question_(
				Kernel.string_dash__greaterthan_list("hi"), lst1).isTrue());
		assertTrue(Kernel.equal_question_(
				Kernel.string_dash__greaterthan_list("hi"), lst2).isFalse());
	}

	@Test
	public void testString_dash_append() {
	    assertTrue(Kernel.string_equal__question_(new Object[] { "hello world",
								     Kernel.string_dash_append(new Object[] {"hello", " world"})}).isTrue());
	}

	@Test
	public void testString() {
		Object[] arr = { 'h', 'i' };
		assertTrue(Kernel.string_equal__question_(new Object[] {"hi", Kernel.string(arr)})
				.isTrue());
	}

	@Test
	public void testMake_dash_string() {
	    assertTrue(Kernel.string_equal__question_(new Object[] { "hh",
								     Kernel.make_dash_string(Kernel.TWO, 'h')}).isTrue());
	}

	@Test
	public void testList_dash__greaterthan_string() {
		org.plt.types.List lst = new Pair('h', new Pair('i', Empty.EMPTY));
		assertTrue(Kernel.string_equal__question_(
							  new Object[] { Kernel.list_dash__greaterthan_string(lst), "hi"}).isTrue());
	}

	@Test
	public void testChar_dash__greaterthan_integer() {
	    assertTrue(Kernel._equal_(new Object[] { Kernel.char_dash__greaterthan_integer('1'),
						     new Rational(49, 1)}).isTrue());
	    assertTrue(Kernel._equal_(new Object[] { Kernel.char_dash__greaterthan_integer('d'),
						     new Rational(100, 1)}).isTrue());
	}

	@Test
	public void testInteger_dash__greaterthan_char() {
		assertTrue(Kernel.char_equal__question_(
							new Object[] { Kernel.integer_dash__greaterthan_char(new Rational(49, 1)),
								       new Character('1')}).isTrue());
		assertTrue(Kernel.char_equal__question_(
							new Object[] { Kernel.integer_dash__greaterthan_char(new Rational(100, 1)),
								       new Character('d')}).isTrue());
	}

	@Test
	public void testMember() {
		org.plt.types.List lst = new Pair("h", new Pair("i", Empty.EMPTY));
		assertTrue(Kernel.member("i", lst).isTrue());
		assertTrue(Kernel.member("a", lst).isFalse());
	}

	@Test
	public void testMemq() {
		org.plt.types.List lst1 = new Pair("h", new Pair("i", Empty.EMPTY));
		org.plt.types.List lst2 = new Pair(Kernel.ONE, new Pair(Kernel.TWO,
				Empty.EMPTY));
		org.plt.types.List lst3 = new Pair(Kernel.TWO, Empty.EMPTY);

		assertTrue(Kernel.equal_question_(lst1, Kernel.memq("h", lst1))
				.isTrue());
		assertTrue(((org.plt.types.Logic) (Kernel.memq("a", lst1))).isFalse());
		assertTrue(Kernel.equal_question_(lst3, Kernel.memq(Kernel.TWO, lst2))
				.isTrue());
	}

	@Test
	public void testMemv() {
		org.plt.types.List lst1 = new Pair("h", new Pair("i", Empty.EMPTY));
		org.plt.types.List lst2 = new Pair(Kernel.ONE, new Pair(Kernel.TWO,
				Empty.EMPTY));
		org.plt.types.List lst3 = new Pair(Kernel.TWO, Empty.EMPTY);

		assertTrue(Kernel.equal_question_(lst1, Kernel.memv("h", lst1))
				.isTrue());
		assertTrue(((org.plt.types.Logic) (Kernel.memv("a", lst1))).isFalse());
		assertTrue(Kernel.equal_question_(lst3, Kernel.memv(Kernel.TWO, lst2))
				.isTrue());
	}

	@Test
	public void testAppend() {
		org.plt.types.List lst1 = new Pair(1, new Pair(2, Empty.EMPTY));
		org.plt.types.List lst2 = new Pair(3, new Pair(4, Empty.EMPTY));
		org.plt.types.List lst3 = new Pair(1, new Pair(2, new Pair(3, new Pair(
				4, Empty.EMPTY))));

		assertTrue(Kernel.equal_question_(lst3, Kernel.append(new Object[] { lst1, lst2} ))
				.isTrue());
	}

	@Test
	public void testAssq() {
		org.plt.types.List lst1 = new Pair(new Pair(Kernel.ONE, Empty.EMPTY),
				Empty.EMPTY);
		org.plt.types.List lst2 = new Pair(new Pair("hi", Empty.EMPTY),
				Empty.EMPTY);

		assertTrue(Kernel.equal_question_(new Pair(Kernel.ONE, Empty.EMPTY),
				Kernel.assq(Kernel.ONE, lst1)).isTrue());

		assertTrue(((org.plt.types.Logic) (Kernel.assq("ha", lst2))).isFalse());
	}

	@Test
	public void testCurrent_dash_seconds() {
		org.plt.types.Number firstTime = Kernel.current_dash_seconds();
		try {
			Thread.sleep(1000);
		} catch (InterruptedException e) {
		}
		org.plt.types.Number secondTime = Kernel.current_dash_seconds();
		assertTrue(Kernel._lessthan_(new Object[] { firstTime, secondTime }).isTrue());
	}

	@Test
	public void testComplex_question_() {
		assertTrue(Kernel.complex_question_(Kernel.sqrt(new Rational(-1, 1)))
				.isTrue());
		assertTrue(Kernel.complex_question_(Kernel.ONE).isTrue());
		assertTrue(Kernel.complex_question_(new Rational(4, 3)).isTrue());
		assertTrue(Kernel.complex_question_(FloatPoint.fromString("4.3"))
				.isTrue());
		assertTrue(Kernel.complex_question_(
				Kernel.sqrt(FloatPoint.fromString("-1"))).isTrue());
		assertTrue(Kernel.complex_question_("hi").isFalse());
		assertTrue(Kernel.complex_question_('h').isFalse());
		assertTrue(Kernel.complex_question_(Logic.FALSE).isFalse());
		assertTrue(Kernel.complex_question_(Empty.EMPTY).isFalse());
	}

	@Test
	public void testReal_dash_part() {
	    assertTrue(Kernel._equal_(new Object[] { Kernel.ONE,
						     Kernel.real_dash_part(FloatPoint.fromString("1.0"))}).isTrue());
	    assertTrue(Kernel._equal_(new Object[] { FloatPoint.fromString("1.0"),
						     Kernel.real_dash_part(FloatPoint.fromString("1.0"))}).isTrue());
	    assertTrue(Kernel._equal_(new Object[] { Kernel.ONE,
						     Kernel.real_dash_part(FloatPoint.fromString("1.1"))}).isFalse());
		assertTrue(Kernel
				._equal_(
					 new Object[] { Kernel.ZERO,
						Kernel.real_dash_part(Kernel.sqrt(FloatPoint
										  .fromString("-1")))}).isTrue());
	}

	@Test
	public void testImag_dash_part() {
	    assertTrue(Kernel._equal_(new Object[] { Kernel.ZERO,
						     Kernel.imag_dash_part(FloatPoint.fromString("1.0"))}).isTrue());
	    assertTrue(Kernel._equal_(new Object[] { FloatPoint.fromString("0"),
						     Kernel.imag_dash_part(FloatPoint.fromString("1.0"))}).isTrue());
		assertTrue(Kernel
			   ._equal_(new Object[] {
						Kernel.ONE,
						Kernel.imag_dash_part(Kernel.sqrt(FloatPoint
										  .fromString("-1")))}).isTrue());
	}

	@Test
	public void testEqual_tilde__question_() {
		assertTrue(Kernel.equal_tilde__question_(Kernel.ONE, Kernel.THREE,
				Kernel.ONE).isFalse());
		assertTrue(Kernel.equal_tilde__question_(Kernel.ONE, Kernel.TWO,
				Kernel.ONE).isTrue());
		assertTrue(Kernel.equal_tilde__question_(Kernel.ONE, 'a', Kernel.ONE)
				.isFalse());
		assertTrue(Kernel.equal_tilde__question_('a', 'a', Kernel.ONE).isTrue());
	}

	@Test
	public void testExact_question_() {
		assertTrue(Kernel.exact_question_(Kernel.ONE).isTrue());
		assertTrue(Kernel.exact_question_(FloatPoint.fromString("-1.1"))
				.isFalse());
		assertTrue(Kernel.exact_question_(
				Kernel.sqrt(FloatPoint.fromString("-1"))).isTrue());
		assertTrue(Kernel.exact_question_(Kernel.pi).isFalse());
	}

	@Test
	public void testInexact_question_() {
		assertTrue(Kernel.inexact_question_(Kernel.ONE).isFalse());
		assertTrue(Kernel.inexact_question_(FloatPoint.fromString("-1.1"))
				.isTrue());
	}

	@Test
	public void testExact_dash__greaterthan_inexact() {
		assertTrue(Kernel._equal_(
					  new Object[] { Kernel.exact_dash__greaterthan_inexact(Kernel.pi), Kernel.pi})
				.isTrue());
		assertTrue(Kernel._equal_(
					  new Object[] {
					      Kernel.exact_dash__greaterthan_inexact(Kernel.ONE),
					      FloatPoint.fromString("1.0")}).isTrue());
	}

	@Test
	public void testInexact_dash__greaterthan_exact() {
		assertTrue(Kernel
			   ._equal_(new Object[] { Kernel.inexact_dash__greaterthan_exact(Kernel.pi),
						   Kernel.THREE}).isTrue());
	}

	@Test
	public void testAngle() {
		assertTrue(Kernel._equal_(
					  new Object[] { 
				Kernel.angle(Kernel.sqrt(FloatPoint.fromString("-1"))),
				Kernel._slash_(new Object[] {FloatPoint.PI, Kernel.TWO})}).isTrue());
		assertTrue(Kernel._equal_(new Object[] { Kernel.angle(Kernel.ONE), Kernel.ZERO})
				.isTrue());
		assertTrue(Kernel._equal_(new Object[] { Kernel.angle(FloatPoint.fromString("-2")),
							 FloatPoint.PI}).isTrue());
		assertTrue(Kernel._equal_(new Object[] { Kernel.angle(new Complex(1, 1)),
							 Kernel._slash_(new Object[] {FloatPoint.PI, Kernel.FOUR})}).isTrue());
		assertTrue(Kernel._equal_(new Object[] { Kernel.angle(new Complex(-1, 1)),
							 Kernel._star_(new Object[] { FloatPoint.PI, FloatPoint.fromString("0.75")})})
				.isTrue());
		assertTrue(Kernel._equal_(new Object[] { Kernel.angle(new Complex(1, -1)),
							 Kernel._slash_(new Object[] { FloatPoint.PI, FloatPoint.fromString("-4")})})
				.isTrue());
		assertTrue(Kernel._equal_(new Object[] { Kernel.angle(new Complex(-1, -1)),
							 Kernel._star_(new Object[] { FloatPoint.PI, FloatPoint.fromString("-0.75")})})
				.isTrue());
	}

	@Test
	public void testConjugate() {
		assertTrue(Kernel._equal_(
					  new Object[] { 
				Kernel.conjugate(FloatPoint.fromString("-1")),
				FloatPoint.fromString("-1")}).isTrue());
		assertTrue(Kernel._equal_(new Object[] { Kernel.conjugate(new Complex(1, 1)),
							 new Complex(1, -1)}).isTrue());
	}

	@Test
	public void testMagnitude() {
		assertTrue(Kernel._equal_(
					  new Object[] { 
					      Kernel.magnitude(FloatPoint.fromString("-1")), Kernel.ONE})
				.isTrue());
		assertTrue(Kernel._equal_(new Object[] { Kernel.magnitude(new Complex(-3, 4)),
							 Kernel.FIVE}).isTrue());
	}
}
