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
	public void testGreaterThanOrEqual() {
	assertFalse(Kernel._greaterthan__equal_(new Rational(0, 1),
						new Rational(480, 1),
						new Object[] {}).isTrue());
	assertTrue(Kernel._greaterthan__equal_(new Rational(480, 1),
					       new Rational(480, 1),
					       new Object[] {}).isTrue());
	assertTrue(Kernel._greaterthan__equal_(new Rational(481, 1),
					       new Rational(480, 1),
					       new Object[] {}).isTrue());
    }




        @Test
        public void testSingleArgNegation() {
	    assertTrue(NumberTower.equal(Kernel._dash_(new Rational(1, 1),
						       new Object[] {}),
					 new Rational(-1, 1)));
	}


	// Fill me in with more test cases!
	@Test
	public void testTan() {
	    assertTrue(NumberTower.equal(Kernel.tan(Kernel.ZERO),
					 Kernel.ZERO));

	}

	@Test
	public void testSinh() {
	    assertTrue(NumberTower.equal(Kernel.sinh(Kernel.ZERO), 
					 Kernel.ZERO));
	}

	@Test
	public void testCosh() {
	    assertTrue(NumberTower.equal(Kernel.cosh(Kernel.ZERO),
					 Kernel.ONE));
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
	    assertTrue(NumberTower.equal(Kernel.expt(Kernel.TWO, Kernel.ONE),
					 Kernel.TWO));
	}

	@Test
	public void testExp() {
	    assertTrue(NumberTower.equal(Kernel.exp(Kernel.ZERO), Kernel.ONE));
	}

	// @Test
	// public void testBigExp() {
	// assertEquals(Kernel.expt(new Rational(2, 1), new Rational(128, 1))
	// .toString(), "340282366920938463463374607431768211456");
	// }

	@Test
	public void testLog() {
		assertTrue(NumberTower.equal
			   (Kernel.log(Kernel.ONE), 
			    FloatPoint.fromInt(0)));
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
	    assertTrue(NumberTower.equal(Kernel.sgn(Kernel.ONE), 
					 Kernel.ONE));
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
	    assertTrue(NumberTower.equal(Kernel.gcd(Kernel.TWO, Kernel.THREE,
						    new Object[] {}),
					 Kernel.ONE));
	}

	@Test
	public void testLCM() {
	    assertTrue(NumberTower.equal(Kernel.lcm(Kernel.TWO, Kernel.THREE, 
						    new Object[] {}),
					 Kernel.SIX));
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
	    assertTrue(NumberTower.equal(Kernel.quotient(Kernel.SIX,
							 Kernel.FOUR),
					 Kernel.ONE));
	}

	@Test
	public void testRemainder() {
	    assertTrue(NumberTower.equal(Kernel.remainder(Kernel.SIX, 
							  Kernel.FOUR),
					 Kernel.TWO));
	}

	@Test
	public void testNumerator() {
	    assertTrue(NumberTower.equal(Kernel.numerator(new Rational(12, 10)),
				   Kernel.SIX));
	}

	@Test
	public void testDenominator() {
	    assertTrue(NumberTower.equal(Kernel.denominator(new Rational(12, 10)),
					 Kernel.FIVE));
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
		assertTrue(NumberTower.equal(Kernel.length(Kernel.cons(Kernel.THREE, Empty.EMPTY)),
					    Kernel.ONE));
	}

	@Test
	public void testList_ref_() {
	    assertTrue(NumberTower.equal
		       ((org.plt.types.Number)
			Kernel.list_dash_ref(Kernel.cons(Kernel.THREE, Empty.EMPTY),
					     Kernel.ZERO), 
			Kernel.THREE));
	}

	@Test
	public void testRound() {
	    assertTrue(NumberTower.equal(Kernel.round(FloatPoint.fromString("3.1")),
					 FloatPoint.fromString("3")));
	    assertTrue(NumberTower.equal(Kernel.round(FloatPoint.fromString("3.5")),
					 FloatPoint.fromString("4")));
	    assertTrue(NumberTower.equal(Kernel.round(FloatPoint.fromString("-3.5")),
					 FloatPoint.fromString("-4")));
	    assertTrue(NumberTower.equal(Kernel.round(FloatPoint.fromString("-3.1")),
					 FloatPoint.fromString("-3")));
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
	    assertTrue(Kernel.string_greaterthan__question_("b", "a", new Object[] {}).isTrue());
	    assertTrue(Kernel.string_greaterthan__question_("a", "a", new Object[] {}).isFalse());
	    assertTrue(Kernel.string_greaterthan__question_("a", "b", new Object [] {}).isFalse());
	}

	@Test
	public void testString_greaterthan__equal__question_() {
	    assertTrue(Kernel.string_greaterthan__equal__question_("b", "a", new Object[] {})
				.isTrue());
	    assertTrue(Kernel.string_greaterthan__equal__question_("a", "a", new Object[] {})
				.isTrue());
	    assertTrue(Kernel.string_greaterthan__equal__question_("a", "b", new Object[] {})
				.isFalse());
	}

	@Test
	public void testString_lessthan__question_() {
	    assertTrue(Kernel.string_lessthan__question_("b", "a", new Object[] {}).isFalse());
	    assertTrue(Kernel.string_lessthan__question_("a", "a", new Object[] {}).isFalse());
	    assertTrue(Kernel.string_lessthan__question_("a", "b", new Object[] {}).isTrue());
	}

	@Test
	public void testString_lessthan__equal__question_() {
	    assertTrue(Kernel.string_lessthan__equal__question_("b", "a", new Object[] {}).isFalse());
	    assertTrue(Kernel.string_lessthan__equal__question_("a", "a", new Object[] {}).isTrue());
	    assertTrue(Kernel.string_lessthan__equal__question_("a", "b", new Object[] {}).isTrue());
	}

	@Test
	public void testSubstring() {
		assertTrue(Kernel.string_equal__question_(Kernel.substring("world", Kernel.ZERO, Kernel.TWO), 
							  "wo",
							  new Object[] {			  
							      })
				.isTrue());
	}

	// @Test
	// public void testString_dash_ref() {
	// assertTrue(Kernel.char_equal__question_('w',
	// Kernel.string_dash_ref("world", Kernel.ZERO)).isTrue());
	// }

	@Test
	public void testString_dash_length() {
	    assertTrue(NumberTower.equal(Kernel.FIVE,
					 Kernel.string_dash_length("world")));
	}

	@Test
	public void testString_dash_copy() {
	    assertTrue(Kernel.string_equal__question_(Kernel.string_dash_copy("hi"), "hi", 
							  new Object[] { }).isTrue());
	}

	@Test
	public void testString_dash__greaterthan_number() {
	    assertTrue(NumberTower.equal((org.plt.types.Number)
					 Kernel.string_dash__greaterthan_number("3"),
					 Kernel.THREE));
	    assertTrue(NumberTower.equal((org.plt.types.Number)
					 Kernel.string_dash__greaterthan_number("3.1"),
					 FloatPoint.fromString("3.1")));
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
	    assertTrue(Kernel.char_equal__question_('a', 'a').isTrue());
	    assertTrue(Kernel.char_equal__question_('a', 'A').isFalse());
	    assertTrue(Kernel.char_equal__question_('a', 'b').isFalse());
	    assertTrue(Kernel.char_equal__question_(new Character('a'),
						    new Character('a')).isTrue());
	}

	@Test
	public void testChar_lessthan__question_() {
	    assertTrue(Kernel.char_lessthan__question_('a', 'b', new Object[] {}).isTrue());
	    assertTrue(Kernel.char_lessthan__question_('a', 'a', new Object[] {}).isFalse());
	    assertTrue(Kernel.char_lessthan__question_('b', 'a', new Object[] {}).isFalse());
	}

	@Test
	public void testChar_lessthan__equal__question_() {
	    assertTrue(Kernel.char_lessthan__equal__question_('a', 'b', new Object[] {}).isTrue());
	    assertTrue(Kernel.char_lessthan__equal__question_('a', 'a', new Object[] {}).isTrue());
	    assertTrue(Kernel.char_lessthan__equal__question_('b', 'a', new Object[] {}).isFalse());
	}

	@Test
	public void testChar_greaterthan_question_() {
	    assertTrue(Kernel.char_greaterthan__question_('b', 'a', new Object[] {}).isTrue());
	    assertTrue(Kernel.char_greaterthan__question_('b', 'b', new Object[] {}).isFalse());
	    assertTrue(Kernel.char_greaterthan__question_('a', 'b', new Object[] {}).isFalse());
	}

	@Test
	public void testChar_greaterthan__equal__question_() {
	    assertTrue(Kernel.char_greaterthan__equal__question_('b', 'a', new Object[] {}).isTrue());
	    assertTrue(Kernel.char_greaterthan__equal__question_('a', 'b', new Object[] {})
				.isFalse());
	    assertTrue(Kernel.char_greaterthan__equal__question_('b', 'b', new Object[] {}).isTrue());
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
	    assertTrue(Kernel.char_equal__question_(Kernel.char_dash_upcase('1'),
						    '1').isTrue());
	    assertTrue(Kernel.char_equal__question_(Kernel.char_dash_upcase('A'),
						    'A').isTrue());
	    assertTrue(Kernel.char_equal__question_(Kernel.char_dash_upcase('a'),
						    'A').isTrue());
	}

	@Test
	public void testChar_dash_downcase() {
	    assertTrue(Kernel.char_equal__question_(Kernel.char_dash_downcase('1'),
						    '1').isTrue());
	    assertTrue(Kernel.char_equal__question_(Kernel.char_dash_downcase('a'),
								   'a').isTrue());
	    assertTrue(Kernel.char_equal__question_(Kernel.char_dash_downcase('A'),
								   'a').isTrue());
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
	    assertTrue(Kernel.char_dash_ci_equal__question_('a', 'a', new Object[] {}).isTrue());
	    assertTrue(Kernel.char_dash_ci_equal__question_('a', 'A', new Object[] {}).isTrue());
	    assertTrue(Kernel.char_dash_ci_equal__question_('a', 'b', new Object[] {}).isFalse());
	    assertTrue(Kernel.char_dash_ci_equal__question_(new Character('a'),
							    new Character('a'),
							    new Object[] {}).isTrue());
	}

	@Test
	public void testChar_dash_ci_greaterthan__question_() {
	    assertTrue(Kernel.char_dash_ci_greaterthan__question_('B', 'a', new Object[] {})
				.isTrue());
	    assertTrue(Kernel.char_dash_ci_greaterthan__question_('b', 'A', new Object[] {})
				.isTrue());
	    assertTrue(Kernel.char_dash_ci_greaterthan__question_('a', 'b', new Object[] {})
				.isFalse());
	    assertTrue(Kernel.char_dash_ci_greaterthan__question_('a', 'A', new Object[] {})
				.isFalse());
	}

	@Test
	public void testChar_dash_ci_greaterthan__equal__question_() {
	    assertTrue(Kernel.char_dash_ci_greaterthan__equal__question_('B', 'a', new Object[] {})
				.isTrue());
	    assertTrue(Kernel.char_dash_ci_greaterthan__equal__question_('b', 'A', new Object[] {})
				.isTrue());
	    assertTrue(Kernel.char_dash_ci_greaterthan__equal__question_('a', 'b', new Object[] {})
				.isFalse());
	    assertTrue(Kernel.char_dash_ci_greaterthan__equal__question_('a', 'A', new Object[] {})
				.isTrue());
	}

	@Test
	public void testChar_dash_ci_lessthan__question_() {
	    assertTrue(Kernel.char_dash_ci_lessthan__question_('B', 'a', new Object[] {}).isFalse());
	    assertTrue(Kernel.char_dash_ci_lessthan__question_('b', 'A', new Object[] {}).isFalse());
	}

	@Test
	public void testChar_dash_ci_lessthan__equal__question_() {
	    assertTrue(Kernel.char_dash_ci_lessthan__equal__question_('B', 'a', new Object[] {})
				.isFalse());
	    assertTrue(Kernel.char_dash_ci_lessthan__equal__question_('b', 'A', new Object[] {})
				.isFalse());

	}

	@Test
	public void testString_dash_ci_equal__question_() {
	    assertTrue(Kernel.string_dash_ci_equal__question_("Hi", "hI", new Object[] {}).isTrue());
	    assertTrue(Kernel.string_dash_ci_equal__question_("hi", "hi", new Object[] {}).isTrue());
	    assertTrue(Kernel.string_dash_ci_equal__question_("hi", "ha", new Object[] {}).isFalse());
	}

	@Test
	public void testString_dash_ci_greaterthan__question_() {
	    assertTrue(Kernel.string_dash_ci_greaterthan__question_("Hi", "hi", new Object[] {})
				.isFalse());
	    assertTrue(Kernel.string_dash_ci_greaterthan__question_("H", "a", new Object[] {})
				.isTrue());
	    assertTrue(Kernel.string_dash_ci_greaterthan__question_("h", "a", new Object[] {})
				.isTrue());
	}

	@Test
	public void testString_dash_ci_greaterthan__equal__question_() {
	    assertTrue(Kernel.string_dash_ci_greaterthan__equal__question_( "Hi",
									    "hI", new Object[] {}).isTrue());
		assertTrue(Kernel
			   .string_dash_ci_greaterthan__equal__question_( "H", "a", new Object[] {})
				.isTrue());
	}

	@Test
	public void testString_dash_ci_lessthan__question_() {
	    assertTrue(Kernel.string_dash_ci_lessthan__question_("h", "M", new Object[] {}).isTrue());
	    assertTrue(Kernel.string_dash_ci_lessthan__question_("h", "H", new Object[] {})
				.isFalse());
	    assertTrue(Kernel.string_dash_ci_lessthan__question_("b", "a", new Object[] {})
				.isFalse());
	}

	@Test
	public void testString_dash_ci_lessthan__equal__question_() {
	    assertTrue(Kernel.string_dash_ci_lessthan__equal__question_("h", "M", new Object[] {})
				.isTrue());
	    assertTrue(Kernel.string_dash_ci_lessthan__equal__question_("h", "H", new Object[] {})
				.isTrue());
	    assertTrue(Kernel.string_dash_ci_lessthan__equal__question_("b", "a", new Object[] {})
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
	    assertTrue(Kernel.string_equal__question_("hello world",
						      Kernel.string_dash_append("hello",new Object[] { " world"}), new Object[] {}).isTrue());
	}

	@Test
	public void testString() {
	    assertTrue(Kernel.string_equal__question_("hi", Kernel.string('h', new Object[]{ 'i' }), new Object[] {})
		       .isTrue());
	}

	@Test
	public void testMake_dash_string() {
	    assertTrue(Kernel.string_equal__question_( "hh",
						       Kernel.make_dash_string(Kernel.TWO, 'h'), new Object[] {}).isTrue());
	}

	@Test
	public void testList_dash__greaterthan_string() {
		org.plt.types.List lst = new Pair('h', new Pair('i', Empty.EMPTY));
		assertTrue(Kernel.string_equal__question_(Kernel.list_dash__greaterthan_string(lst), "hi", 
							  new Object[] { }).isTrue());
	}

	@Test
	public void testChar_dash__greaterthan_integer() {
	    assertTrue(NumberTower.equal( Kernel.char_dash__greaterthan_integer('1'),
					  new Rational(49, 1)));
	    assertTrue(NumberTower.equal( Kernel.char_dash__greaterthan_integer('d'),
					  new Rational(100, 1)));
	}

	@Test
	public void testInteger_dash__greaterthan_char() {
		assertTrue(Kernel.char_equal__question_(Kernel.integer_dash__greaterthan_char(new Rational(49, 1)),
							new Character('1')).isTrue());
		assertTrue(Kernel.char_equal__question_(Kernel.integer_dash__greaterthan_char(new Rational(100, 1)),
							new Character('d')).isTrue());
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

		assertTrue(Kernel.equal_question_(lst3, Kernel.append(lst1, new Object[] {lst2} ))
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
		assertTrue(Kernel._lessthan_(firstTime, secondTime, new Object[] {}).isTrue());
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
	    assertTrue(Kernel._equal_(Kernel.ONE, 
				      Kernel.real_dash_part(FloatPoint.fromString("1.0")),
				      new Object[] {}).isTrue());
	    assertTrue(NumberTower.equal(FloatPoint.fromString("1.0"),
					 Kernel.real_dash_part(FloatPoint.fromString("1.0"))));
	    assertFalse(NumberTower.equal(Kernel.ONE,
					  Kernel.real_dash_part(FloatPoint.fromString("1.1"))));
	    assertTrue(NumberTower.equal(Kernel.ZERO,
					 Kernel.real_dash_part(Kernel.sqrt(FloatPoint
									   .fromString("-1")))));
	}

	@Test
	public void testImag_dash_part() {
	    assertTrue(NumberTower.equal(Kernel.ZERO,
					 Kernel.imag_dash_part(FloatPoint.fromString("1.0"))));
	    assertTrue(NumberTower.equal(FloatPoint.fromString("0"),
					 Kernel.imag_dash_part(FloatPoint.fromString("1.0"))));
	    assertTrue(NumberTower.equal(Kernel.ONE,
					 Kernel.imag_dash_part(Kernel.sqrt(FloatPoint
									   .fromString("-1")))));
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
		assertTrue(NumberTower.equal
			   (Kernel.exact_dash__greaterthan_inexact(Kernel.pi), Kernel.pi));
		assertTrue(NumberTower.equal
			   (Kernel.exact_dash__greaterthan_inexact(Kernel.ONE),
			    FloatPoint.fromString("1.0")));
	}

	@Test
	public void testInexact_dash__greaterthan_exact() {
		assertTrue(NumberTower.equal
			   (Kernel.inexact_dash__greaterthan_exact(Kernel.pi),
			    Kernel.THREE));
	}

	@Test
	public void testAngle() {
		assertTrue(NumberTower.equal
			   (Kernel.angle(Kernel.sqrt(FloatPoint.fromString("-1"))),
			    Kernel._slash_(FloatPoint.PI, new Object[] { Kernel.TWO})));
		assertTrue(NumberTower.equal( Kernel.angle(Kernel.ONE), Kernel.ZERO));
		assertTrue(NumberTower.equal( Kernel.angle(FloatPoint.fromString("-2")),
					      FloatPoint.PI));
		assertTrue(NumberTower.equal(Kernel.angle(new Complex(1, 1)),
					     Kernel._slash_(FloatPoint.PI, new Object[] { Kernel.FOUR})));
		assertTrue(NumberTower.equal(Kernel.angle(new Complex(-1, 1)),
					     Kernel._star_(new Object[] { FloatPoint.PI, FloatPoint.fromString("0.75")})));
		assertTrue(NumberTower.equal(Kernel.angle(new Complex(1, -1)),
					     Kernel._slash_(FloatPoint.PI, new Object[] { FloatPoint.fromString("-4")})));
		assertTrue(NumberTower.equal(Kernel.angle(new Complex(-1, -1)),
					     Kernel._star_(new Object[] { FloatPoint.PI, FloatPoint.fromString("-0.75")})));
	}

	@Test
	public void testConjugate() {
	    assertTrue(NumberTower.equal
		       (Kernel.conjugate(FloatPoint.fromString("-1")),
			FloatPoint.fromString("-1")));
	    assertTrue(NumberTower.equal(Kernel.conjugate(new Complex(1, 1)),
					 new Complex(1, -1)));
	}

	@Test
	public void testMagnitude() {
		assertTrue(NumberTower.equal
			   (Kernel.magnitude(FloatPoint.fromString("-1")), Kernel.ONE));
		assertTrue(NumberTower.equal(Kernel.magnitude(new Complex(-3, 4)),
					     Kernel.FIVE));
	}
}
