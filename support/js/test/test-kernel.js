var getTests;


(function() {

    var Rational = plt.types.Rational;
    var FloatPoint = plt.types.FloatPoint;
    var Complex = plt.types.Complex;
    var Kernel = plt.Kernel;
    var PI = plt.Kernel.pi;
    var String = plt.types.String;
    var Symbol = plt.types.Symbol;
    var Logic = plt.types.Logic;
    var Cons = plt.types.Cons;
    var Empty = plt.types.Empty;
    var EMPTY = Empty.EMPTY;
    var Char = plt.types.Char;


    getTests = function() {

	return new Test.Unit.Runner( {

	    setup: function() {
	    },
	    
	    teardown: function() {
		
	    },


	    
	    testRationalReduction: function() {
		var n1 = Rational.makeInstance(1,2);
		var n2 = Rational.makeInstance(5, 10);
		var n3 = Rational.makeInstance(5, 12);
		this.assert(n1.isEqual(n2));
		this.assert(! n2.isEqual(n3));
	    },

	    testEqv: function() {
		this.assert(Kernel.eqv_question_(FloatPoint.makeInstance(Number.NaN),
						 FloatPoint.makeInstance(Number.NaN)));


		this.assert(Kernel.eqv_question_(FloatPoint.makeInstance(Number.POSITIVE_INFINITY),
						 FloatPoint.makeInstance(Number.POSITIVE_INFINITY)));


		this.assert(Kernel.eqv_question_(FloatPoint.makeInstance(Number.NEGATIVE_INFINITY),
						 FloatPoint.makeInstance(Number.NEGATIVE_INFINITY)));


	    },
	    


	    testEqual: function(){
		var n1 = Rational.makeInstance(2,1);
		var n2 = FloatPoint.makeInstance(2.0);
		this.assert(Kernel.equal_question_(n1, n2));
		
		var n3 = Complex.makeInstance(Rational.makeInstance(2),Rational.makeInstance(0));
		var n4 = Complex.makeInstance(Rational.makeInstance(2),Rational.makeInstance(1));
		this.assert(Kernel.equal_question_(n1, n3));
		this.assert(!Kernel.equal_question_(n3, n4));
		
		this.assert(Kernel.equal_question_(String.makeInstance("hi"), String.makeInstance("hi")));


		this.assert(Kernel._equal_(Rational.makeInstance(1, 2),
					   Rational.makeInstance(2, 4),
					   [])); 

		this.assert(false == Kernel._equal_(Rational.makeInstance(1, 2),
						    Rational.makeInstance(2, 5),
						    [])); 

	    },
	    
	    testAbs : function(){
		var n1 = Rational.makeInstance(-2,1);
		var n2 = Rational.makeInstance(4, 2);
		var n3 = Complex.makeInstance(Rational.makeInstance(2),
					      Rational.makeInstance(0));
		this.assert(Kernel.equal_question_(Kernel.abs(n1), n2));
		this.assert(Kernel.equal_question_(Kernel.abs(n3), n2));
	    },
	    
	    testAdd : function(){
		var n1 = [Rational.makeInstance(2,1), Rational.makeInstance(3,1)];
		this.assert(Kernel.equal_question_(Kernel._plus_(n1), Rational.makeInstance(5,1)));
		var n2 = [Rational.makeInstance(2,1), FloatPoint.makeInstance(2.1)];
		this.assert(Kernel.equal_question_(Kernel._plus_(n2), FloatPoint.makeInstance(4.1)));
		var n3 = [Rational.makeInstance(2,1), Complex.makeInstance(Rational.makeInstance(2),Rational.makeInstance(2))];
		this.assert(Kernel.equal_question_(Kernel._plus_(n3), Complex.makeInstance(Rational.makeInstance(4),Rational.makeInstance(2))));
		var n4 = [FloatPoint.makeInstance(3.1), Complex.makeInstance(Rational.makeInstance(2),Rational.makeInstance(2))];
		var a1 = Kernel._plus_(n4);
		this.assert(Kernel.equal_question_(a1, Complex.makeInstance(FloatPoint.makeInstance(5.1), Rational.makeInstance(2))));
		var n5 = [Complex.makeInstance(Rational.makeInstance(2),Rational.makeInstance( 2)), Complex.makeInstance(Rational.makeInstance(3),Rational.makeInstance( 2))];
		this.assert(Kernel.equal_question_(Kernel._plus_(n5), Complex.makeInstance(Rational.makeInstance(5),Rational.makeInstance( 4))));
	    },

	    testDivisionByZero: function() {
		Kernel._slash_(Rational.ONE, [Rational.ONE]);
		this.assertRaise("MobyRuntimeError", 
				 function() {
				     Kernel._slash_(Rational.ONE, [Rational.ZERO]);
				 });

		this.assertRaise("MobyRuntimeError", 
				 function() {
				     Kernel._slash_(FloatPoint.makeInstance(1),
						    [Rational.ZERO]);
				 });

		this.assertRaise("MobyRuntimeError", 
				 function() {
				     Kernel._slash_(FloatPoint.makeInstance(1),
						    [FloatPoint.makeInstance(0)]);
				 });
	    },

	    testAdd1 : function() {
		this.assert(Kernel.equal_question_(Kernel.add1(Rational.ZERO), 
						   Rational.ONE));

		this.assert(Kernel.equal_question_(Kernel.add1(Rational.ONE), 
						   Rational.makeInstance(2)));

		this.assert(Kernel.equal_question_(Kernel.add1(Rational.makeInstance(2)), 
						   Rational.makeInstance(3)));
	    },


	    testSub1 : function() {
		this.assert(Kernel.equal_question_(Kernel.sub1(Rational.ZERO), 
						   Rational.makeInstance(-1)));

		this.assert(Kernel.equal_question_(Kernel.sub1(Rational.ONE), 
						   Rational.makeInstance(0)));

		this.assert(Kernel.equal_question_(Kernel.sub1(Rational.makeInstance(2)), 
						   Rational.makeInstance(1)));
	    },

	    testAddFloats: function() {
		this.assertEqual(0.1, 
				 Kernel._plus_([Rational.makeInstance(0), 
						FloatPoint.makeInstance(0.1)]).toFloat());
	    },

	    
	    testSubtract : function(){
		var n1 = [Rational.makeInstance(2,1), Rational.makeInstance(3,1)];
		this.assert(Kernel.equal_question_(Kernel._dash_(Rational.ZERO, n1), Rational.makeInstance(-5,1)));		
		var n2 = [Rational.makeInstance(2,1), FloatPoint.makeInstance(2.1)];
		this.assert(Kernel.equal_question_(Kernel._dash_(Rational.ZERO, n2), FloatPoint.makeInstance(-4.1)));
		var n3 = [Rational.makeInstance(2,1), Complex.makeInstance(Rational.makeInstance(2),Rational.makeInstance( 2))];
		this.assert(Kernel.equal_question_(Kernel._dash_(Rational.ZERO, n3), Complex.makeInstance(Rational.makeInstance(-4),Rational.makeInstance( -2))));
		var n4 = [FloatPoint.makeInstance(2.1), Complex.makeInstance(Rational.makeInstance(2),Rational.makeInstance( 2))];
		this.assert(Kernel.equal_question_(Kernel._dash_(Rational.ZERO, n4), Complex.makeInstance(FloatPoint.makeInstance(-4.1),Rational.makeInstance( -2))));
		var n5 = [Complex.makeInstance(Rational.makeInstance(2),Rational.makeInstance( 2)), Complex.makeInstance(Rational.makeInstance(3),Rational.makeInstance( 2))];
		this.assert(Kernel.equal_question_(Kernel._dash_(Rational.ZERO, n5), Complex.makeInstance(Rational.makeInstance(-5),Rational.makeInstance( -4))));
		
	    },
	    
	    testMultiply : function(){
		var n1 = [Rational.makeInstance(2,1), Rational.makeInstance(3,1)];
		this.assert(Kernel.equal_question_(Kernel._star_(n1), Rational.makeInstance(6,1)));
		var n2 = [Rational.makeInstance(2,1), FloatPoint.makeInstance(2.1)];
		this.assert(Kernel.equal_question_(Kernel._star_(n2), FloatPoint.makeInstance(4.2)));
		var n3 = [Rational.makeInstance(2,1), Complex.makeInstance(Rational.makeInstance(2),Rational.makeInstance( 2))];
		this.assert(Kernel.equal_question_(Kernel._star_(n3), Complex.makeInstance(Rational.makeInstance(4),Rational.makeInstance( 4))));
		var n4 = [FloatPoint.makeInstance(2.1), Complex.makeInstance(Rational.makeInstance(2),Rational.makeInstance( 2))];
		this.assert(Kernel.equal_question_(Kernel._star_(n4), Complex.makeInstance(FloatPoint.makeInstance(4.2),FloatPoint.makeInstance( 4.2))));
		var n5 = [Complex.makeInstance(Rational.makeInstance(2),Rational.makeInstance( 2)), Complex.makeInstance(Rational.makeInstance(3),Rational.makeInstance( 2))];
		this.assert(Kernel.equal_question_(Kernel._star_(n5), Complex.makeInstance(Rational.makeInstance(2),Rational.makeInstance( 10))));
	    },
	    
	    
	    testDivide : function(){
		var n1 = [Rational.makeInstance(2,1), Rational.makeInstance(3,1)];
		var six = Rational.makeInstance(6, 1);
		this.assert(Kernel.equal_question_(Kernel._slash_(six, n1), Rational.ONE));
		var n2 = [FloatPoint.makeInstance(1.5), FloatPoint.makeInstance(4.0)];
		this.assert(Kernel.equal_question_(Kernel._slash_(six, n2), Rational.ONE));
		var n3 = [Complex.makeInstance(3, 4), Complex.makeInstance(3, -4)];
		this.assert(Kernel.equal_question_(Kernel._slash_(FloatPoint.makeInstance(150), n3), six));

		this.assert(Kernel.equal_question_(Kernel._slash_(six, []),
						   Rational.makeInstance(1, 6)));
	    },
	    
	    
	    testConjugate : function(){
		var n1 = Rational.makeInstance(2,1);
		var n2 = FloatPoint.makeInstance(2.1);
		this.assert(Kernel.equal_question_(n1, Kernel.conjugate(n1)));
		this.assert(Kernel.equal_question_(n2, Kernel.conjugate(n2)));
		this.assert(Kernel.equal_question_(Complex.makeInstance(1, 2), Kernel.conjugate(Complex.makeInstance(1, -2))));
	    },
	    
	    testMagnitude : function(){
		var n1 = Rational.makeInstance(2,1);
		var n2 = FloatPoint.makeInstance(2.1);
		this.assert(Kernel.equal_question_(n1, Kernel.magnitude(n1)));
		this.assert(Kernel.equal_question_(n2, Kernel.magnitude(n2)));
		this.assert(Kernel.equal_question_(Complex.makeInstance(5, 0), Kernel.magnitude(Complex.makeInstance(3, -4))));
	    },
	    
	    testComparison : function(){	
		this.assert(Kernel._greaterthan_(Rational.makeInstance(2,1),
						 Rational.makeInstance(1,1), 
						 []));
		this.assert(Kernel._greaterthan_(FloatPoint.makeInstance(2.1),
						 Rational.makeInstance(2,1), []));
		this.assert(Kernel._greaterthan__equal_(FloatPoint.makeInstance(2.0),
							Rational.makeInstance(2,1),
							[]));
		this.assert(Kernel._greaterthan__equal_(Complex.makeInstance(2.0, 0),
							Rational.makeInstance(2,1),
							[]));


		this.assert(Kernel._lessthan_(Rational.makeInstance(2),
					      Rational.makeInstance(3), []));

		this.assert(! Kernel._lessthan_(Rational.makeInstance(3),
						Rational.makeInstance(2), []));
	    },

	    testComparisionTypes : function() {
		this.assertRaise("MobyTypeError",
				 function() {
				     Kernel._lessthan_(2, 3, [])});
		this.assertRaise("MobyTypeError",
				 function() {
				     Kernel._greaterthan_("2", "3", [])});
	    },

	    testComparisonMore: function() {
		this.assert(! Kernel._greaterthan_(Rational.makeInstance(2),
						   Rational.makeInstance(3), []));

		this.assert(Kernel._greaterthan_(Rational.makeInstance(3),
						 Rational.makeInstance(2), []));

		this.assert(! Kernel._greaterthan_(Rational.makeInstance(3),
						   Rational.makeInstance(3), []));

		this.assert(Kernel._lessthan__equal_(Rational.makeInstance(17),
						     Rational.makeInstance(17), []));

		this.assert(Kernel._lessthan__equal_(Rational.makeInstance(16),
						     Rational.makeInstance(17), []));

		this.assert(!Kernel._lessthan__equal_(Rational.makeInstance(16),
						      Rational.makeInstance(15), []));
		this.assertRaise("MobyTypeError",
				 function() {
				     Kernel._lessthan__equal_("2", "3", [])});
	    },

	    
	    testComparison2 : function () {
		var num = Rational.makeInstance(0, 1);
		var upper = Rational.makeInstance(480, 1);

		this.assert(Kernel._lessthan_(Rational.makeInstance(5, 1),
					      upper, []));
		this.assert(Kernel._lessthan_(Rational.makeInstance(6, 1),
					      upper, []));
		this.assert(Kernel._lessthan_(Rational.makeInstance(7, 1),
					      upper, []));
		this.assert(Kernel._lessthan_(Rational.makeInstance(8, 1),
					      upper, []));
		this.assert(Kernel._lessthan_(Rational.makeInstance(9, 1),
					      upper, []));

		for (var i = 0; i < 60; i++) {
		    this.assert(Kernel._lessthan_
				(num, upper, []));
		    num = Kernel._plus_([num, Rational.ONE]);
		}
	    },

	    
	    testAtan : function(){
		this.assert(Kernel.equal_question_(Kernel.atan(Rational.ONE, []), plt.Kernel.pi.half().half()));
	    },
	    
	    testLog : function(){
		this.assert(Kernel.equal_question_(Kernel.log(Rational.ONE), Rational.ZERO));		
		this.assert(Kernel.equal_question_(Kernel.log(Complex.makeInstance(0,1)), plt.Kernel.pi.toComplex().timesI().half()));
		this.assert(Kernel.equal_question_(Kernel.log(FloatPoint.makeInstance(-1)), plt.Kernel.pi.toComplex().timesI()));
	    },
	    
	    testAngle : function(){
		this.assert(Kernel.equal_question_(Kernel.angle(Complex.makeInstance(0,1)), PI.half()));
		this.assert(Kernel.equal_question_(Kernel.angle(Complex.makeInstance(1,1)), PI.half().half()));
		this.assert(Kernel.equal_question_(Kernel.angle(FloatPoint.makeInstance(-1)), PI));
		this.assert(Kernel.equal_question_(Kernel.angle(Complex.makeInstance(-1, 1)), PI.multiply(FloatPoint.makeInstance(0.75))));
		this.assert(Kernel.equal_question_(Kernel.angle(Complex.makeInstance(-1, -1)), PI.multiply(FloatPoint.makeInstance(-0.75))));
		this.assert(Kernel.equal_question_(Kernel.angle(Complex.makeInstance(1, -1)), PI.half().half().minus()));
	    },
	    
	    testExp : function(){
		this.assert(Kernel._equal_(Kernel.exp(Rational.ZERO), Rational.ONE, []));
		this.assert(Kernel._equal_(Kernel.exp(Rational.ONE),
					   Kernel.e, []));
		this.assert(Kernel._equal__tilde_(Kernel.exp(Rational.makeInstance(2)), 
						  Kernel.sqr(Kernel.e),
						  FloatPoint.makeInstance(0.0001)));
	    },
	    
	    
	    testExpt : function(){
		//var i = plt.types.Complex.makeInstance(0, 1);
		//this.assert(Kernel.equal_question_(Kernel.expt(i, i), Kernel.exp(PI.half().minus())));
		this.assert(Kernel.equal_question_(Kernel.expt(FloatPoint.makeInstance(2), FloatPoint.makeInstance(3)), FloatPoint.makeInstance(8)));
	    },
	    
	    
	    testSin : function(){
		this.assert(Kernel.equal_question_(Kernel.sin(PI.divide(FloatPoint.makeInstance(2))), Rational.ONE));
	    },
	    
	    testCos : function(){
		this.assert(Kernel.equal_question_(Kernel.cos(Rational.ZERO), Rational.ONE));
	    },
	    

	    testSqr: function() {
		var n1 = Rational.makeInstance(42);
		this.assertEqual(1764, Kernel.sqr(n1).toInteger());
		this.assertRaise("MobyTypeError",
				 function() { Kernel.sqr("42"); });
	    },

	    testIntegerSqrt: function() {
		var n1 = Rational.makeInstance(36);
		var n2 = Rational.makeInstance(6);
		
		this.assertEqual(n2, Kernel.integer_dash_sqrt(n1));
		this.assertRaise("MobyTypeError", function() { Kernel.integer_dash_sqrt(FloatPoint.makeInstance(3.5)); }); 
	    },


	    testSqrt : function(){
		this.assert(Kernel.equal_question_(Kernel.sqrt(FloatPoint.makeInstance(4)), FloatPoint.makeInstance(2)));
		this.assert(Kernel.equal_question_(Kernel.sqrt(FloatPoint.makeInstance(-1)), Complex.makeInstance(0,1)));
	    },
	    
	    testAcos : function(){
		this.assert(Kernel.equal_question_(Kernel.acos(Rational.ONE), Rational.ZERO));
		this.assert(Kernel.equal_question_(Kernel.acos(FloatPoint.makeInstance(-1)), PI));
	    },
	    
	    testAsin : function(){
		this.assert(Kernel.equal_question_(Kernel.asin(Rational.ZERO), Rational.ZERO));
		this.assert(Kernel.equal_question_(Kernel.asin(Rational.ONE.minus()), PI.half().minus()));
		this.assert(Kernel.equal_question_(Kernel.asin(Rational.ONE), PI.half()));
	    },
	    
	    testTan : function(){
		this.assert(Kernel.equal_question_(Kernel.tan(Rational.ZERO), Rational.ZERO));
	    },
	    
	    testComplex_question_ : function(){
		this.assert(Kernel.complex_question_(PI));
		this.assert(Kernel.complex_question_(Rational.ONE));
		this.assert(Kernel.complex_question_(FloatPoint.makeInstance(2.718)));
		this.assert(Kernel.complex_question_(Complex.makeInstance(0,1)));
		this.assert(!Kernel.complex_question_(plt.types.Empty.EMPTY));
		this.assert(!Kernel.complex_question_(String.makeInstance("hi")));
		this.assert(!Kernel.complex_question_(Symbol.makeInstance('h')));
	    },



	    testMakePolar : function() {
		this.assert(Kernel.equal_question_(Kernel.make_dash_polar(Rational.makeInstance(5),
									  Rational.makeInstance(0)),
						   Complex.makeInstance(5, 0)));
		var n = Kernel.make_dash_polar(Rational.makeInstance(5),
					       PI);
		var delta = FloatPoint.makeInstance(0.0000001);
		this.assert(Kernel._equal__tilde_(Kernel.imag_dash_part(n),
						  Rational.makeInstance(0),
						  delta));
		this.assert(Kernel._equal__tilde_(Kernel.real_dash_part(n),
						  Rational.makeInstance(-5),
						  delta));
	    },
	    
	    
	    testMakeRectangular: function() {
		this.assert(Kernel.equal_question_(Kernel.make_dash_rectangular
						   (Rational.makeInstance(4),
						    Rational.makeInstance(3)),
						   Complex.makeInstance(4, 3)));		
		this.assert(Kernel.equal_question_(Kernel.make_dash_rectangular
						   (Rational.makeInstance(5),
						    Rational.makeInstance(4)),
						   Complex.makeInstance(5, 4)));
	    },

	    
	    testCosh : function(){
		this.assert(Kernel.equal_question_(Kernel.cosh(Rational.ZERO), Rational.ONE));
	    },
	    
	    testSinh : function(){
		this.assert(Kernel.equal_question_(Kernel.sinh(Rational.ZERO), Rational.ZERO));
	    },
	    
	    testDenominator : function(){
		this.assert(Kernel.equal_question_(Kernel.denominator(Rational.makeInstance(7,2)), Rational.makeInstance(2,1)));
		this.assertRaise("MobyTypeError",
				 function() { Kernel.denominator(FloatPoint.makeInstance(3)); });
	    },
	    
	    testNumerator : function(){
		this.assert(Kernel.equal_question_(Kernel.numerator(Rational.makeInstance(7,2)), Rational.makeInstance(7,1)));
		this.assertRaise("MobyTypeError",
				 function() { Kernel.numerator(FloatPoint.makeInstance(3)); });
	    },


	    testIsExact : function() {
		this.assert(Kernel.exact_question_(Rational.makeInstance(3)));
		this.assert(! Kernel.exact_question_(FloatPoint.makeInstance(3.0)));
		this.assert(! Kernel.exact_question_(FloatPoint.makeInstance(3.5)));
	    },

	    testIsInexact : function() {
		this.assert(! Kernel.inexact_question_(Rational.makeInstance(3)));
		this.assert(Kernel.inexact_question_(FloatPoint.makeInstance(3.0)));
		this.assert(Kernel.inexact_question_(FloatPoint.makeInstance(3.5)));
	    },



	    testExactToInexact : function() {
		this.assert(Kernel._equal_(Kernel.exact_dash__greaterthan_inexact(Rational.makeInstance(3)),
					   FloatPoint.makeInstance(3.0),
					   []));
		this.assert(Kernel.inexact_question_(Kernel.exact_dash__greaterthan_inexact(Rational.makeInstance(3))));
	    },


	    testInexactToExact : function() {
		this.assert(Kernel._equal_(Kernel.inexact_dash__greaterthan_exact(FloatPoint.makeInstance(3)),
					   Rational.makeInstance(3),
					   []));
		this.assert(Kernel.exact_question_(Kernel.inexact_dash__greaterthan_exact(FloatPoint.makeInstance(3))));
	    },

	    testFloatsAreInexact: function() {
		this.assert(! Kernel.exact_question_(FloatPoint.makeInstance(3.0)));
	    },

	    
	    testOdd_question_ : function(){
		this.assert(Kernel.odd_question_(Rational.ONE));
		this.assert(! Kernel.odd_question_(Rational.ZERO));
		this.assert(Kernel.odd_question_(FloatPoint.makeInstance(1)));
		this.assert(Kernel.odd_question_(Complex.makeInstance(1, 0)));
		this.assert(Kernel.odd_question_(Rational.makeInstance(-1, 1)));
	    },
	    
	    testInfinityComputations : function() {
		this.assert(Kernel._equal_(Rational.ZERO,
					   Kernel._star_([Rational.ZERO, 
							  FloatPoint.makeInstance(Number.POSITIVE_INFINITY)]),
					   []));
	    },

	    testEven_question_ : function(){
		this.assert(Kernel.even_question_(Rational.ZERO));
		this.assert(! Kernel.even_question_(Rational.ONE));
		this.assert(Kernel.even_question_(FloatPoint.makeInstance(2)));
		this.assert(Kernel.even_question_(Complex.makeInstance(2, 0)));
	    },
	    
	    testPositive_question_ : function(){
		this.assert(Kernel.positive_question_(Rational.ONE));
		this.assert(!Kernel.positive_question_(Rational.ZERO));
		this.assert(Kernel.positive_question_(FloatPoint.makeInstance(1.1)));
		this.assert(Kernel.positive_question_(Complex.makeInstance(1,0)));
	    },
	    
	    testNegative_question_ : function(){
		this.assert(Kernel.negative_question_(Rational.makeInstance(-5)));
		this.assert(!Kernel.negative_question_(Rational.ONE));
		this.assert(!Kernel.negative_question_(Rational.ZERO));
		this.assert(!Kernel.negative_question_(FloatPoint.makeInstance(1.1)));
		this.assert(!Kernel.negative_question_(Complex.makeInstance(1,0)));
	    },
	    
	    testCeiling : function(){
		this.assert(Kernel.equal_question_(Kernel.ceiling(Rational.ONE), Rational.ONE));
		this.assert(Kernel.equal_question_(Kernel.ceiling(PI), FloatPoint.makeInstance(4)));
		this.assert(Kernel.equal_question_(Kernel.ceiling(Complex.makeInstance(3.1,0)), FloatPoint.makeInstance(4)));
	    },
	    
	    testFloor : function(){
		this.assert(Kernel.equal_question_(Kernel.floor(Rational.ONE), Rational.ONE));
		this.assert(Kernel.equal_question_(Kernel.floor(PI), FloatPoint.makeInstance(3)));
		this.assert(Kernel.equal_question_(Kernel.floor(Complex.makeInstance(3.1,0)), FloatPoint.makeInstance(3)));
	    },
	    
	    testImag_dash_part : function(){
		this.assert(Kernel.equal_question_(Kernel.imag_dash_part(Rational.ONE), Rational.ZERO));
		this.assert(Kernel.equal_question_(Kernel.imag_dash_part(PI), Rational.ZERO));
		this.assert(Kernel.equal_question_(Kernel.imag_dash_part(Complex.makeInstance(0,1)), Rational.ONE));
	    },
	    
	    testReal_dash_part : function(){
		this.assert(Kernel.equal_question_(Kernel.real_dash_part(Rational.ONE), Rational.ONE));
		this.assert(Kernel.equal_question_(Kernel.real_dash_part(PI), PI));
		this.assert(Kernel.equal_question_(Kernel.real_dash_part(Complex.makeInstance(0,1)), Rational.ZERO));
	    },
	    
	    testInteger_question_ : function(){
		this.assert(Kernel.integer_question_(Rational.ONE));
		this.assert(Kernel.integer_question_(FloatPoint.makeInstance(3.0)));
		this.assert(!Kernel.integer_question_(FloatPoint.makeInstance(3.1)));
		this.assert(Kernel.integer_question_(Complex.makeInstance(3,0)));
		this.assert(!Kernel.integer_question_(Complex.makeInstance(3.1,0)));
	    },
	    
	    testMake_dash_rectangular: function(){
		this.assert(Kernel.equal_question_(Kernel.make_dash_rectangular(Rational.ONE, Rational.ONE), Complex.makeInstance(1,1)));
	    },
	    
	    testMaxAndMin : function(){
		var n1 = FloatPoint.makeInstance(-1);
		var n2 = Rational.ZERO;
		var n3 = Rational.ONE;
		var n4 = Complex.makeInstance(4,0);
		this.assert(Kernel.equal_question_(n4, Kernel.max(n1, [n2,n3,n4])));
		this.assert(Kernel.equal_question_(n1, Kernel.min(n1, [n2,n3,n4])));

		var n5 = FloatPoint.makeInstance(1.1);
		this.assertEqual(n5, Kernel.max(n1, [n2, n3, n5]));
		this.assertEqual(n1, Kernel.min(n2, [n3, n4, n5, n1]));
	    },

	    testLcm : function () {
		this.assert(Kernel.equal_question_(Rational.makeInstance(12),
						   Kernel.lcm(Rational.makeInstance(1),
							      [Rational.makeInstance(2), Rational.makeInstance(3), Rational.makeInstance(4)])));
	    },

	    testGcd : function () {
		this.assert(Kernel.equal_question_(Rational.makeInstance(1),
						   Kernel.gcd(Rational.makeInstance(1),
							      [Rational.makeInstance(2), Rational.makeInstance(3), Rational.makeInstance(4)])));

		this.assert(Kernel.equal_question_(Rational.makeInstance(5),
						   Kernel.gcd(Rational.makeInstance(100),
							      [Rational.makeInstance(5), Rational.makeInstance(10), Rational.makeInstance(25)])));
	    },


	    testIsRational : function() {
		this.assert(Kernel.rational_question_(Rational.makeInstance(42)));
		this.assert(! Kernel.rational_question_(FloatPoint.makeInstance(3.1415)));
		this.assert(! Kernel.rational_question_("blah"));
	    },	

	    
	    testNumberQuestion : function() {
		this.assert(Kernel.number_question_(plt.types.Rational.makeInstance(42)));
		this.assert(Kernel.number_question_(42) == false);
	    },


	    testNumber_dash__greaterthan_string : function(){
		this.assert(Kernel.string_equal__question_(String.makeInstance("1"), Kernel.number_dash__greaterthan_string(Rational.ONE),[]));
		this.assert(!Kernel.string_equal__question_(String.makeInstance("2"), Kernel.number_dash__greaterthan_string(Rational.ONE),[]));
	    },
	    
	    testString_equal__question_	: function(){
		var s1 = String.makeInstance("hi");
		var s2 = String.makeInstance("hi");
		var s3 = String.makeInstance("hi");
		var s4 = String.makeInstance("hi2");
		this.assert(Kernel.string_equal__question_(s1, s2, [s3]));
		this.assert(!Kernel.string_equal__question_(s1,s2,[s4]));
	    },
	    
	    testString_lessthan__equal__question_: function(){
		var s1 = String.makeInstance("hi");
		var s2 = String.makeInstance("hi");
		var s3 = String.makeInstance("hi2");
		var s4 = String.makeInstance("a");
		this.assert(Kernel.string_lessthan__equal__question_(s1, s2, [s3]));
		this.assert(!Kernel.string_lessthan__equal__question_(s1,s2,[s4]));
	    },
	    
	    testString_lessthan__question_: function(){
		var s1 = String.makeInstance("ha");
		var s2 = String.makeInstance("hi");
		var s3 = String.makeInstance("hi2");
		var s4 = String.makeInstance("hi");
		this.assert(Kernel.string_lessthan__question_(s1, s2, [s3]));
		this.assert(!Kernel.string_lessthan__question_(s1,s2,[s4]));
	    },
	    
	    testString_greaterthan__equal__question_: function(){
		var s1 = String.makeInstance("hi");
		var s2 = String.makeInstance("ha");
		var s3 = String.makeInstance("ha");
		var s4 = String.makeInstance("hi");
		this.assert(Kernel.string_greaterthan__equal__question_(s1, s2, [s3]));
		this.assert(!Kernel.string_greaterthan__equal__question_(s1,s2,[s4]));
	    },
	    
	    testString_greaterthan__question_: function(){
		var s1 = String.makeInstance("hi");
		var s2 = String.makeInstance("hb");
		var s3 = String.makeInstance("ha");
		var s4 = String.makeInstance("hb");
		this.assert(Kernel.string_greaterthan__question_(s1, s2, [s3]));
		this.assert(!Kernel.string_greaterthan__question_(s1,s2,[s4]));
	    },
	    
	    testQuotient : function(){
		this.assert(Kernel.equal_question_(Kernel.quotient(FloatPoint.makeInstance(3), FloatPoint.makeInstance(4)), Rational.ZERO));	
		this.assert(Kernel.equal_question_(Kernel.quotient(FloatPoint.makeInstance(4), FloatPoint.makeInstance(3)), Rational.ONE));

		this.assert(Kernel.equal_question_(
		    Kernel.quotient(Rational.makeInstance(-36),
				    Rational.makeInstance(7)),
		    Rational.makeInstance(-5)));


		this.assert(Kernel.equal_question_(
		    Kernel.quotient(Rational.makeInstance(-36),
				    Rational.makeInstance(-7)),
		    Rational.makeInstance(5)));


		this.assert(Kernel.equal_question_(
		    Kernel.quotient(Rational.makeInstance(36),
				    Rational.makeInstance(-7)),
		    Rational.makeInstance(-5)));


		this.assert(Kernel.equal_question_(
		    Kernel.quotient(Rational.makeInstance(36),
				    Rational.makeInstance(7)),
		    Rational.makeInstance(5)));

		

	    },
	    
	    testRemainder : function(){
		this.assert(Kernel.equal_question_(Kernel.remainder(FloatPoint.makeInstance(3), FloatPoint.makeInstance(4)), FloatPoint.makeInstance(3)));	
		this.assert(Kernel.equal_question_(Kernel.remainder(FloatPoint.makeInstance(4), FloatPoint.makeInstance(3)), FloatPoint.makeInstance(1)));
	    },

	    
	    testModulo : function() {
		var n1 = Rational.makeInstance(17);
		var n2 = Rational.makeInstance(3);
		var n3 = Rational.makeInstance(2);
		this.assertEqual(n3, Kernel.modulo(n1, n2));
		this.assertEqual(n2, Kernel.modulo(n2, n1));
		this.assert(Kernel.equal_question_(
		    Rational.makeInstance(-3), 
		    Kernel.modulo(Rational.makeInstance(13),
				  Rational.makeInstance(-4))));

		this.assert(Kernel.equal_question_(
		    Rational.makeInstance(3), 
		    Kernel.modulo(Rational.makeInstance(-13),
				  Rational.makeInstance(4))));

		this.assert(Kernel.equal_question_(
		    Rational.makeInstance(-1), 
		    Kernel.modulo(Rational.makeInstance(-13),
				  Rational.makeInstance(-4))));


		this.assert(Kernel.equal_question_(
		    Rational.makeInstance(0), 
		    Kernel.modulo(Rational.makeInstance(4),
				  Rational.makeInstance(-2))));

	    },

	    
	    testReal_question_ : function(){
		this.assert(Kernel.real_question_(PI));
		this.assert(Kernel.real_question_(Rational.ONE));
		this.assert(!Kernel.real_question_(Complex.makeInstance(0,1)));
		this.assert(Kernel.real_question_(Complex.makeInstance(1,0)));
		this.assert(!Kernel.real_question_(plt.types.Empty.EMPTY));
		this.assert(!Kernel.real_question_(String.makeInstance("hi")));
		this.assert(!Kernel.real_question_(Symbol.makeInstance('h')));
	    },
	    
	    testRound : function(){
		this.assert(Kernel.equal_question_(Kernel.round(FloatPoint.makeInstance(3.499999)), 
						   FloatPoint.makeInstance(3)));
		this.assert(Kernel.equal_question_(Kernel.round(FloatPoint.makeInstance(3.5)), 
						   FloatPoint.makeInstance(4)));
		this.assert(Kernel.equal_question_(Kernel.round(FloatPoint.makeInstance(3.51)),
						   FloatPoint.makeInstance(4)));
		this.assert(Kernel.equal_question_(Kernel.round(Rational.makeInstance(3)),
						   Rational.makeInstance(3)));

		this.assert(Kernel.equal_question_(Kernel.round(Rational.makeInstance(17, 4)),
						   Rational.makeInstance(4)));


		this.assert(Kernel.equal_question_(Kernel.round(Rational.makeInstance(-17, 4)),
						   Rational.makeInstance(-4)));
	    },
	    
	    testSgn : function(){
		this.assert(Kernel.equal_question_(Kernel.sgn(FloatPoint.makeInstance(4)), Rational.ONE));
		this.assert(Kernel.equal_question_(Kernel.sgn(FloatPoint.makeInstance(-4)), Rational.NEGATIVE_ONE));
		this.assert(Kernel.equal_question_(Kernel.sgn(Rational.ZERO), Rational.ZERO));
	    },
	    
	    testZero_question_ : function(){
		this.assert(Kernel.zero_question_(Rational.ZERO));
		this.assert(!Kernel.zero_question_(Rational.ONE));
		this.assert(Kernel.zero_question_(Complex.makeInstance(0,0)));
	    },
	    
	    testBoolean_equal__question_ : function(){
		this.assert(Kernel.boolean_equal__question_(Logic.TRUE, Logic.TRUE));
		this.assert(!Kernel.boolean_equal__question_(Logic.TRUE, Logic.FALSE));
		this.assert(Kernel.boolean_equal__question_(Logic.FALSE, Logic.FALSE));	
		this.assertRaise("MobyTypeError",
				 function() {
				     Kernel.boolean_equal__question_(Logic.TRUE, "true");
				 });
		this.assertRaise("MobyTypeError",
				 function() {
				     Kernel.boolean_equal__question_("true", Logic.TRUE);
				 });
		
	    },
	    
	    testBoolean_question_ : function(){
		this.assert(Kernel.boolean_question_(Logic.TRUE));
		this.assert(Kernel.boolean_question_(Logic.FALSE));
		this.assert(!Kernel.boolean_question_(PI));
	    },
	    
	    testFalse_question_: function(){
		this.assert(Kernel.false_question_(Logic.FALSE));
		this.assert(!Kernel.false_question_(Logic.TRUE));
		this.assert(!Kernel.false_question_(PI));
	    },
	    
	    testNot : function(){
		this.assert(Kernel.not(Logic.FALSE).valueOf());
		this.assert(!Kernel.not(Logic.TRUE).valueOf());
	    },
	    
	    testSymbol_dash__greaterthan_string : function(){
		this.assert(Kernel.string_equal__question_(Kernel.symbol_dash__greaterthan_string(Symbol.makeInstance("ha")), String.makeInstance("ha"), []));
	    },
	    
	    testSymbol_equal__question_ : function(){
		this.assert(Kernel.symbol_equal__question_(Symbol.makeInstance("hi"), Symbol.makeInstance("hi")));
		this.assert(!Kernel.symbol_equal__question_(Symbol.makeInstance("hi"), Symbol.makeInstance("hi1")));
	    },
	    
	    testSymbol_question_ : function(){
		this.assert(Kernel.symbol_question_(Symbol.makeInstance("hi")));
		this.assert(!Kernel.symbol_question_(String.makeInstance("hi")));
	    },
	    
	    testEmpty_question_ : function(){
		this.assert(Kernel.empty_question_(Empty.EMPTY));
		this.assert(!Kernel.empty_question_(Cons.makeInstance(Rational.ONE, Empty.EMPTY)));
	    },
	    
	    testReverse : function(){
		var lst1 = Cons.makeInstance(FloatPoint.makeInstance(1), Cons.makeInstance(FloatPoint.makeInstance(2),
											   Cons.makeInstance(FloatPoint.makeInstance(3), Cons.makeInstance(FloatPoint.makeInstance(4), EMPTY))));
		var lst2 = Cons.makeInstance(FloatPoint.makeInstance(4), Cons.makeInstance(FloatPoint.makeInstance(3),
											   Cons.makeInstance(FloatPoint.makeInstance(2), Cons.makeInstance(FloatPoint.makeInstance(1), EMPTY))));
		var lst3 = Kernel.reverse(lst1);
		while (!lst2.isEmpty()){
		    this.assert(Kernel.equal_question_(lst2.first(), lst3.first()));
		    lst2 = lst2.rest();
		    lst3 = lst3.rest();
		}
	    },
	    
	    testAppend : function(){		
		var lst1 = Cons.makeInstance(FloatPoint.makeInstance(1), Cons.makeInstance(FloatPoint.makeInstance(2), EMPTY));
		var lst2 = Cons.makeInstance(FloatPoint.makeInstance(3), Cons.makeInstance(FloatPoint.makeInstance(4), EMPTY));
		var lst3 = Cons.makeInstance(FloatPoint.makeInstance(5), Cons.makeInstance(FloatPoint.makeInstance(6), EMPTY));
		var lst4 = Cons.makeInstance(FloatPoint.makeInstance(7), Cons.makeInstance(FloatPoint.makeInstance(8), EMPTY));
		
		var lst6 = Kernel.append(lst1, [lst2, lst3, lst4]);
		
		var i;
		for (i = 1; i < 9; i++){
		    this.assert(Kernel.equal_question_(lst6.first(), FloatPoint.makeInstance(i), []));
		    lst6 = lst6.rest();
		}
	    },

	    testEq_question_ : function() {
		var a = FloatPoint.makeInstance(2);
		var b = FloatPoint.makeInstance(3);
		var c = FloatPoint.makeInstance(2);
		this.assert(!Kernel.eq_question_(a, b));
		this.assert(Kernel.eq_question_(a,a));
		this.assert(!Kernel.eq_question_(a, c));
	    },
	    
	    
	    testAssq : function(){
		var lst = Cons.makeInstance(Cons.makeInstance(Rational.ONE, EMPTY), Cons.makeInstance(Cons.makeInstance(FloatPoint.makeInstance(2), EMPTY), Cons.makeInstance(Cons.makeInstance(FloatPoint.makeInstance(3), EMPTY), EMPTY)));
		
		this.assert(Kernel.false_question_(Kernel.assq(FloatPoint.makeInstance(4), lst)));
		this.assert(!Kernel.assq(Rational.ONE, lst).isEmpty());
	    },
	    
	    testCaaar : function(){
		var lst = Cons.makeInstance(Cons.makeInstance(Cons.makeInstance(Rational.ONE, EMPTY), EMPTY), EMPTY);
		this.assert(Kernel.equal_question_(Kernel.caaar(lst), FloatPoint.makeInstance(1)));
	    },
	    
	    testCaadr : function(){
		var lst = Cons.makeInstance(Cons.makeInstance(Cons.makeInstance(Rational.ONE, Cons.makeInstance(FloatPoint.makeInstance(2), EMPTY)), EMPTY), EMPTY);
		this.assert(Kernel.equal_question_(Kernel.caadr(lst).first(), FloatPoint.makeInstance(2)));
	    }, 
	    
	    testCaar : function(){
		var lst = Cons.makeInstance(Cons.makeInstance(Rational.ONE, EMPTY), EMPTY)
		this.assert(Kernel.equal_question_(Kernel.caar(lst), FloatPoint.makeInstance(1)));
	    },
	    
	    testCadar : function(){
		var lst = Cons.makeInstance(Cons.makeInstance(FloatPoint.makeInstance(1), Cons.makeInstance(FloatPoint.makeInstance(2, Cons.makeInstance(FloatPoint.makeInstance(3), EMPTY))), EMPTY));
		this.assert(Kernel.equal_question_(Kernel.cadar(lst), FloatPoint.makeInstance(2)));
	    },
	    
	    testCadddr : function(){
		var lst = Cons.makeInstance(FloatPoint.makeInstance(1), Cons.makeInstance(FloatPoint.makeInstance(2), Cons.makeInstance(FloatPoint.makeInstance(3), Cons.makeInstance(FloatPoint.makeInstance(4), EMPTY))));
		this.assert(Kernel.equal_question_(Kernel.cadddr(lst), FloatPoint.makeInstance(4)));
	    },
	    
	    testCaddr : function(){
		var lst = Cons.makeInstance(FloatPoint.makeInstance(1), Cons.makeInstance(FloatPoint.makeInstance(2), Cons.makeInstance(FloatPoint.makeInstance(3), EMPTY)));
		this.assert(Kernel.equal_question_(Kernel.caddr(lst), FloatPoint.makeInstance(3)));
	    },
	    
	    testCadr : function(){
		var lst = Cons.makeInstance(FloatPoint.makeInstance(1), Cons.makeInstance(FloatPoint.makeInstance(2), EMPTY));
		this.assert(Kernel.equal_question_(Kernel.cadr(lst), FloatPoint.makeInstance(2)));
	    },
	    
	    testCar : function(){
		var lst = Cons.makeInstance(FloatPoint.makeInstance(1), EMPTY);
		this.assert(Kernel.equal_question_(Kernel.car(lst), FloatPoint.makeInstance(1)));
	    },
	    
	    testCdaar : function(){
		var lst = Cons.makeInstance(Cons.makeInstance(Cons.makeInstance(FloatPoint.makeInstance(1), Cons.makeInstance(FloatPoint.makeInstance(2), EMPTY)), EMPTY), EMPTY);
		this.assert(Kernel.equal_question_(Kernel.cdaar(lst).first(), FloatPoint.makeInstance(2)));
	    },
	    
	    testCdadr : function(){
		var lst = Cons.makeInstance(FloatPoint.makeInstance(1), Cons.makeInstance(Cons.makeInstance(FloatPoint.makeInstance(2), Cons.makeInstance(FloatPoint.makeInstance(3), EMPTY)), Cons.makeInstance(FloatPoint.makeInstance(4), EMPTY)));
		this.assert(Kernel.equal_question_(Kernel.cdadr(lst).first(), FloatPoint.makeInstance(3)));
	    },
	    
	    testCdar : function(){
		var lst = Cons.makeInstance(Cons.makeInstance(FloatPoint.makeInstance(2), Cons.makeInstance(FloatPoint.makeInstance(3), EMPTY)), EMPTY);
		this.assert(Kernel.equal_question_(Kernel.cdar(lst).first(), FloatPoint.makeInstance(3)));
	    },
	    
	    testCddar : function(){
		var lst = Cons.makeInstance(Cons.makeInstance(FloatPoint.makeInstance(1), Cons.makeInstance(FloatPoint.makeInstance(2), Cons.makeInstance(FloatPoint.makeInstance(3), EMPTY))), EMPTY);
		this.assert(Kernel.equal_question_(Kernel.cddar(lst).first(), FloatPoint.makeInstance(3)));
	    },
	    
	    testCdddr: function(){
		var lst = Cons.makeInstance(FloatPoint.makeInstance(1), Cons.makeInstance(FloatPoint.makeInstance(2), Cons.makeInstance(FloatPoint.makeInstance(3), Cons.makeInstance(FloatPoint.makeInstance(4), EMPTY))));
		this.assert(Kernel.equal_question_(Kernel.cdddr(lst).first(), FloatPoint.makeInstance(4)));
	    },
	    
	    testCddr : function(){
		var lst = Cons.makeInstance(FloatPoint.makeInstance(1), Cons.makeInstance(FloatPoint.makeInstance(2), Cons.makeInstance(FloatPoint.makeInstance(3), EMPTY)));
		this.assert(Kernel.equal_question_(Kernel.cddr(lst).first(), FloatPoint.makeInstance(3)));
	    },
	    
	    testCdr : function(){
		var lst = Cons.makeInstance(FloatPoint.makeInstance(1), Cons.makeInstance(FloatPoint.makeInstance(2), EMPTY));
		this.assert(Kernel.equal_question_(Kernel.cdr(lst).first(), FloatPoint.makeInstance(2)));
	    },
	    
	    testCons_question_ : function(){
		this.assert(Kernel.cons_question_(Cons.makeInstance(Rational.ONE, EMPTY)));
		this.assert(!Kernel.cons_question_(EMPTY));
	    },
	    
	    testSixth : function(){
		var lst = Cons.makeInstance(FloatPoint.makeInstance(1), Cons.makeInstance(FloatPoint.makeInstance(2), Cons.makeInstance(FloatPoint.makeInstance(3), Cons.makeInstance(FloatPoint.makeInstance(4), Cons.makeInstance(FloatPoint.makeInstance(5), Cons.makeInstance(FloatPoint.makeInstance(6), EMPTY))))));
		this.assert(Kernel.equal_question_(Kernel.sixth(lst), FloatPoint.makeInstance(6)));
	    },
	    
	    testSeventh : function(){
		var lst = Cons.makeInstance(FloatPoint.makeInstance(1), Cons.makeInstance(FloatPoint.makeInstance(2), Cons.makeInstance(FloatPoint.makeInstance(3), Cons.makeInstance(FloatPoint.makeInstance(4), Cons.makeInstance(FloatPoint.makeInstance(5), Cons.makeInstance(FloatPoint.makeInstance(6), Cons.makeInstance(FloatPoint.makeInstance(7), EMPTY)))))));
		this.assert(Kernel.equal_question_(Kernel.seventh(lst), FloatPoint.makeInstance(7)));
	    },
	    
	    testEighth : function(){
		var lst = Cons.makeInstance(FloatPoint.makeInstance(1), Cons.makeInstance(FloatPoint.makeInstance(2), Cons.makeInstance(FloatPoint.makeInstance(3), Cons.makeInstance(FloatPoint.makeInstance(4), Cons.makeInstance(FloatPoint.makeInstance(5), Cons.makeInstance(FloatPoint.makeInstance(6), Cons.makeInstance(FloatPoint.makeInstance(7), Cons.makeInstance(FloatPoint.makeInstance(8), EMPTY))))))));
		this.assert(Kernel.equal_question_(Kernel.eighth(lst), FloatPoint.makeInstance(8)));
	    },
	    
	    testLength : function(){
		var lst1 = Cons.makeInstance(FloatPoint.makeInstance(1), Cons.makeInstance(FloatPoint.makeInstance(2), Cons.makeInstance(FloatPoint.makeInstance(3), EMPTY)));
		var lst2 = EMPTY;
		this.assert(Kernel.equal_question_(Kernel.length(lst1), FloatPoint.makeInstance(3)));
		this.assert(Kernel.equal_question_(Kernel.length(lst2), FloatPoint.makeInstance(0)));
	    },
	    
	    testList : function(){
		var arr = [FloatPoint.makeInstance(1), FloatPoint.makeInstance(2)];
		var lst = Kernel.list(arr);
		for (var i = 1; i <= 2; i++){
		    this.assert(Kernel.equal_question_(lst.first(), FloatPoint.makeInstance(i)));
		    lst = lst.rest();
		}
	    },
	    
	    testList_star_simple : function() {
		this.assert(Kernel.equal_question_(Kernel.list_star_(plt.types.String.makeInstance("hello"),
								     [plt.types.Empty.EMPTY]),
						   Kernel.list([plt.types.String.makeInstance("hello")])));
	    },

	    testList_star_ : function(){
		var lst1 = Cons.makeInstance(FloatPoint.makeInstance(3),
					     Cons.makeInstance(FloatPoint.makeInstance(4), EMPTY));
		
		var lst2 = Kernel.list_star_(FloatPoint.makeInstance(1),
					     [FloatPoint.makeInstance(2), lst1]);
		for (var i = 1; i <= 4; i++){
		    this.assert(Kernel.equal_question_(lst2.first(), FloatPoint.makeInstance(i)));
		    lst2 = lst2.rest();
		}
	    },
	    
	    testList_dash_ref : function(){
		var arr = [FloatPoint.makeInstance(2), FloatPoint.makeInstance(3), FloatPoint.makeInstance(4)];
		var lst = Kernel.list(arr);

		this.assert(Kernel.equal_question_(Kernel.list_dash_ref(lst, Rational.makeInstance(2, 1)), FloatPoint.makeInstance(4)));	
	    },
	    
	    testMember : function(){
		var arr = [FloatPoint.makeInstance(2), FloatPoint.makeInstance(3), FloatPoint.makeInstance(4)];
		var lst = Kernel.list(arr);
		this.assert(Kernel.member(FloatPoint.makeInstance(4), lst));
		this.assert(!Kernel.member(FloatPoint.makeInstance(5), lst));
	    },
	    

	    testMemq : function(){
		var a = FloatPoint.makeInstance(3);
		var arr = [FloatPoint.makeInstance(2), a, FloatPoint.makeInstance(4)];
		var lst = Kernel.list(arr);
		this.assert(Kernel.equal_question_(Kernel.memq(a, lst).first(), a));
		this.assert(! Kernel.memq(FloatPoint.makeInstance(5), lst));
		this.assert(! Kernel.memq(FloatPoint.makeInstance(3), lst));
	    },


	    testMemv : function(){
		var a = FloatPoint.makeInstance(3);
		var arr = [FloatPoint.makeInstance(2), a, FloatPoint.makeInstance(4)];
		var lst = Kernel.list(arr);
		this.assert(Kernel.equal_question_(Kernel.memv(FloatPoint.makeInstance(3), lst).first(), a));
		this.assert(!Kernel.memv(FloatPoint.makeInstance(5), lst));
	    },


	    
	    testNull_question_: function(){
		this.assert(!Kernel.null_question_(Rational.ONE));
		this.assert(Kernel.null_question_(EMPTY));
	    },
	    
	    testPair_question_ : function(){
		this.assert(!Kernel.pair_question_(EMPTY));
		this.assert(Kernel.pair_question_(Cons.makeInstance(1, EMPTY)));
	    },
	    
	    
	    testString_dash__greaterthan_number : function(){
		this.assert(!Kernel.string_dash__greaterthan_number("hi"));
		this.assert(Kernel.equal_question_(Kernel.string_dash__greaterthan_number("5"), FloatPoint.makeInstance(5)));
	    },
	    
	    testString_dash__greaterthan_symbol : function(){
		this.assert(Kernel.symbol_equal__question_(Kernel.string_dash__greaterthan_symbol(String.makeInstance("hi")), Symbol.makeInstance("hi")));
		this.assert(!Kernel.symbol_equal__question_(Kernel.string_dash__greaterthan_symbol(String.makeInstance("hi")), Symbol.makeInstance("hi5")));
	    },
	    
	    testString_dash_append : function(){
		var arr = [String.makeInstance("hello"),
			   String.makeInstance("zhe"),
			   String.makeInstance("zhang")];
		var str = Kernel.string_dash_append(arr);

		//		this.assert(Kernel.string_equal__question_(str, String.makeInstance("hellozhezhang"), []));
		//		this.assert(!Kernel.string_equal__question_(str, String.makeInstance("hellozhezhang1"), []));
	    },
	    
	    testString_dash_ci_equal__question_ : function(){
		this.assert(Kernel.string_dash_ci_equal__question_(String.makeInstance("hi"), String.makeInstance("Hi"), []));
		this.assert(!Kernel.string_dash_ci_equal__question_(String.makeInstance("hi"), String.makeInstance("Hi1"), []));
	    },
	    
	    testString_dash_ci_lessthan__equal__question_ : function(){
		this.assert(Kernel.string_dash_ci_lessthan__equal__question_(String.makeInstance("hi"), String.makeInstance("Hi1"), []));
		
		this.assert(Kernel.string_dash_ci_lessthan__equal__question_(String.makeInstance("hi"), String.makeInstance("Hi"), []));
		this.assert(!Kernel.string_dash_ci_lessthan__equal__question_(String.makeInstance("hi1"), String.makeInstance("Hi"), []));	
	    },
	    
	    testString_dash_ci_lessthan__question_ : function(){
		this.assert(Kernel.string_dash_ci_lessthan__question_(String.makeInstance("hi"), String.makeInstance("Hi1"), []));
		
		this.assert(!Kernel.string_dash_ci_lessthan__question_(String.makeInstance("hi"), String.makeInstance("Hi"), []));
		this.assert(!Kernel.string_dash_ci_lessthan__question_(String.makeInstance("hi1"), String.makeInstance("Hi"), []));	
	    },
	    
	    testString_dash_copy : function(){
		var str1 = String.makeInstance("hi");
		var str2 = Kernel.string_dash_copy(str1);
		this.assert(Kernel.string_equal__question_(str1, str2, []));
		// FLAW: javascript strings compare with == regardless
		// of location.  It may be unnecessary to test for
		// eq-ness of strings: in beginner student level,
		// strings are immutable already.
		// this.assert(!Kernel.eq_question_(str1, str2));

	    },
	    
	    testString_dash_length : function(){
		this.assert(Kernel.equal_question_(Kernel.string_dash_length(String.makeInstance("")), Rational.ZERO));
		this.assert(Kernel.equal_question_(Kernel.string_dash_length(String.makeInstance("hi")), Rational.makeInstance(2)));
	    },


	    testString_dash_ith : function() {
		var s = String.makeInstance("haruhi");
		this.assertEqual('h', Kernel.string_dash_ith(s, Rational.makeInstance(0)));
		this.assertEqual('a', Kernel.string_dash_ith(s, Rational.makeInstance(1)));
		this.assertEqual('r', Kernel.string_dash_ith(s, Rational.makeInstance(2)));
		this.assertEqual('u', Kernel.string_dash_ith(s, Rational.makeInstance(3)));
		this.assertEqual('h', Kernel.string_dash_ith(s, Rational.makeInstance(4)));
		this.assertEqual('i', Kernel.string_dash_ith(s, Rational.makeInstance(5)));
		
		this.assertRaise("MobyRuntimeError", 
				 function() { Kernel.string_dash_ith
					      (s, Rational.makeInstance(6)) });
	    },
	    
	    testString_dash_ref : function(){
		var zhe = String.makeInstance("zhe");
		var i = FloatPoint.makeInstance(2);
		this.assert(Kernel.string_equal__question_(String.makeInstance("e"), 
							   Kernel.string_dash_ref(zhe, i), []));
		this.assertRaise("MobyTypeError",
				 function() { Kernel.string_dash_ref
					      (zhe, Rational.makeInstance(-1)) });
		this.assertRaise("MobyRuntimeError",
				 function() { Kernel.string_dash_ref
					      (zhe, Rational.makeInstance(4)) });
	    }, 
	    

	    testReplicate : function() {
		this.assertEqual(String.makeInstance("hihihi"),
				 Kernel.replicate(Rational.makeInstance(3),
						  String.makeInstance("hi")));
	    },

	    testIntToString: function() {
		this.assertEqual(String.makeInstance("d"),
				 Kernel.int_dash__greaterthan_string(Rational.makeInstance(100)));
	    },

	    testStringToInt: function() {
		this.assert(Kernel.equal_question_
			    (Rational.makeInstance(100),
			     Kernel.string_dash__greaterthan_int(String.makeInstance("d"))));
	    },


	    testExplode: function() {
		this.assert(
		    Kernel.equal_question_(
			Kernel.list([String.makeInstance('b'),
				     String.makeInstance('o'),
				     String.makeInstance('o'),
				     String.makeInstance('m'),
				     String.makeInstance('!')]),
			Kernel.explode(String.makeInstance("boom!"))));
	    },


	    testImplode: function() {
		this.assert(
		    Kernel.equal_question_(
			String.makeInstance("floop!"),
			Kernel.implode(Kernel.list([String.makeInstance('f'),
						    String.makeInstance('l'),
						    String.makeInstance('o'),
						    String.makeInstance('o'),
						    String.makeInstance('p'),
						    String.makeInstance('!')]))));
	    },


	    
	    testString_question_ : function(){
		this.assert(! Kernel.string_question_(Rational.ONE));
		this.assert(Kernel.string_question_(String.makeInstance("hi")));
	    },
	    

	    testStringIsNumeric : function() {
		this.assert(Kernel.string_dash_numeric_question_(String.makeInstance("12345")));
		this.assert(! Kernel.string_dash_numeric_question_(String.makeInstance("xxx")));
		this.assert(! Kernel.string_dash_numeric_question_(String.makeInstance("12xxx34")));
	    },


	    testStringIsAlphabetic : function() {
		this.assert(! Kernel.string_dash_alphabetic_question_(String.makeInstance("12345")));
		this.assert(Kernel.string_dash_alphabetic_question_(String.makeInstance("xxx")));
		this.assert(! Kernel.string_dash_alphabetic_question_(String.makeInstance("12xxx34")));
	    },


	    testStringIsWhitespace : function() {
		this.assert(! Kernel.string_dash_whitespace_question_(String.makeInstance("12 345")));
		this.assert(Kernel.string_dash_whitespace_question_(String.makeInstance("   ")));
		this.assert(Kernel.string_dash_whitespace_question_(String.makeInstance(" ")));
		this.assert(! Kernel.string_dash_whitespace_question_(String.makeInstance("   12xxx34")));
	    },

	    testStringIsLowerCase : function() {
		this.assert(Kernel.string_dash_lower_dash_case_question_(String.makeInstance("hello")));
		this.assert(!Kernel.string_dash_lower_dash_case_question_(String.makeInstance("Hello")));
		this.assert(!Kernel.string_dash_lower_dash_case_question_(String.makeInstance("hello1")));
	    },


	    testStringIsUpperCase : function() {
		this.assert(Kernel.string_dash_upper_dash_case_question_(String.makeInstance("HELLO")));
		this.assert(!Kernel.string_dash_upper_dash_case_question_(String.makeInstance("HELLo")));
		this.assert(!Kernel.string_dash_upper_dash_case_question_(String.makeInstance("HELLO1")));
	    },


	    testString: function() {
		this.assertEqual("", Kernel.string([]));
		this.assertEqual("abc", Kernel.string([Char.makeInstance('a'), 
						       Char.makeInstance('b'),
						       Char.makeInstance('c')]));
	    },

	    testSubstring : function(){
		var str1 = String.makeInstance("he");
		var str2 = String.makeInstance("hello");
		var str3 = Kernel.substring(str2, 
					    Rational.makeInstance(0), 
					    Rational.makeInstance(2));
		this.assert(Kernel.string_equal__question_(str1, str3, []));
		
		this.assertEqual(String.makeInstance(""),
				 Kernel.substring(String.makeInstance("foobar"), Rational.makeInstance(3), Rational.makeInstance(3)));
		this.assertEqual(String.makeInstance("b"),
				 Kernel.substring(String.makeInstance("foobar"), Rational.makeInstance(3), Rational.makeInstance(4)));
		this.assertEqual(String.makeInstance("ba"),
				 Kernel.substring(String.makeInstance("foobar"), Rational.makeInstance(3), Rational.makeInstance(5)));
		this.assertEqual(String.makeInstance("bar"),
				 Kernel.substring(String.makeInstance("foobar"), Rational.makeInstance(3), Rational.makeInstance(6)));
	    },
	    
	    testChar_dash__greaterthan_integer : function(){	
		this.assert(Kernel.equal_question_(FloatPoint.makeInstance(101), Kernel.char_dash__greaterthan_integer(Char.makeInstance("e"))));	
	    },
	    
	    testInteger_dash__greaterthan_char : function(){
		this.assert(Kernel.equal_question_(Char.makeInstance("e"), Kernel.integer_dash__greaterthan_char(Rational.makeInstance(101, 1))));
	    },

	    testRandom : function() {
		this.assert(Kernel.random(Rational.makeInstance(5)).toInteger() < 5);	
		this.assertRaise("MobyTypeError", function() { Kernel.random(42) });
	    },

	    testCurrentSeconds : function() {
		var n1 = Kernel.current_dash_seconds();
		var n2 = Kernel.current_dash_seconds();
		this.assert(n1.toInteger() <= n2.toInteger());
	    },

	    
	    testChar_dash_alphabetic_question_ : function(){
		this.assert(Kernel.char_dash_alphabetic_question_(Char.makeInstance("e")));
		this.assert(!Kernel.char_dash_alphabetic_question_(Char.makeInstance("1")));
	    },
	    
	    testChar_equal__question_ : function(){
		var s1 = Char.makeInstance("h");
		var s2 = Char.makeInstance("h");
		var s3 = Char.makeInstance("h");
		var s4 = Char.makeInstance("g");
		this.assert(Kernel.char_equal__question_(s1, s2, [s3]));
		this.assert(! Kernel.char_equal__question_(s1, s2, [s4]));
	    },
	    
	    testChar_lessthan__question_ : function(){
		var s1 = Char.makeInstance("h");
		var s2 = Char.makeInstance("g");
		var s3 = Char.makeInstance("g");
		this.assert(Kernel.char_lessthan__question_(s2, s1, []));
		this.assert(!Kernel.char_lessthan__question_(s2, s3, []));
	    },
	    
	    testChar_lessthan__equal__question_ : function(){
		var s1 = Char.makeInstance("h");
		var s2 = Char.makeInstance("g");
		var s3 = Char.makeInstance("g");
		this.assert(Kernel.char_lessthan__equal__question_(s2, s1, []));
		this.assert(Kernel.char_lessthan__equal__question_(s2, s3, []));
		this.assert(!Kernel.char_lessthan__equal__question_(s1, s2, []));
	    },
	    
	    testChar_dash_ci_equal__question_ : function(){
		var s1 = Char.makeInstance("h");
		var s2 = Char.makeInstance("H");
		var s3 = Char.makeInstance("g");
		this.assert(Kernel.char_dash_ci_equal__question_(s1, s2, []));
		this.assert(!Kernel.char_dash_ci_equal__question_(s1, s3, []));
	    },
	    
	    testChar_dash_ci_lessthan__question_ : function(){
		var s1 = Char.makeInstance("h");
		var s2 = Char.makeInstance("H");
		var s3 = Char.makeInstance("g");
		this.assert(Kernel.char_dash_ci_lessthan__question_(s3, s1, []));
		this.assert(Kernel.char_dash_ci_lessthan__question_(s3, s2, []));
		this.assert(!Kernel.char_dash_ci_lessthan__question_(s3, s3, []));
		this.assert(!Kernel.char_dash_ci_lessthan__question_(s1, s3, []));
	    },
	    
	    testChar_dash_ci_lessthan__equal__question_ : function(){
		var s1 = Char.makeInstance("h");
		var s2 = Char.makeInstance("H");
		var s3 = Char.makeInstance("g");
		this.assert(Kernel.char_dash_ci_lessthan__equal__question_(s3, s1, []));
		this.assert(Kernel.char_dash_ci_lessthan__equal__question_(s3, s2, []));
		this.assert(Kernel.char_dash_ci_lessthan__equal__question_(s3, s3, []));
		this.assert(!Kernel.char_dash_ci_lessthan__equal__question_(s1, s3, []));
	    },
	    
	    
	    testChar_dash_lower_dash_case_question_	: function(){
		var c1 = Char.makeInstance("h");
		var c2 = Char.makeInstance("H");
		var c3 = Char.makeInstance("1");
		this.assert(Kernel.char_dash_lower_dash_case_question_(c1));
		this.assert(!Kernel.char_dash_lower_dash_case_question_(c2));
		this.assert(!Kernel.char_dash_lower_dash_case_question_(c3));
	    },
	    
	    testChar_dash_numeric_question_ : function(){
		var c0 = Char.makeInstance("0");
		var c1 = Char.makeInstance("1");
		var ch = Char.makeInstance("h");
		var cO = Char.makeInstance("O");
		this.assert(Kernel.char_dash_numeric_question_(c0));
		this.assert(Kernel.char_dash_numeric_question_(c1));
		this.assert(!Kernel.char_dash_numeric_question_(ch));
		this.assert(!Kernel.char_dash_numeric_question_(cO));
	    },
	    
	    testChar_dash_upcase : function(){
		var s1 = Char.makeInstance("h");
		var s2 = Char.makeInstance("H");
		var s3 = Char.makeInstance("3");
		this.assert(Kernel.char_equal__question_(s2, Kernel.char_dash_upcase(s1),[]));
		this.assert(Kernel.char_equal__question_(s3, Kernel.char_dash_upcase(s3),[]));
	    },

	    testChar_dash_downcase : function(){
		var s1 = Char.makeInstance("h");
		var s2 = Char.makeInstance("H");
		this.assert(Kernel.char_equal__question_(s1, Kernel.char_dash_downcase(s2),[]));
	    },
	    
	    testChar_dash_upper_dash_case_question_	: function(){
		var c1 = Char.makeInstance("h");
		var c2 = Char.makeInstance("H");
		var c3 = Char.makeInstance("1");
		this.assert(Kernel.char_dash_upper_dash_case_question_(c2));
		this.assert(!Kernel.char_dash_upper_dash_case_question_(c1));
		this.assert(!Kernel.char_dash_upper_dash_case_question_(c3));
	    },
	    
	    testChar_dash_whitespace_question_ : function(){
		this.assert(Kernel.char_dash_whitespace_question_(Char.makeInstance(" ")));
		this.assert(!Kernel.char_dash_whitespace_question_(Char.makeInstance("a")));
	    },
	    
	    testList_dash__greaterthan_string : function(){
		var arr = [Char.makeInstance("z"), Char.makeInstance("h"), Char.makeInstance("e")];	
		var lst = Kernel.list(arr);
		var str = Kernel.list_dash__greaterthan_string(lst);
		this.assert(Kernel.equal_question_(str, String.makeInstance("zhe")));
	    },
	    

	    testFormat : function() {
		this.assertEqual("hello danny",
				 Kernel.format("hello ~a", ["danny"]));
		this.assertEqual("hello ethan and \"jeff\"!",
				 Kernel.format("hello ~a and ~s!", ["ethan", "jeff"]));
	    },
	    

	    testMake_dash_string : function(){
		var str = String.makeInstance("zz");
		var c = Char.makeInstance("z");
		var n = Rational.makeInstance(2, 1);
		var str2 = Kernel.make_dash_string(n, c);
		this.assert(Kernel.equal_question_(str, str2));
	    },
	    
	    testString_dash__greaterthan_list : function(){
		var lst = Kernel.string_dash__greaterthan_list(String.makeInstance("zhe"));
		Kernel.equal_question_(lst.first(), Char.makeInstance("z"));
		lst = lst.rest();
		Kernel.equal_question_(lst.first(), Char.makeInstance("h"));
		lst = lst.rest();
		Kernel.equal_question_(lst.first(), Char.makeInstance("e"));
	    },
	    
	    
	    testToWrittenString : function() {
		this.assertEqual(String.makeInstance('hello').toWrittenString(),
				 '"hello"');
		this.assertEqual(Kernel.make_dash_posn(FloatPoint.makeInstance(3),
						       FloatPoint.makeInstance(4)).toWrittenString(),
				 '(make-posn 3 4)');
		this.assertEqual(Logic.TRUE.toWrittenString(), "true");
		this.assertEqual(Logic.FALSE.toWrittenString(), "false");


		this.assertEqual(Cons.makeInstance(Cons.makeInstance(FloatPoint.makeInstance(2),
								     Empty.EMPTY),
						   Empty.EMPTY).toWrittenString(),
				 "((2))");
		this.assertEqual(FloatPoint.makeInstance(3.1415).toWrittenString(),
				 "3.1415");
		this.assertEqual(Cons.makeInstance(Cons.makeInstance(FloatPoint.makeInstance(3.1415),
								     Empty.EMPTY),
						   Empty.EMPTY).toWrittenString(),
				 "((3.1415))");

	    },


	    testChar : function() {
		this.assert(Kernel.char_question_(Char.makeInstance("x")));
		this.assert(! Kernel.char_question_(String.makeInstance("x")));
	    },


	    testPosn : function() {
		var x = Rational.makeInstance(31337);
		var y = Rational.makeInstance(31337);
		var p = Kernel.make_dash_posn(x, y);
		this.assert(Kernel.posn_question_(p));
		this.assert(! Kernel.posn_question_(x));

		this.assertEqual(x, Kernel.posn_dash_x(p));
		this.assertEqual(y, Kernel.posn_dash_y(p));

		this.assertRaise("MobyTypeError",
				 function() { Kernel.posn_dash_x(42)} );
	    },


	    testError : function() {
		this.assertRaise("MobyRuntimeError",
				 function() { Kernel.error(Symbol.makeInstance("ah"), "blah")});
	    },


	    testIsStruct : function() {
		this.assert(! Kernel.struct_question_(String.makeInstance("hey")));
		this.assert(Kernel.struct_question_(Kernel.make_dash_posn(3, 4)));
	    },
	    
	    testEqualHuhApprox : function () {
		this.assert(Kernel.equal_tilde__question_("hello", "hello", 
							  FloatPoint.makeInstance("0.0001")));

		
		this.assert(! Kernel.equal_tilde__question_("hello", "world", 
							    FloatPoint.makeInstance("0.0001")));
		this.assert(! Kernel.equal_tilde__question_(Rational.makeInstance(22, 7),
							    FloatPoint.makeInstance(3.1415),
							    FloatPoint.makeInstance("0.001")));
		this.assert(Kernel.equal_tilde__question_(Rational.makeInstance(22, 7),
							  FloatPoint.makeInstance(3.1415),
							  FloatPoint.makeInstance("0.01")));

	    },

	    testMap : function() {
		this.assert(Kernel.equal_question_(Kernel.list([Rational.makeInstance(1),
								Rational.makeInstance(4),
								Rational.makeInstance(9)]),
						   Kernel.map(function(args) { return Kernel.sqr(args[0])}, 
							      [Kernel.list([Rational.makeInstance(1),
									    Rational.makeInstance(2),
									    Rational.makeInstance(3)])])));
	    },



	    testFoldl : function() {
		this.assert(Kernel.equal_question_(
		    Rational.makeInstance(6),
		    Kernel.foldl(function(args) { return Kernel._plus_(args)}, 
				 Rational.ZERO,
				 [Kernel.list([Rational.makeInstance(1),
					       Rational.makeInstance(2),
					       Rational.makeInstance(3)])])));
	    },



	    testFoldr : function() {
		this.assert(Kernel.equal_question_(
		    Rational.makeInstance(6),
		    Kernel.foldr(function(args) { return Kernel._plus_(args)}, 
				 Rational.ZERO,
				 [Kernel.list([Rational.makeInstance(1),
					       Rational.makeInstance(2),
					       Rational.makeInstance(3)])])));
	    },




	    testFilter : function() {
		this.assert(Kernel.equal_question_(Kernel.list([Rational.makeInstance(2)]),
						   Kernel.filter(function(args) { 
						       return Kernel.even_question_(args[0])},
								 Kernel.list([Rational.makeInstance(1),
									      Rational.makeInstance(2),
									      Rational.makeInstance(3)]))));
	    },
	    
	    testBuildList: function() {
		this.assert(Kernel.equal_question_(Kernel.list([]),
						   Kernel.build_dash_list(Rational.ZERO,
									  function(args) { return args[0]; })));
		this.assert(Kernel.equal_question_(Kernel.list([Rational.ZERO]),
						   Kernel.build_dash_list(Rational.makeInstance(1),
									  function(args) { return args[0]; })));
		this.assert(Kernel.equal_question_(Kernel.list([Rational.ZERO, Rational.ONE]),
						   Kernel.build_dash_list(Rational.makeInstance(2),
									  function(args) { return args[0]; })));
	    },

	    testBuildString: function() {
		this.assert(Kernel.equal_question_(Kernel.string([]),
						   Kernel.build_dash_string
						   (Rational.ZERO,
						    function(args) { return Char.makeInstance("x"); })));
		this.assert(Kernel.equal_question_("x",
						   Kernel.build_dash_string
						   (Rational.makeInstance(1),
						    function(args) { return Char.makeInstance("x"); })));
		this.assert(Kernel.equal_question_("xx",
						   Kernel.build_dash_string
						   (Rational.makeInstance(2),
						    function(args) { return Char.makeInstance("x"); })));
	    },


	    

	    testSort : function() {
		this.assert(Kernel.equal_question_
			    (Kernel.list([Rational.makeInstance(1),
					  Rational.makeInstance(4),
					  Rational.makeInstance(9)]),
			     Kernel.sort(Kernel.list([Rational.makeInstance(4),
						      Rational.makeInstance(1),
						      Rational.makeInstance(9)]),
					 function(args) {
					     return Kernel._lessthan_(args[0], 
								      args[1], []);
					 })));
	    }, 


	    testAndmap : function() {
		this.assert(Kernel.equal_question_(plt.types.Logic.FALSE,
						   Kernel.andmap(function(args) { 
						       var result = Kernel.even_question_(args[0]);
						       return result; },
								 [Kernel.list([Rational.makeInstance(1),
									       Rational.makeInstance(2),
									       Rational.makeInstance(3)])])));

 		this.assert(Kernel.equal_question_(plt.types.Logic.TRUE,
 						   Kernel.andmap(function(args) { 
 						       return Kernel.even_question_(args[0])},
 								 [Kernel.list([Rational.makeInstance(2)])])));
 		this.assert(Kernel.equal_question_(plt.types.Logic.TRUE,
 						   Kernel.andmap(function(args) { 
 						       return Kernel.even_question_(args[0])},
 								 [Kernel.list([])])));
	    },


	    testOrmap : function() {
		this.assert(Kernel.equal_question_(plt.types.Logic.TRUE,
						   Kernel.ormap(function(args) { 
						       var result = Kernel.even_question_(args[0]);
						       return result; },
								[Kernel.list([Rational.makeInstance(1),
									      Rational.makeInstance(2),
									      Rational.makeInstance(3)])])));

 		this.assert(Kernel.equal_question_(plt.types.Logic.TRUE,
 						   Kernel.ormap(function(args) { 
 						       return Kernel.even_question_(args[0])},
 								[Kernel.list([Rational.makeInstance(2)])])));
 		this.assert(Kernel.equal_question_(plt.types.Logic.FALSE,
 						   Kernel.ormap(function(args) { 
 						       return Kernel.even_question_(args[0])},
 								[Kernel.list([])])));
	    },



	    testArgmin : function() {
		var lst = Kernel.list(["hello", "world", "testing"]);
		this.assertEqual("hello",
				 Kernel.argmin(function(args) {
				     return Rational.makeInstance(args[0].length) },
					       lst));
	    },


	    testArgmax : function() {
		var lst = Kernel.list(["hello", "world", "testing"]);
		this.assertEqual("testing",
				 Kernel.argmax(function(args) {
				     return Rational.makeInstance(args[0].length) },
					       lst));
	    },


	    testMemf: function() {
		var lst = Kernel.list([Rational.makeInstance(2),
				       Rational.makeInstance(4),
				       Rational.makeInstance(7),
				       Rational.makeInstance(8)]);
		this.assertEqual(lst,
				 Kernel.memf(function(args) {
				     return args[0].toInteger() % 2 == 0},
					     lst));
		
		this.assertEqual(lst.rest().rest(),
				 Kernel.memf(function(args) {
				     return args[0].toInteger() % 2 == 1},
					     lst));

		this.assert(! Kernel.memf(function(args) {
		    return args[0].toInteger() > 8},
					  lst));


	    },


	    testCompose: function() { 
		var f1 = Kernel.compose([]);
		this.assertEqual(42, f1([42]));
		this.assertEqual(42, f1([42, 43, 44]));


		var f2 = Kernel.compose([function(args) { return args[0] * args[0]; },
					 function(args) { return args[0] + 1; } ]);
		this.assertEqual(36, f2([5]));
	    }, 

	    testIsProcedure: function() {
		this.assert(! Kernel.procedure_question_(42));
		this.assert(Kernel.procedure_question_(function() {return 42;}));
	    }, 

	    testRemove: function() {
		this.assert(Kernel.equal_question_(
		    plt.Kernel.list(["one", "two", "four"]),
		    Kernel.remove("three", 
				  plt.Kernel.list(
				      ["one", "two", "three", "four"]))));
		
		
		this.assert(Kernel.equal_question_(
		    plt.Kernel.list(["one", "two", "three", "four"]),
		    Kernel.remove("zero", 
				  plt.Kernel.list(
				      ["one", "two", "three", "four"]))));
		
		this.assert(Kernel.equal_question_(
		    plt.Kernel.list(["two", "three", "four"]),
		    Kernel.remove("one", 
				  plt.Kernel.list(
				      ["one", "two", "three", "four"]))));
	    },
	    

	    testSetCar: function() {
		var lst = Kernel.list([Rational.makeInstance(1),
				       Rational.makeInstance(2),
				       Rational.makeInstance(3)]);

		Kernel.set_dash_car_bang_(lst,Char.makeInstance("x"));

		this.assert(Kernel.equal_question_(Char.makeInstance("x"), lst.first()));

		this.assertRaise("MobyTypeError",
				 function() {
				     Kernel.set_dash_car_bang_(Rational.makeInstance(10),lst);});
	    },

	    testSetCdr: function() {
		var lst = Kernel.list([Rational.makeInstance(1),
				       Rational.makeInstance(2),
				       Rational.makeInstance(3)]);

		var lst2 = Kernel.list([Rational.makeInstance(4),
					Rational.makeInstance(5)]);

		Kernel.set_dash_cdr_bang_(lst,lst2);

		this.assert(Kernel.equal_question_(lst2, lst.rest()));

		this.assertRaise("MobyTypeError",
				 function() {
				     Kernel.set_dash_cdr_bang_(Rational.makeInstance(10),lst);});

		this.assertRaise("MobyTypeError",
				 function() {
				     Kernel.set_dash_cdr_bang_(lst,Rational.makeInstance(10));});
		},

	    testVectors: function() {
		    this.assert(Kernel.equal_question_(Kernel.vector_dash_length(Kernel.vector([])),
						       Rational.makeInstance(0)));

		    this.assert(Kernel.equal_question_(Kernel.vector_dash_length(Kernel.vector(["foo", "bar"])),
						       Rational.makeInstance(2)));

		    this.assert(Kernel.equal_question_(Kernel.vector_dash_ref(Kernel.vector(["foo", "bar"]), Rational.makeInstance(0)),
						       "foo"));

		    this.assert(Kernel.equal_question_(Kernel.vector_dash_ref(Kernel.vector(["foo", "bar"]), Rational.makeInstance(1)),
						       "bar"));
		    this.assertRaise("MobyTypeError",
				     function() {
					 Kernel.vector_dash_ref
					     (Kernel.vector(["foo", "bar"]), Rational.makeInstance(2));});

		    this.assertEqual(5, Kernel.vector_dash_length(Kernel.make_dash_vector(Rational.makeInstance(5))).toInteger());

		    var aVector = Kernel.make_dash_vector(Rational.makeInstance(5));
		    Kernel.vector_dash_set_bang_(aVector, Rational.ZERO, "blah");
		    this.assertEqual("blah", Kernel.vector_dash_ref(aVector, Rational.ZERO));
		    Kernel.vector_dash_set_bang_(aVector, Rational.ZERO, "boing");
		    this.assertEqual("boing", Kernel.vector_dash_ref(aVector, Rational.ZERO));


		    var zeroonetwo = Kernel.build_dash_vector(Rational.makeInstance(3),
							      function(args) {
								  return args[0];
							      });
		    for (var i = 0; i < 3; i++) {
			this.assertEqual(i,
					 Kernel.vector_dash_ref(zeroonetwo, Rational.makeInstance(i)).toInteger());
		    }

		    this.assert(Kernel.vector_question_(Kernel.vector([])));
		    this.assert(! Kernel.vector_question_(Kernel.list([])));
	    }
	    
	    
	})
    }

})();
