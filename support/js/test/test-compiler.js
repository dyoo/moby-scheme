function init() {

    function isEqual(x, y) {
	return plt.Kernel.equal_question_(x, y);
    }

    function cons(x, y) {
	return plt.Kernel.cons(x, y);
    }

    var empty = plt.types.Empty.EMPTY;
    var number = function(x) { return plt.types.Rational.makeInstance(x, 1); };
    var str = function(x) { return plt.types.String.makeInstance(x); };
    var symbol = plt.types.Symbol.makeInstance;

    var TRUE = plt.types.Logic.TRUE;
    var FALSE = plt.types.Logic.FALSE;

    var pinfo = get_dash_base_dash_pinfo(symbol("moby"));

    // run: string -> scheme-value
    // Evaluates the string and produces the evaluated scheme value.
    var run = function(aSource) {
	var namespace = new Namespace();
	var program = plt.reader.readSchemeExpressions(aSource);
	var compiledProgram = (program_dash__greaterthan_compiled_dash_program_slash_pinfo
			       (program, pinfo));
	var defns = compiled_dash_program_dash_defns(compiledProgram);
	var interFunc = compiled_dash_program_dash_toplevel_dash_exprs(compiledProgram);
	var runToplevel = namespace.eval(defns, interFunc);
	
	var result;				  
	runToplevel(function(val) { result = val;  });
	return result;
    }



    

    return new Test.Unit.Runner({
	setup: function() {},
	
        teardown: function() {},
	
	testSimpleDatum: function() {
	    this.assert(isEqual(number(42), run("42")))
	},

	testSimpleConditionals: function() {
	    this.assert(isEqual("foo",
				run("(if true \"foo\" \"bar\")")));
	    this.assert(isEqual("bar",
				run("(if false \"foo\" \"bar\")")));
	},

	testSimpleFunctionDefinition: function() {
	    this.assert(isEqual(number(9),
				run("(define (f x) (* x x))" +
				    "(f 3)")));
	    
	},
	testLambda: function() {
	    this.assert(isEqual(number(25),
				run("((lambda (x) (* x x)) 5)")));
	},


	testDivisionByZero: function() {
	    plt.Kernel.lastLoc = undefined;
	    this.assertRaise("MobyRuntimeError",
			     function() { run("   (/ 1 0)")});
	    this.assert(isEqual("offset=3 line=1 span=7 id=\"\"", plt.Kernel.lastLoc));
	},


	testHigherOrderFunction: function() {
	    this.assert(isEqual(cons(str("A"),
				     cons(str("B"),
					  cons(str("F"),
					       cons(str("X"),
						    cons(str("Y"), empty))))),
				run("(define (insert cmp elt l)"+
				    "  (cond"+
				    "    [(empty? l) (list elt)]"+
				    "    [(cons? l) (if (cmp elt (first l))"+
				    "                   (cons elt l)"+
				    "                   (cons (first l)"+
				    "                         (insert cmp elt (rest l))))]))"+
				    "(insert string<=? \"F\" '(\"A\" \"B\" \"X\" \"Y\"))")))
	},


	// Bug reported by Alex Kruckman
	testLambdaDefinition: function() {
	    this.assert(isEqual(number(43),
				run("(define add1 (lambda (x) (+ x 1)))" +
				    "(add1 42)")));
	},
	

	testAnd: function() {
	    this.assert(isEqual(FALSE, run("(and false)")));
	    this.assert(isEqual(TRUE, run("(and true)")));
	    this.assert(isEqual(FALSE, run("(and true false)")));
	    this.assert(isEqual(TRUE, run("(and true true)")));
	    this.assert(isEqual(FALSE, run("(and false true)")));
	    this.assert(isEqual(FALSE, run("(and false false)")));
	},


	testOr: function() {
	    this.assert(isEqual(TRUE, run("(or true)")))
	    this.assert(isEqual(FALSE, run("(or false)")))
	    this.assert(isEqual(TRUE, run("(or true false)")));
	    this.assert(isEqual(TRUE, run("(or true true)")));
	    this.assert(isEqual(TRUE, run("(or false true)")));
	    this.assert(isEqual(FALSE, run("(or false false)")));
	},


	testOrmap: function() {
	    this.assert(isEqual(TRUE,
				run("(ormap (lambda (x) x) (list true))")));

 	    this.assert(isEqual(FALSE,
 				run("(ormap (lambda (x) x) (list))")));

 	    this.assert(isEqual(FALSE,
 				run("(ormap (lambda (x) x) (list false))")));

 	    this.assert(isEqual(TRUE,
 				run("(ormap (lambda (x) x) (list false true))")));

	},

	testAndmap: function() {
	    this.assert(isEqual(TRUE,
				run("(andmap (lambda (x) x) (list true))")));

	    this.assert(isEqual(TRUE,
				run("(andmap (lambda (x) x) (list))")));

	    this.assert(isEqual(FALSE,
				run("(andmap (lambda (x) x) (list false))")));

	    this.assert(isEqual(FALSE,
				run("(andmap (lambda (x) x) (list false true))")));

	    this.assert(isEqual(TRUE,
				run("(andmap (lambda (x) x) (list true true))")));

	},



	testBegin: function() {
	    // normal behaviour
	    this.assert(isEqual(TRUE,
				run("(begin (+ 1 2) (+ 3 4) true)")));


	    // non top-level definition
	    this.assertRaise("MobySyntaxError",
			     function () {
				 run("(begin (+ 1 2) (- 3 4) (define j 5) (* 2 3))")});

	    // sequencial evaluation
	    this.assert(isEqual(number(3),
			    	run("(define x 5)(define y 3)" +
				    "(begin (set! x y) (set! y x) y)")));
	    
	},

	testBoxMutation: function() {
	    this.assert(isEqual(number(2),
			    	run("(define bx (box 5))" +
				    "(begin (set-box! bx 2)" +
				    "(unbox bx))")));
	    this.assertRaise("MobyTypeError",
			     function () {
				 run("(define bx (box 5))" +
				     "(begin (set-box! 2 bx)" +
				     "(unbox bx))")});

	},

	testStructureMutators: function() {
	    this.assert(isEqual(number(9),
				run("(define-struct str (a b c))" +
				    "(define a-str (make-str 1 2 3))" +
				    "(set-str-b! a-str 9)" +
				    "(str-b a-str)")));

	    this.assertRaise("MobyRuntimeError",
			     function () {
			    	 run("(define-struct str (a b c))" +
				     "(define a-str (make-str 1 2 3))" +
				     "(set-str-b! 9 a-str)" +
				     "(str-b a-str)")});

	    // posns are immutable
	    this.assertRaise("MobySyntaxError",
			     function () {
			    	 run("(define a-posn (make-posn 1 3))" +
				     "(set-posn-x! a-posn 6)" +
				     "(posn-x a-posn)")});
	},

	testSetWithNonIdentifier: function() {
	    this.assertRaise("MobySyntaxError",
			     function() {
				 run("(set! \"a string\" 17)")});
	    this.assertRaise("MobySyntaxError",
			     function() {
				 run("(set! 'a-quoted-id 17)")});
	},

	testSet: function() {
	    // normal behaviour
	    this.assert(isEqual("a",
			    	run("(define x 5)" +
				    "(set! x \"a\")" +
				    "x")));
	    
	    // undefined top-level variable
	    this.assertRaise("MobySyntaxError",
			     function () {
				 run("(set! c 42)")});
	    
	    // function arguments are mutable
	    this.assert(isEqual(number(3),
		            	run("(define (func x) (begin (set! x 3) x))" +
				    "(func 42)")));
	    
	    // scoping
	    this.assert(isEqual(number(42),
				run("(define a 42)" +
				    "(define (f x) " +
				    "(local ((define a 3)) " +
				    "(begin (set! a x) a)))" +
	      			    "(begin (f 21) a)")));
	    
	},

	testCaseSpecialForm: function(){

	    // normal behaviour
	    this.assert(isEqual(symbol("big"),
				run("(case (+ 7 5)" +
				    "[(1 2 3) 'small]" +
				    "[(10 11 12) 'big])")));

	    // normal behaviour - 2
	    this.assert(isEqual(symbol("small"),
				run("(case (- 7 5)" +
				    "[(1 2 3) 'small]" +
				    "[(10 11 12) 'big])")));

	    // else clause
	    this.assert(isEqual(symbol("dummy"),
				run("(case (* 7 5)" +
				    "[(1 2 3) 'small]" +
				    "[(10 11 12) 'big]" +
				    "[else 'dummy])")));

 	    // clause with else should be the last one
 	    this.assertRaise("MobySyntaxError",
 			     function(){
 				 run("(case (* 7 5)" +
 				     "[(1 2 3) 'small]" +
 				     "[(10 11 12) 'big]" +
 				     "[else 'dummy]" +
 				     "[(1 2 3) 'shouldBeError])")});

	    // no else caluse - void output
	    this.assert(isEqual(number(3),
				run("(case (* 7 5)" +
				    "[(1 2 3) 'small]" +
				    "[(10 11 12) 'big])" +
				    "3")));

	    // first occurance will be evaluated - like cond
	    this.assert(isEqual(symbol("big"),
				run("(case 12" +
				    "[(1 2 3) 'small]" +
				    "[(10 11 12) 'big]" +
				    "[(12 13 14) 'bigger])")));

	    // no quoted expression
 	    this.assertRaise("MobyTypeError",
 			     function(){
 				 run("(case (* 7 5)" +
 				     "[2 'small]" +
 				     "[(10 11 12) 'big])" +
				     "3")});

	    // symbols
 	    this.assert(isEqual(symbol("big"),
 				run("(case 'x" +
 				    "[(1 2) 'small]" +
 				    "[(b x c) 'big])")));

	},

	testCaseHygiene: function() {
	    this.assert(isEqual(plt.Kernel.list([number(1), number(2)]),
				run("((lambda (check val) " +
				    "    (case val " +
				    "      [(val) (list check val)] " +
				    "      [else (list 1 2)]))" +
				    " 1024 2048)")));

	    this.assert(isEqual(plt.Kernel.list([number(1), number(2)]),
				run("((lambda (check val) " +
				    "    (case check " +
				    "      [(check) (list check val)] " +
				    "      [else (list 1 2)]))" +
				    " 1024 2048)")));

	    this.assert(isEqual(plt.Kernel.list([number(1), number(2)]),
				run("((lambda (check val) " +
				    "    (case check " +
				    "      [(val) (list check val)] " +
				    "      [else (list 1 2)]))" +
				    " 1024 2048)")));
	},

	testLet: function() {
	    this.assert(isEqual(number(1979), run("(let () 1979)")));
	    this.assert(isEqual(number(42),
				run("(let ((x 42)) x)")));

	    this.assert(isEqual(number(129),
				run("(let ((x 42) (y 43) (z 44)) (+ x y z))")));



	    this.assert(isEqual(number(43),
				run("(let ((x 42)) "+
				    "  (let ((x (+ x 1)))"+
                                    "    x))")));

	    // Check for binding symbol more than once.
	    this.assertRaise("MobySyntaxError",
			     function() { run("(let ((x 42) (x 43)) x)") });
            
	},

	testLetStar: function() {
	    this.assert(isEqual(number(10), run("(let* ((x 9) (x (+ x 1))) x)")))
	    this.assert(isEqual(number(16), run("(let* () 16)")))
	},

	testLetrec: function() {
	    this.assert(isEqual(number(12), run("(letrec () (* 3 4))")));
	    this.assert(isEqual(plt.types.Logic.TRUE, 
				run("(letrec ([myeven? (lambda (x) (if (= x 0) true (myodd? (sub1 x))))] [myodd? (lambda (x) (if (= x 0) false (myeven? (sub1 x))))]) (even? 8))")));
	    this.assert(isEqual(plt.types.Logic.FALSE, 
				run("(letrec ([myeven? (lambda (x) (if (= x 0) true (myodd? (sub1 x))))] [myodd? (lambda (x) (if (= x 0) false (myeven? (sub1 x))))]) (even? 7))")));

	    this.assertRaise("MobySyntaxError",
			     function() { run("(letrec ((x 42)(x 43)) x)") });


	},


	testCheckExpect: function() {
	    this.assertRaise("MobyTestingError",
			     function() { run("(check-expect 3 4)"); })
	    run("(check-expect 3 3)");
	},

	testCheckWithin: function() {
	    run("(check-within 22/7 pi 0.01)");
	    this.assertRaise("MobyTestingError",
			     function() { run("(check-within 22/7 pi 0.00001)"); })

	},

	testCheckError: function() {
	    this.assertRaise("MobyTestingError",
			     function() { run("(check-error 42 \"blah\")")})
	    run("(check-error (/ 1 0) \"division by zero\")");
	},
	

	testQuasiquotation: function() {
	    this.assert(isEqual(run("'(unquote x)"),
				run("',x")));

	    this.assert(isEqual(run("'(unquote-splicing x)"),
				run("',@x")));

	    this.assert(isEqual(run("'foo"),
				run("`foo")));
	    this.assert(isEqual(run("'bar"),
				run("(let ((foo 'bar)) `,foo)")));

	    this.assert(isEqual(run("'(hello world)"),
	    			run("`(hello world)")));

	    this.assert(isEqual(run("'(x world)"),
				run("(local ((define x 'goodbye)) `(x world))")));

	    this.assert(isEqual(run("'(goodbye world)"),
				run("(local ((define x 'goodbye)) `(,x world))")));

	    this.assert(isEqual(run("'(x world)"),
				run("(local ((define x 'goodbye)) `(,'x world))")));

	    this.assert(isEqual(run("'(x world)"),
				run("(local ((define x 'goodbye)) `(,`x world))")));

	    this.assert(isEqual(run("'(goodbye world)"),
				run("(local ((define x 'goodbye)) `(,`,x world))")));


	    this.assert(isEqual(run("'(1 2 3)"),
				run("(let ((numbers '(1 2 3))) `(,@numbers))")));

	    this.assert(isEqual(run("'(a b c d)"),
				run("(let ((x '(a b)) (y 'c)) `(,@x ,y d))")));
	},

	testInf: function() {
	    this.assert(isEqual(run("+inf.0"),
				run("(add1 +inf.0)"))); 
	    this.assert(isEqual(run("1"),
				run("(length (list +inf.0))")));
				    
	    },

	    testAtan: function() {
		this.assert(isEqual(run("(atan +inf.0 -inf.0)"),
				    run("2.356194490192345")));
		this.assert(isEqual(run("(atan 0.5)"),
				    run("0.4636476090008061")));
		this.assert(isEqual(run("(atan 2 1)"),
				    run("1.1071487177940904")));
		// Current deficiency in binding expressions prevent us from
		// catching the arity error at compile time.
		this.assertRaise("MobyRuntimeError",
				 function() { run("(atan 1 2 3)") });
	    },

	testNan: function() {
	    this.assert(isEqual(
		run("+nan.0"),
		plt.types.FloatPoint.makeInstance(Number.NaN)));
	},

	testIntegerSqrt: function() {
		this.assert(isEqual(run("(integer-sqrt 10)"),
				    run("3")));
		this.assert(isEqual(
				    run("(integer-sqrt -10)"),
				    run("0+3i")));
	}

    });

}
