var lastException;



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

var compilerModule = plt.Kernel.invokeModule("moby/compiler");
var pinfo = compilerModule.EXPORTS['get-base-pinfo'](symbol("moby"));

// run: string -> scheme-value
// Evaluates the string and produces the evaluated scheme value.
var run = function(aSource) {
    try {
	var namespace = new Namespace();
	var program = plt.reader.readSchemeExpressions(aSource, "test");
	var compiledProgram = (compilerModule.EXPORTS['program->compiled-program/pinfo']
			       (program, pinfo));
	var defns = compilerModule.EXPORTS['compiled-program-defns'](compiledProgram);
	var interFunc = compilerModule.EXPORTS['compiled-program-toplevel-exprs'](compiledProgram);
	var runToplevel = namespace.eval(defns, interFunc);
	
	var result;				  
	runToplevel(function(val) { result = val;  });
	return result;
    } catch(e) {
	lastException = e;
	throw e;
    } 
}



function init() {

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


	testCondExhausted: function() {
	    this.assertMobyRaise(isConditionalExhausted,
				 function() {
				     run("(cond [(= 1 0) 'huh?])");
				 },
				"cond exhausted");
	},

	testCondBadForm: function() {
	    this.assertMobyRaise(isConditionalMissingQuestionAnswer,
				 function() {
				     run("(cond)");
				 },
				 "cond missing question answer");

	    this.assertMobyRaise(isConditionalMalformedClause,
				 function() {
				     run("(cond 42)");
				 });

	    this.assertMobyRaise(isConditionalMalformedClause,
				 function() {
				     run("(cond true)");
				 });
	    
 	    this.assertMobyRaise(isConditionalClauseTooFewElements,
 				 function() {
 				     run("(cond [foo])");
 				 });

 	    this.assertMobyRaise(isConditionalClauseTooFewElements,
 				 function() {
 				     run("(cond [])");
 				 });

 	    this.assertMobyRaise(isConditionalClauseTooManyElements,
 				 function() {
 				     run("(cond [a b c])");
 				 });

	    this.assertMobyRaise(isBranchValueNotBoolean,
				 function() {
				     run("(cond [(+ 1 2) \"oh no!\"])")
				 });
	},

	testNumberToString: function() {
	    this.assert(isEqual("3124",
				run("(number->string 3124)")));
	},


	testStringToNumber: function() {
	    this.assert(isEqual(plt.types.Rational.makeInstance(4213),
				run("(string->number \"4213\")")));
	},



	testBadIfs: function() {
	    this.assertMobyRaise(isIfTooManyElements,
				 function() {
				     run("(if 3 4 5 6)")
				 });

	    this.assertMobyRaise(isIfTooFewElements,
				 function() {
				     run("(if 3)")
				 });
	},


	testTypeMismatching: function() {
	    this.assertMobyRaise(isTypeMismatch,
				 function() {
				     run("(+ 'three 'four)");
				 });
	},


	testSimpleFunctionDefinition: function() {
	    this.assert(isEqual(number(9),
				run("(define (f x) (* x x))" +
				    "(f 3)")));
	    
	},

	testLambda: function() {
	    this.assert(isEqual(number(25),
				run("((lambda (x) (* x x)) 5)")));
	    this.assert(isEqual(number(42),
				run("((lambda () 42))")));
	},


	testLambdaBadListOfArguments: function() {
	    this.assertMobyRaise(isDuplicateIdentifier,
				 function() { run("(lambda (x x) (* x x))")});

	    this.assertMobyRaise(isExpectedIdentifier,
				 function() { run("(lambda (x 1) (* x x))")});

 	    this.assertMobyRaise(isExpectedListOfIdentifiers,
 				 function() { run("(lambda 42 (* x x))")});

	},

	testLambdaNotSingleBody: function() {
 	    this.assertMobyRaise(isLambdaTooFewElements,
 				 function() { run("(lambda)")});

 	    this.assertMobyRaise(isLambdaTooFewElements,
 				 function() { run("(lambda ())")});

 	    this.assertMobyRaise(isLambdaTooManyElements,
 				 function() { run("(lambda () 1 2 3)")});
	},



	testDivisionByZero: function() {
	    plt.Kernel.lastLoc = undefined;
	    this.assertMobyRaise(isGenericRuntimeError,
				 function() { run("   (/ 1 0)")});
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


	testRaise: function() {
	    try {
		run("(raise 42)");
		this.fail();
	    } catch (e) {
		this.assert(isEqual(number(42),
				    e));
	    }
	},



	// Bug reported by Alex Kruckman
	testLambdaDefinition: function() {
	    this.assert(isEqual(number(43),
				run("(define add1 (lambda (x) (+ x 1)))" +
				    "(add1 42)")));
	},
	



	// Bug reported by David Treijo
	testIdentifierWithLeadingNumber: function() {
		this.assert(isEqual("first",
				    run("(define 1px \"first\")" + " 1px")));
	},

	testApplication: function() {
	    this.assert(isEqual(number(9),
				run("(let ((x (lambda (a b c) (a b c)))) (x + 4 5))")));
	},

	testMakeVector: function() {
		this.assert(isEqual("#(0 0 0 0 0)\n",
				    run("(format \"~a~n\" (make-vector 5 0))")));

		this.assert(isEqual("#(<undefined> <undefined> <undefined> <undefined>)\n",
				    run("(format \"~a~n\" (make-vector 4))")));
        },


	testProcedureArity: function() {
	    this.assert(isEqual(number(2),
				run("(procedure-arity make-rectangular)")));
	    this.assert(isEqual(run("'(at-least 0)"),
				run("(procedure-arity +)")));
	    this.assert(isEqual(run("3"),
				run("(procedure-arity (lambda (x y z) 'blah))")));
	    this.assert(isEqual(run("2"), 
				run("(local [(define (f x) (procedure-arity x))] (f make-polar))")));

	    this.assert(isEqual(run("'(at-least 1)"),
				run("(procedure-arity /)")));

	},

	
	testAnd: function() {
	    this.assert(isEqual(FALSE, run("(and true false)")));
	    this.assert(isEqual(TRUE, run("(and true true)")));
	    this.assert(isEqual(FALSE, run("(and false true)")));
	    this.assert(isEqual(FALSE, run("(and false false)")));
	},

	testAndExpectsAtLeastTwoExpressions: function() {
	    this.assertMobyRaise(isBooleanChainTooFewElements,
				 function() { run("(and)")} );
	    this.assertMobyRaise(isBooleanChainTooFewElements,
				 function() { run("(and true)")} );
	},


	testOr: function() {
	    this.assert(isEqual(TRUE, run("(or true false)")));
	    this.assert(isEqual(TRUE, run("(or true true)")));
	    this.assert(isEqual(TRUE, run("(or false true)")));
	    this.assert(isEqual(FALSE, run("(or false false)")));
	},


	testOrExpectsAtLeastTwoExpressions: function() {
	    this.assertMobyRaise(isBooleanChainTooFewElements,
				 function() { run("(or)") });
	    this.assertMobyRaise(isBooleanChainTooFewElements,
				 function() { run("(or true)")} );
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


	testRequire: function() {
	    run("(require moby/runtime/error-struct)");
	},


	testBegin: function() {
	    // normal behaviour
	    this.assert(isEqual(TRUE,
				run("(begin (+ 1 2) (+ 3 4) true)")));


	    this.assertMobyRaise(isUndefinedIdentifier,
				 function() {
				     run("(begin (+ 1 2) (- 3 4) (define j 5) (* 2 3))");
				 });

	    // sequential evaluation
	    this.assert(isEqual(number(3),
			    	run("(define x 5)(define y 3)" +
				    "(begin (set! x y) (set! y x) y)")));
	    
	},

	testBeginEmptyBody: function() {
	    this.assertMobyRaise(isBeginBodyEmpty,
				 function(){
				     run("(begin)")
				 });
	},
	
	testBoxMutation: function() {
	    this.assert(isEqual(number(2),
			    	run("(define bx (box 5))" +
				    "(begin (set-box! bx 2)" +
				    "(unbox bx))")));
	    this.assertMobyRaise(isTypeMismatch,
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

	    this.assertMobyRaise(
		isGenericRuntimeError,
		function () {
		    run("(define-struct str (a b c))" +
			"(define a-str (make-str 1 2 3))" +
			"(set-str-b! 9 a-str)" +
			"(str-b a-str)")});

	    // posns are immutable, so set-posn-x! should not exist.
	    this.assertMobyRaise(isUndefinedIdentifier,
				 function () {
			    	     run("(define a-posn (make-posn 1 3))" +
					 "(set-posn-x! a-posn 6)" +
					 "(posn-x a-posn)")});
	},

	testSetWithNonIdentifier: function() {
	    this.assertMobyRaise(isExpectedIdentifier,
				 function() {
				     run("(set! \"a string\" 17)")});
	    this.assertMobyRaise(isExpectedIdentifier,
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
	    this.assertMobyRaise(isUndefinedIdentifier,
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
 	    this.assertMobyRaise(isGenericSyntacticError,
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
 	    this.assertMobyRaise(isGenericSyntacticError,
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
	    this.assertMobyRaise(isDuplicateIdentifier,
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

	    this.assertMobyRaise(isDuplicateIdentifier,
			     function() { run("(letrec ((x 42)(x 43)) x)") });


	},


	testCheckExpect: function() {
	    run("(check-expect 3 3)");
	    this.assertMobyRaise(
		isCheckExpect,
		function() { run("(check-expect 3 4)"); })
	},
	
	
	testExample: function() {
	    this.assertMobyRaise(isCheckExpect,
			     function() { run("(EXAMPLE 3 4)"); })
	    run("(EXAMPLE 3 3)");
	},


	testCheckWithin: function() {
	    run("(check-within 22/7 pi 0.01)");
	    this.assertMobyRaise(isCheckWithin,
			     function() { run("(check-within 22/7 pi 0.00001)"); })

	},

	testCheckError: function() {
  	    this.assertMobyRaise(
  		isCheckError,
  		function() { run("(check-error (/ 1 0) \"blah\")")})
  	    this.assertMobyRaise(
  		isCheckErrorNoError,
  		function() { run("(check-error 42 \"blah\")")})
	    run("(check-error (/ 1 0) \"division by zero\")");
	},
	
	testQuoteMissingExpression: function() {
	    this.assertMobyRaise(isQuoteTooFewElements,
				 function() { run("(quote)"); });
	    this.assert(isEqual(run("(list 'quote)"),
				run("(quote (quote))")));
	},

	testQuoteTooManyElements: function() {
	    this.assertMobyRaise(isQuoteTooManyElements,
				 function() { run("(quote a b)"); });
	},


	testQuasiquoteMissingExpression: function() {
	    this.assertMobyRaise(isQuasiquoteTooFewElements,
				 function() { run("(quasiquote)"); });
	    this.assert(isEqual(run("(list 'quasiquote)"),
				run("(quasiquote (quasiquote))")));
	},

	testQuasiquoteTooManyElements: function() {
	    this.assertMobyRaise(isQuasiquoteTooManyElements,
				 function() { run("(quasiquote 1 2)"); });
	},


	testUnquoteMissingExpression: function() {
	    this.assertMobyRaise(isUnquoteTooFewElements,
				 function() { run("(quasiquote (unquote))"); });
	},

	testUnquoteTooManyElements: function() {
	    this.assertMobyRaise(isUnquoteTooManyElements,
				 function() { run("(quasiquote (unquote x y))"); });
	},



	testUnquote: function() {
	    this.assert(isEqual(run("(list 'unquote 'pi)"),
				run("(quote (unquote pi))")));

	    this.assert(isEqual(run("pi"),
				run("(quasiquote (unquote pi))")));

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
				    

	    this.assert(isEqual(run("+inf.0"),
				run("(+ +inf.0 +inf.0)")));

	    this.assert(isEqual(run("+inf.0"),
				run("(+ +inf.0 -42)")));


	    this.assert(isEqual(run("+inf.0"),
				run("(- +inf.0 -42)")));


	    this.assert(isEqual(run("-inf.0"),
				run("(- 42 +inf.0)")));

	    this.assert(isEqual(run("+inf.0"),
				run("(- 42 -inf.0)")));


	    this.assert(isEqual(run("-inf.0"),
				run("(* 42 -inf.0)")));

	    this.assert(isEqual(run("+inf.0"),
				run("(* -42 -inf.0)")));

	    this.assert(isEqual(run("+inf.0"),
				run("(* -inf.0 -42)")));

	    this.assert(isEqual(run("+inf.0"),
				run("(* -inf.0 -inf.0)")));

	    this.assert(isEqual(run("-inf.0"),
				run("(+ -inf.0 -inf.0)")));

	    this.assert(isEqual(run("+nan.0"),
				run("(- -inf.0 -inf.0)")));

	    this.assert(isEqual(run("+nan.0"),
				run("(- +inf.0 +inf.0)")));

	    this.assert(isEqual(run("+nan.0"),
				run("(+ +inf.0 -inf.0)")));

	    this.assert(isEqual(run("true"),
				run("(> +inf.0 -inf.0)")));

	    this.assert(isEqual(run("false"),
				run("(> -inf.0 +inf.0)")));

	    this.assert(isEqual(run("true"),
				run("(<= +inf.0 +inf.0)")));

	    this.assert(isEqual(run("false"),
				run("(> +inf.0 +inf.0)")));

	    this.assert(isEqual(run("true"),
				run("(>= +inf.0 +inf.0)")));

	    this.assert(isEqual(run("false"),
				run("(< +inf.0 +inf.0)")));

	    this.assert(isEqual(run("true"),
				run("(<= +inf.0 +inf.0)")));

	    this.assert(isEqual(run("+inf.0"),
				run("(* +inf.0 1)")));
				    

	    this.assert(isEqual(run("-inf.0"),
				run("(* +inf.0 -1)")));


	    this.assert(isEqual(run("+inf.0"),
				run("(/ +inf.0 1)")));
				    

	    this.assert(isEqual(run("-inf.0"),
				run("(/ +inf.0 -1)")));


	    this.assert(isEqual(run("0"),
				run("(/ 1 +inf.0)")));
				    

	    this.assert(isEqual(run("0"),
				run("(/ -1 +inf.0)")));

	    this.assert(isEqual(run("0"),
				run("(/ 1 -inf.0)")));
				    

	    this.assert(isEqual(run("0"),
				run("(/ -1 -inf.0)")));


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
		this.assertMobyRaise(isApplicationArity,
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
	},

	testCompose: function() {
	    this.assert(isEqual(run("((compose sqrt sqr) 9)"),
				run("9")));
	},

	testHashtables: function() {
	    this.assert(isEqual(run("(let ([ht (make-hasheq)]) (equal? ht ht))"),
				run("true")));
	    this.assert(isEqual(run("(let ([ht (make-hasheq)]) (hash? ht))"),
				run("true")));
	    this.assert(isEqual(run("(let ([ht (make-hasheq)]) (hash? 'hash))"),
				run("false")));
	    
	    this.assert(isEqual(run("(let ([ht (make-hasheq)]) (begin (hash-set! ht 'danny 'yoo) (hash-ref ht 'danny 'unknown)))"),
				run("'yoo")));


	    this.assert(isEqual(run("(let ([ht (make-hasheq)]) (begin (hash-set! ht 'danny 'yoo) (hash-map ht (lambda (k v) (list v k)))))"),
				run("'((yoo danny))")));

	    this.assert(isEqual(run("(let ([ht (make-hasheq)]) (begin (hash-set! ht 'danny 'yoo) (hash-ref ht 'yoo 'unknown)))"),
				run("'unknown")));
	    this.assert(isEqual(run("(let ([ht (make-hasheq)]) (begin (hash-set! ht 'danny 'yoo) (hash-remove! ht 'shriram) (hash-ref ht 'danny 'unknown)))"),
				run("'yoo")));
            this.assert(isEqual(run("(let ([ht (make-hasheq)]) (begin (hash-set! ht 'danny 'yoo) (hash-remove! ht 'danny) (hash-ref ht 'danny 'unknown)))"),
				 run("'unknown")));

	    this.assert(isEqual(run('(list 42 "???" 43)'),
				run(
				    '(local [(define m (make-hash))' +
				    '        (define m2 (make-hasheq))' +
				    '        (define p (make-posn 5 6))]' +
				    '  (begin' +
				    '    (hash-set! m (make-posn 3 4) 42)' +
				    '    (hash-set! m2 p 43)' +
				    '    (list' +
				    '     (hash-ref m (make-posn 3 4) "???")' +
				    '     (hash-ref m2 (make-posn 5 6) "???")' +
				    '     (hash-ref m2 p "!!!"))))')));
	},

	testHashtables2: function() {
	    this.assert(isEqual(run("(list \"yes\" \"no\")"),
				run("(local [(define-struct blob (boy))"+
				    "        (define ht (make-hasheq))"+
				    "        (define b1 (make-blob \"sam\"))"+
				    "        (define b2 (make-blob \"lisa\"))"+
				    "]" +
				    "  (begin " +
				    "    (hash-set! ht b1 \"yes\")" +
				    "    (set-blob-boy! b1 \"lisa\")" +
				    "    (list (hash-ref ht b1 \"no\")" +
				    "          (hash-ref ht b2 \"no\"))))")));
	},

	testFunctionEq: function() {
	    // Thanks to Daniel Patterson for exposing an error involving function comparison.
	    this.assert(run("(eq? + +)"));
	    this.assert(run("(equal? + +)"));
	    this.assert(false === run("(equal? + sqr)"));
	    this.assert(false === run("(eq? + -)"));
	    this.assert(run("(local [(define (f x) x)] (eq? f f))"));
	    this.assert(false === run("(local [(define (f x) x) (define (g x) x)] (eq? f g))"));
	},


	testSgn: function() {
	    this.assert(isEqual(run("1"),
				run("(sgn 42)")));

	    this.assert(isEqual(run("-1"),
				run("(sgn -42)")));

	    this.assert(isEqual(run("0"),
				run("(sgn 0)")));
	},




	testCircularity: function() {
	    this.assert(isEqual(run("true"),
				run("(local [(define-struct p (f r)) " +
				    "        (define p1 (make-p 1 empty)) "+
				    "        (define p2 (make-p 1 empty))] "+
				    "  (begin " + 
				    "    (set-p-r! p1 p1)"+
				    "    (set-p-r! p2 p2)"+
				    "    (equal? p1 p2)))")));
				    

	    this.assert(isEqual(run("false"),
				run("(local [(define-struct p (f r)) " +
				    "        (define p1 (make-p 1 empty)) "+
				    "        (define p2 (make-p 2 empty))]"+
				    "  (begin " + 
				    "    (set-p-r! p1 p1)"+
				    "    (set-p-r! p2 p2)"+
				    "    (equal? p1 p2)))")));
				    

	    this.assert(isEqual(run("true"),
				run("(local [(define-struct p (f r))" +
				    "        (define p1 (make-p 1 (make-p 1 empty))) "+
				    "        (define p2 (make-p 1 empty))]"+
				    "  (begin " + 
				    "    (set-p-r! (p-r p1) p1)"+
				    "    (set-p-r! p2 p2)"+
				    "    (equal? p1 p2)))")));


	    this.assert(isEqual(run("false"),
				run("(local [(define-struct p (f r)) " +
				    "        (define p1 (make-p 1 (make-p 0 empty))) "+
				    "        (define p2 (make-p 1 empty))]"+
				    "  (begin " + 
				    "    (set-p-r! (p-r p1) p1)"+
				    "    (set-p-r! p2 p2)"+
				    "    (equal? p1 p2)))")));
				    


	    this.assert(isEqual(run("false"),
				run("(local [(define-struct p (f r)) " +
				    "        (define p1 (make-p 1 (make-p 0 (make-p 1 empty)))) "+
				    "        (define p2 (make-p 1 (make-p 0 empty)))]"+
				    "  (begin " + 
				    "    (set-p-r! (p-r (p-r p1)) p1)"+
				    "    (set-p-r! (p-r p2) p2)"+
				    "    (equal? p1 p2)))")));
				    


	    this.assert(isEqual(run("true"),
				run("(local [(define-struct p (f r)) " +
				    "        (define p1 (make-p 1 (make-p 0 (make-p 1 (make-p 0 empty))))) "+
				    "        (define p2 (make-p 1 (make-p 0 empty)))]"+
				    "  (begin " + 
				    "    (set-p-r! (p-r (p-r (p-r p1))) p1)"+
				    "    (set-p-r! (p-r p2) p2)"+
				    "    (equal? p1 p2)))")));
				    


	},
	

	testBareCond: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("cond")});
	},
	testBareCase: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("case")});
	},
	testBareLet: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("let")});
	},
	testBareLetStar: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("let*")});
	},
	testBareLetRec: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("letrec")});
	},
	testBareQuasiquote: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("quasiquote")});
	},
	testBareUnquote: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("unquote")});
	},
	testBareUnquoteSplicing: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("unquote-splicing")});
	},


	testBareLocal: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("local")});
	},

	testBareBegin: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("begin")});
	},
	testBareSetBang: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("set!")});
	},
	testBareIf: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("if")});
	},
	testBareAnd: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("and")});
	},
	testBareOr: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("or")});
	},

	testBareWhen: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("when")});
	},

	testBareUnless: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("unless")});
	},

	testBareLambda: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("lambda")});
	},

	testBareQuote: function() {
	    this.assertMobyRaise(isSyntaxNotApplied,
				 function() { run("quote")});
	},


	testUnsupportedLexicalToken: function() {
	    this.assertMobyRaise(isUnsupportedLexicalToken,
				 function() { run(".") });
	},


	testExerciseSyntaxErrorsAndDomGeneration: function() {
	    var errorRecords = 
		[["\"", isUnclosedLexicalToken],
		 // missing test on unrecognized-lexical-token
		 // missing test on unsupported-expression-form
		 [".", isUnsupportedLexicalToken],
		 ["(", isUnclosedParentheses],
		 [")", isClosingParenthesisBeforeOpener],
		 ["(]", isUnbalancedParenthesis],
		 ["begin", isSyntaxNotApplied],
		 ["(quote)", isQuoteTooFewElements],
		 ["(quote x y)", isQuoteTooManyElements],
		 ["(define (f x x) (* x x))", isDuplicateIdentifier],
		 ["(define x 42) (set! 43 x)", isExpectedIdentifier],
		 ["(define 42 x)", isGenericSyntacticError],
		 ["(lambda x 42)", isExpectedListOfIdentifiers],
		 ["x", isUndefinedIdentifier],
		 ["(define-struct foo ()) foo", isStructureIdentifierNotExpression],
		 ["(provide x)", isProvidedNameNotDefined],
		 ["(provide (struct-out x)) (define x 16)", isProvidedStructureNotStructure],
		 ["(require blah-is-not-a-known-module)", isUnknownModule],
		 ["(cond)", isConditionalMissingQuestionAnswer],
		 ["(cond 42)", isConditionalMalformedClause],
		 ["(cond [true])", isConditionalClauseTooFewElements],
		 ["(cond [true false false])", isConditionalClauseTooManyElements],
		 ["(cond [false 'ok])", isConditionalExhausted],
		 ["(cond [42 'huh?])", isBranchValueNotBoolean],
		 ["(if true 4 5 6)", isIfTooManyElements],
		 ["(if true)", isIfTooFewElements],
		 ["(if)", isIfTooFewElements],
		 ["(list ')", isMissingExpressionFollowingQuote],
		 ["(list #;)", isMissingExpressionFollowingQuote],
		 ["(quasiquote)", isQuasiquoteTooFewElements],
		 ["(quasiquote x y)", isQuasiquoteTooManyElements],
		 ["(quasiquote (unquote))", isUnquoteTooFewElements],
		 ["(quasiquote (unquote 1 2))", isUnquoteTooManyElements],
		 ["(quasiquote (list (unquote-splicing)))", isUnquoteSplicingTooFewElements],
		 ["(quasiquote (list (unquote-splicing (list) (list))))", isUnquoteSplicingTooManyElements],
		 ["(begin)", isBeginBodyEmpty],
		 ["(and)", isBooleanChainTooFewElements],
		 ["(lambda)", isLambdaTooFewElements],
		 ["(lambda (x) x x)", isLambdaTooManyElements],
		 ["(when)", isWhenNoBody],
		 ["(unless)", isUnlessNoBody],
		 ["(check-expect 3 4)", isCheckExpect],
		 ["(check-within 3 4 0.1)", isCheckWithin],
		 ["(check-error (/ 1 0) \"oh\")", isCheckError],
		 ["(check-error (* 1 0) \"oh\")", isCheckErrorNoError],
		 ["(sqr 3 4)", isApplicationArity],		
		 ["((+ 3) + 4)", isApplicationOperatorNotAFunction],
		 ["(3 + 4)", isApplicationOperatorNotAFunction],
		 ["(let ([f 42]) (f 3))", isApplicationOperatorNotAFunction],
		 ["(string-length 42)", isTypeMismatch],
		 ["(string-ref \"hi\" 17)", isIndexOutOfBounds]
		];
	    var errorToDom = plt.Kernel.invokeModule(
		"moby/runtime/error-struct-to-dom").
		EXPORTS['error-struct->dom-sexp'];
	    for(var i = 0; i < errorRecords.length; i++) {
		var errorString = errorRecords[i][0];
		var errorTest = errorRecords[i][1];
		this.assertMobyRaise(errorTest,
				     function() { run(errorString); },
				     errorString);
		var err;
		try {
		    run(errorString);
		} catch (e){
		    err = e;
		}
		if (! err) { continue; }
		
		try {
		    var domSexp = errorToDom(err, false);
		} catch (e) {
		    this.fail("Unable to produce dom for expression: " + errorString);
		}
	    }
	},



	testConstructEffectStructures: function() {
	    var that = this;
	    var testEffect = function(e) { that.assert(e['run'] !== 'undefined'); };
	    testEffect(run("(make-effect:none)"));
	    testEffect(run("(make-effect:beep)"));
	    testEffect(run("(make-effect:play-dtmf-tone 0 1)"));
	    testEffect(run("(make-effect:send-sms \"1234\" \"hello world\")"));
	    testEffect(run("(make-effect:play-sound \"test.wav\")"));
	    testEffect(run("(make-effect:pause-sound \"test.wav\")"));
	    testEffect(run("(make-effect:stop-sound \"test.wav\")"));
	    testEffect(run("(make-effect:set-sound-volume 0)"));
	    testEffect(run("(make-effect:raise-sound-volume)"));
	    testEffect(run("(make-effect:lower-sound-volume)"));
	    testEffect(run("(make-effect:set-wake-lock 0)"));
	    testEffect(run("(make-effect:release-wake-lock)"));
	    testEffect(run("(make-effect:pick-playlist (lambda (k) k))"));
	    testEffect(run("(make-effect:pick-random 0 (lambda (k) k))"));
	}




    });

}
