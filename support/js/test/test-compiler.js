function init() {

    function isEqual(x, y) {
	return plt.Kernel.equal_question_(x, y);
    }

    function cons(x, y) {
	return plt.Kernel.cons(x, y);
    }

    var empty = plt.Danny Yoo <dyoo@cs.wpi.edu>types.Empty.EMPTY;
    var number = function(x) { return plt.types.Rational.makeInstance(x, 1); };
    var symbol = plt.types.Symbol.makeInstance;

    var TRUE = plt.types.Logic.TRUE;
    var FALSE = plt.types.Logic.FALSE;

    // run: string -> scheme-value
    // Evaluates the string and produces the evaluated scheme value.
    function run(aSource) {
	var pinfo = get_dash_base_dash_pinfo(symbol("moby"));
	var namespace = new Namespace();
	var program = readSchemeExpressions(aSource);
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
		this.assert("foo",
			    run("(if true \"foo\" \"bar\")"));
		this.assert("bar",
			    run("(if false \"foo\" \"bar\")"));
	    },

	    testSimpleFunctionDefinition: function() {
		this.assert(number(9),
			    run("(define (f x) (* x x))" +
				"(f 3)"));
			    
	    },

	    testBegin: function() {
		// normal behaviour
		this.assert(TRUE,
			    run("(begin (+ 1 2) (+ 3 4) true)"));

		// sequencial evaluation
		this.assert(number(3),
			    run("(define x 5)(define y 3)" +
				"(begin (set! x y) (set! y x) y)"));
	    },

	    testBoxMutation: function() {
		this.assert(number(2),
			    run("(define bx (box 5))" +
				"(begin (set-box! bx 2)" +
					"(unbox bx))"));
		this.assertRaise("MobyTypeError",
				function () {
				 run("(define bx (box 5))" +
				     "(begin (set-box! 2 bx)" +
					"(unbox bx))")});
	    },

	    testStructureMutators: function() {
		this.assert(number(9),
			    run("(define ps (make-posn 3 4))" +
				"(begin (set-posn-x! ps 9) " +
					"(posn-x ps))"));
		this.assertRaise("MobyTypeError",
				function () {
			    	  run("(define ps (make-posn 3 4))" +
				      "(begin (set-posn-x! 5 ps) " +
					   "(posn-x ps))")});
	    },

	    testSet: function() {
		// normal behaviour
		this.assert("a",
			    run("(define x 5)" +
				"(begin (set! x \"a\") x)"));

		// undefined top-level variable
		this.assertRaise("MobyRuntimeError",
				function () {
				 run("(begin (set! c 42) 3)")});

		// function arguments are mutable
		this.assertRaise(number(3),
		                 run("(define (f x) (begin (set! x 3) x)) (f 42)"));

		// scoping
		// this seems like a test of "local" instead of "set"
		this.assert(number(42),
				   run("(define a 42)" +
				       "(define (f x) " +
					 "(local ((define a 3)) " +
					   "(begin (set! a x) a)))" +
	      			       "(begin (f 21) a)"));
	    }
    }); 
}
