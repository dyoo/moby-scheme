function init() {

    function isEqual(x, y) {
	return plt.Kernel.equal_question_(x, y);
    }

    function cons(x, y) {
	return plt.Kernel.cons(x, y);
    }
    var empty = plt.types.Empty.EMPTY;
    var number = function(x) { return plt.types.Rational.makeInstance(x, 1); };
    var symbol = plt.types.Symbol.makeInstance;


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
			    
	    }
    }); 
}