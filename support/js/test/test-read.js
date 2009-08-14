

function init() {
    function isArray(x) {
	return typeof(x) == 'object' && 'length' in x;
    }

    function isEqual(x, y) {
	if (isArray(x) && isArray(y)) {
	    if (x.length == y.length) {
		for (var i = 0; i < x.length; i++) {
		    if (! isEqual(x[i], y[i])) {
			return false;
		    }
		}
		return true;
	    } else {
		return false;
	    }
	} else if (!isArray(x) && !isArray(y)) {
	    return x == y;         
	} else {
	    return false;
	}
    }

    function schemeIsEqual(x, y) {
	return plt.Kernel.equal_question_(x, y);
    }
    function cons(x, y) {
	return plt.Kernel.cons(x, y);
    }


    var empty = plt.types.Empty.EMPTY;
    var number = function(x) { return plt.types.Rational.makeInstance(x, 1); };
    var makeFloat = function(x) { return plt.types.FloatPoint.makeInstance(x); };
    var symbol = plt.types.Symbol.makeInstance;


    return new Test.Unit.Runner({
	    setup: function() {},
    
		teardown: function() {},
    
		testTokenizeSymbol: function() {
		this.assert(isEqual(tokenize("hello"),
				    [[["symbol", "hello"]], ""]));
	    },

		testTokenizeSimpleList: function() {
		this.assert(isEqual(tokenize("(hello world)"),
				    [[["(", "("],
				      ["symbol", "hello"],
				      ["symbol", "world"],
				      [")", ")"]], ""]));
	    },

		testTokenizeSimpleListAndString: function() {
		this.assert(isEqual(tokenize("(hello \"world\")"),
				    [[["(", "("],
				      ["symbol", "hello"],
				      ["string", "world"],
				      [")", ")"]], ""]));
	    },
   
		testReadSymbol: function() {
		this.assert(schemeIsEqual(readSchemeExpressions("hello"),
					  cons(symbol("hello"),
					       empty)));
	    },

	testReadRational: function() {
	    this.assert(schemeIsEqual(readSchemeExpressions("1"),
				      cons(number(1), empty)));
				      
	},


	testReadFloat: function() {
	    this.assert(schemeIsEqual(readSchemeExpressions("0.1"),
				      cons(makeFloat(0.1), empty)));
				      
	},

		testReadSymbolInList: function() {
		this.assert(schemeIsEqual(readSchemeExpressions("(hello again)"),
					  cons(cons(symbol("hello"), cons(symbol("again"), empty)),
					       empty)));
	    },


		testReadListOfNumbers: function() {
		this.assert(schemeIsEqual(readSchemeExpressions("(1) (2 3)"),
					  cons(cons(number(1), empty),
					       cons(cons(number(2), cons(number(3), empty)),
						    empty))));

		this.assert(schemeIsEqual(readSchemeExpressions("(1.4) (2 3)"),
					  cons(cons(number(1.4), empty),
					       cons(cons(number(2), cons(number(3), empty)),
						    empty))));

		this.assert(schemeIsEqual(readSchemeExpressions("(1.4) (2 3)"),
					  cons(cons(number(1.4), empty),
					       cons(cons(number(2), cons(number(3), empty)),
						    empty))));
	    }

    }); 
}