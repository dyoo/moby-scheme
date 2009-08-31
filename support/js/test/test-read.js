

function init() {

    var locOffset = function(loc) {
	return Loc_dash_offset(loc);
    }

    var locSpan = function(loc) {
	return Loc_dash_span(loc);
    }




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

    var stxToDatum = function(aStx) {
	return stx_dash__greaterthan_datum(aStx);
    }

    var tokenize = function(s) {
	var tokensAndLocs = plt.reader.tokenize(s)[0];
	var result = [];
	for (var i = 0; i < tokensAndLocs.length; i++) {
	    result.push([tokensAndLocs[i][0], tokensAndLocs[i][1]]);
	}

	return [result, plt.reader.tokenize(s)[1]];
    }

    var read = function(s) {
	return plt.Kernel.map(function(args) { 
	    return stxToDatum(args[0]) },
			      [plt.reader.readSchemeExpressions(s)]);
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
	    this.assert(schemeIsEqual(read("hello"),
				      cons(symbol("hello"),
					   empty)));
	},

	testReadRational: function() {
	    this.assert(schemeIsEqual(read("1"),
				      cons(number(1), empty)));
	    
	},


	testReadFloat: function() {
	    this.assert(schemeIsEqual(read("0.1"),
				      cons(makeFloat(0.1), empty)));
	    
	},

	testReadSymbolInList: function() {
	    this.assert(schemeIsEqual(read("(hello again)"),
				      cons(cons(symbol("hello"), cons(symbol("again"), empty)),
					   empty)));
	},


	testReadListOfNumbers: function() {
	    this.assert(schemeIsEqual(read("(1) (2 3)"),
				      cons(cons(number(1), empty),
					   cons(cons(number(2), cons(number(3), empty)),
						empty))));

	    this.assert(schemeIsEqual(read("(1.4) (2 3)"),
				      cons(cons(number(1.4), empty),
					   cons(cons(number(2), cons(number(3), empty)),
						empty))));

	    this.assert(schemeIsEqual(read("(1.4) (2 3)"),
				      cons(cons(number(1.4), empty),
					   cons(cons(number(2), cons(number(3), empty)),
						empty))));
	},


	testErrorLocsExtraParen : function() {
	    this.assertRaise("MobyParserError", function() { read("   ())") });
	    try {
		read("   ())");
	    } catch (e) {
		this.assertEqual(5, locOffset(e.loc));
	    }
	},


	testErrorLocsUnclosedParen : function() {
	    this.assertRaise("MobyParserError", function() { read("    (") });
	    try {
		read("    (")
	    } catch (e) {
		this.assertEqual(4, locOffset(e.loc));
	    }
	},

	testErrorLocsMismatching : function() {
	    this.assertRaise("MobyParserError", function() { read(" (]") });
	    try {
		read(" (]")
	    } catch (e) {
		this.assertEqual(2, locOffset(e.loc));
	    }
	}



    }); 
}