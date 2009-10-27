

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

	testTokenizeRational: function() {
	    this.assert(isEqual(tokenize("1/2"),
				[[["number", "1/2"]],
				 ""]));
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

	testReadRational2: function() {
	    this.assert(schemeIsEqual(read("3/4"),
				      cons(plt.types.Rational.makeInstance(3, 4),
					   empty)));
	    this.assert(schemeIsEqual(read("-5/17"),
				      cons(plt.types.Rational.makeInstance(-5, 17),
					   empty)));

	    this.assert(schemeIsEqual(read("5/10"),
				      cons(plt.types.Rational.makeInstance(1, 2),
					   empty)));
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

	testBlockySymbolInList: function() {
	    this.assert(schemeIsEqual(read("[hello again]"),
				      cons(cons(symbol("hello"), cons(symbol("again"), empty)),
					   empty)));
	},


	testReadHashComment: function() {
	    this.assert(schemeIsEqual(read("(hello #;42 again)"),
				      cons(cons(symbol("hello"), cons(symbol("again"), empty)),
					   empty)));
	},

	testReadHashComment2: function() {
	    this.assert(schemeIsEqual(read("(hello #;((())) again)"),
				      cons(cons(symbol("hello"), cons(symbol("again"), empty)),
					   empty)));
	},

	testReadHashComment3: function() {
	    this.assert(schemeIsEqual(read("(             #;'black blue )"),
				      cons(cons(symbol("blue"), empty),
					   empty)));
	},


	testReadHashComment4: function() {
	    this.assert(schemeIsEqual(read("(             #; black blue )"),
				      cons(cons(symbol("blue"), empty),
					   empty)));
	},


	testReadHashComment5: function() {
	    this.assert(schemeIsEqual(read("(             #; black  )"),
				      cons(empty, empty)));
	},



	testPipedComment: function() {
	    this.assert(schemeIsEqual(read("(hello #|((())|# again)"),
				      cons(cons(symbol("hello"), cons(symbol("again"), empty)),
					   empty)));

	},

	testPipedComment2: function() {
	    this.assert(schemeIsEqual(read("(hello #|(((#||##|))))|# again)"),
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
		read("    (");
	    } catch (e) {
		this.assertEqual(4, locOffset(e.loc));
	    }
	},

	testErrorLocsMismatching : function() {
	    this.assertRaise("MobyParserError", function() { read(" (]") });
	    try {
		read(" (]");
	    } catch (e) {
		this.assertEqual(2, locOffset(e.loc));
	    }
	}, 

	testReadDotError: function() {
	    this.assertRaise("MobyParserError",
			     function() { read(".") });

	    this.assertRaise("MobyParserError",
			     function() { read("(define f (lambda (x . y) y))"); });
	},


	testReadHashCommentError: function() {
	    this.assertRaise("MobyParserError",
			     function() { read("(hello #;") });
	    try {
		read("(hello #;");
	    } catch (e) {
		this.assertEqual(7, locOffset(e.loc));
	    }

	},

	testReadWithRationals: function() {
	    this.assert(schemeIsEqual(read("(check-within 22/7 pi 0.01)"),
				      (cons(cons(symbol("check-within"),
						 cons(plt.types.Rational.makeInstance(22, 7),
						      cons(symbol("pi"),
							   cons(makeFloat(0.01), empty)))),
					    empty))));
	},

	testInfinities: function() {
	    this.assert(schemeIsEqual(read("+inf.0"),
				      cons(plt.types.FloatPoint.makeInstance(Number.POSITIVE_INFINITY),
					   empty)));
	    this.assert(schemeIsEqual(read("-inf.0"),
				      cons(plt.types.FloatPoint.makeInstance(Number.NEGATIVE_INFINITY),
					   empty)));

	},


	testComplex: function() {
	    this.assert(schemeIsEqual(read("+1i"),
				      cons(plt.types.Complex.makeInstance(0, 1), empty)));

	    this.assert(schemeIsEqual(read("-1i"),
				      cons(plt.types.Complex.makeInstance(0, -1), empty)));

	    this.assert(schemeIsEqual(read("0+1i"),
				      cons(plt.types.Complex.makeInstance(0, 1), empty)));

	    this.assert(schemeIsEqual(read("-0+1i"),
				      cons(plt.types.Complex.makeInstance(0, 1), empty)));

	    this.assert(schemeIsEqual(read("-0-1i"),
				      cons(plt.types.Complex.makeInstance(0, -1), empty)));

	    this.assert(schemeIsEqual(read("5-1i"),
				      cons(plt.types.Complex.makeInstance(5, -1), empty)));

	    this.assert(schemeIsEqual(read("-5-1i"),
				      cons(plt.types.Complex.makeInstance(-5, -1), empty)));
	}
	


    }); 
}