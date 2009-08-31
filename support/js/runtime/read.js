// Depends on kernel.js, stx.ss

var plt = plt || {};

plt.reader = {};


(function(){


    // replaceEscapes: string -> string
    var replaceEscapes = function(s) {
	return s.replace(/\\./g, function(match, submatch, index) {
	    switch(match) {
	    case '\\b': return "\b";
	    case '\\f': return "\f";
	    case '\\n': return "\n";
	    case '\\r': return "\r";
	    case '\\t': return "\t";
	    case '\\v': return "\v";
	    default:
		return match.substring(1);
	    }
	    // FIXME: add more escape sequences.
	});
    }

    var countLines = function(s) {
	var i;
	var c = 0;
	for(i = 0; i < s.length; i++) {
	    if (s[i] == '\n') { 
		c++; 
	    }
	}
	return c;
    }

    var tokenize = function(s, source) {

	var offset = 0;
	var line = 1;
	var tokens = [];
	var PATTERNS = [['whitespace' , /^(\s+)/],
			['#;', /^#;/],
			['comment' , // piped comments
			 new RegExp("^[#][|]"+
				    "(?:(?:\\|[^\\#])|[^\\|])*"+
				    "[|][#]")],
			['comment' , /(^;[^\n]*)/],
			['(' , /^(\(|\[|\{)/],
			[')' , /^(\)|\]|\})/],
			['\'' , /^(\')/],
			['`' , /^(`)/],
			[',' , /^(,)/],
			['char', /^\#\\(newline)/],
			['char', /^\#\\(.)/],
			['number' , /^([+\-]?(?:\d+\.\d+|\d+\.|\.\d+|\d+))/],
			['string' , new RegExp("^\"((?:([^\\\\\"]|(\\\\.)))*)\"")],      
			['symbol' ,/^([a-zA-Z\:\+\=\~\_\?\!\@\#\$\%\^\&\*\-\/\.\>\<][\w\:\+\=\~\_\?\!\@\#\$\%\^\&\*\-\/\.\>\<]*)/]
		       ];

	if (! source) { source = ""; }
	
	while (true) {
	    var shouldContinue = false;
	    for (var i = 0; i < PATTERNS.length; i++) {
		var patternName = PATTERNS[i][0];
		var pattern = PATTERNS[i][1]
		var result = s.match(pattern);
		if (result != null) {
		    if (patternName == 'string') {
			result[1] = replaceEscapes(result[1]);
		    }
		    if (patternName != 'whitespace' && patternName != 'comment') {
			tokens.push([patternName, 
				     result[1], 
				     new Loc(offset,
					     line,
					     result[0].length, 
					     source)]);
		    }
		    offset = offset + result[0].length;
		    line = line + countLines(result[0]);
		    s = s.substring(result[0].length);
		    shouldContinue = true;
		}
	    }
	    if (! shouldContinue) {
		break;
	    }
	}
	return [tokens, s];
    }


    // readSchemeExpressions: string string -> (listof stx)
    var readSchemeExpressions = function(s, source) {
	var makeList = make_dash_stx_colon_list;
	var makeAtom = make_dash_stx_colon_atom;

	var tokensAndError = tokenize(s, source);
	var tokens = tokensAndError[0];

	var lastToken = undefined;

	if (tokensAndError[1].length > 0) {
	    throw new plt.Kernel.MobyParserError(
		"Error while tokenizing: the rest of the stream is: " +
		    tokensAndError[1],
		new Loc(s.length - tokensAndError[1].length,
			countLines(s.substring(
			    0, s.length - tokensAndError[1].length)),
			tokensAndError[1].length,
			source));
	}
	
	var quoteSymbol = plt.types.Symbol.makeInstance("quote");
	var quasiquoteSymbol = plt.types.Symbol.makeInstance("quasiquote");
	var unquoteSymbol = plt.types.Symbol.makeInstance("unquote");
	var empty = plt.types.Empty.EMPTY;

	function isType(type) {
	    return (tokens.length > 0 && tokens[0][0] == type);
	}
	
	function eat(expectedType) {
	    if (tokens.length == 0) {
		if (lastToken) { 
		    throw new plt.Kernel.MobyParserError(
			"token stream exhausted while trying to eat " +
			    expectedType,
			lastToken[2]);
		} else {
		    throw new plt.Kernel.MobyParserError(
			"token stream exhausted while trying to eat " +
			    expectedType,
			new Loc(0, 0, s.length, source));
		}
	    }
	    var t = tokens.shift();
	    lastToken = t;
	    if (t[0] == expectedType) {
		return t;
	    } else {
		throw new plt.Kernel.MobyParserError(
		    "Unexpected token " + t,
		    t[2]);
	    }
	}


	// NOTE: we define things in this funny way because of a bug in 
	// Firefox 3.5.1 that says the error "can't access optimized closure"
	var readExpr;
	var readExprs;
	var readQuoted;

	readQuoted = function(quoteChar, quoteSymbol) {
	    var leadingQuote = eat(quoteChar);
	    var quoted = readExpr();
	    return makeList(plt.Kernel.cons(
		makeAtom(quoteSymbol, leadingQuote[2]),
		plt.Kernel.cons(quoted, empty)),
			    new Loc(leadingQuote[2].offset,
				    leadingQuote[2].line,
				    (quoted.loc.offset -
				     leadingQuote[2].offset +
				     quoted.loc.span),
				    ""));
	};

	var getParenRShape = function(lparen) {
	    switch(lparen) {
	    case '(': return ')';
	    case '[' : return ']';
	    case '{' : return '}';
	    }
	}


	// readExpr: -> stx
	readExpr = function() {
	    if (tokens.length == 0) {
		if (lastToken) { 
		    throw new plt.Kernel.MobyParserError(
			"Parse broke with empty token stream",
			lastToken[2]);
		} else {
		    throw new plt.Kernel.MobyParserError(
			"Parse broke with empty token stream",
			new Loc(0, 0, s.length, source));
		}
	    }

	    switch(tokens[0][0]) {

	    case '(': 
		var lparen = eat('(');
		var lshape = lparen[1];
		var rshape = getParenRShape(lparen[1]);
		var result = readExprs();
		if (tokens.length == 0) {
		    throw new plt.Kernel.MobyParserError(
			"Expected a " + rshape + " to close " + lshape,
			lparen[2]);
		} else if (tokens[0][1] != rshape) {
		    throw new plt.Kernel.MobyParserError(
			"Expected a " + rshape + " to close " + lshape,
			tokens[0][2]);
		}
		var rparen = eat(rshape);
		return make_dash_stx_colon_list(
		    result,
		    new Loc(lparen[2].offset,
			    lparen[2].line,
			    rparen[2].offset - lparen[2].offset + 1,
			    ""));

	    case '#;':
		var hashcomment = eat('#;');
		var skippingExpr = readExpr();
		return readExpr();
		
	    case '\'':
		return readQuoted("'", quoteSymbol);

	    case '`':
		return readQuoted("`", quasiquoteSymbol);

	    case ',':
		return readQuoted(",", unquoteSymbol);

	    case 'number':
		var t = eat('number');
		if (t[1].match(/\./)) {
		    return makeAtom(plt.types.FloatPoint.makeInstance(parseFloat(t[1])), 
				    t[2]);
		} else {
		    return makeAtom(plt.types.Rational.makeInstance(parseInt(t[1]), 1), 
				    t[2]);
		}
	    case 'string':
		var t = eat('string');
		return makeAtom(plt.types.String.makeInstance(t[1]),
				t.loc);
	    case 'char':
		var t = eat('char');
		if (t[1] == 'newline') {
		    return makeAtom(plt.types.Char.makeInstance('\n'), t[2]);
		}
		else {
		    return makeAtom(plt.types.Char.makeInstance(t[1]), t[2]);
		}

	    case 'symbol':
		var t = eat('symbol');
		return makeAtom(plt.types.Symbol.makeInstance(t[1]), t[2]);

	    default:
		throw new plt.Kernel.MobyParserError
		("Parse broke with token stream " + tokens, 
		 tokens[0][2]);
	    }
	};



	// readExprs: (listof stx)
	readExprs = function() {
	    var result = plt.types.Empty.EMPTY;
	    while (true) {
		if (tokens.length == 0 || isType(')')) {
		    break;
		} else {
		    var nextElt = readExpr();
		    result = plt.types.Cons.makeInstance(nextElt, result);
		}
	    }
	    return plt.Kernel.reverse(result);
	};
	


	var result = readExprs();
	if (tokens.length > 0) {
	    throw new plt.Kernel.MobyParserError
	    ("More elements in the program's token stream than expected: "+
	     "the next unconsumed token is: "  + tokens[0][1],
	     tokens[0][2])
	}
	return result;
    }
    



    // provides:
    plt.reader.tokenize = tokenize;
    plt.reader.readSchemeExpressions = readSchemeExpressions;
}());
