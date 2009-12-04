// Depends on kernel.js, stx.ss

if (typeof(plt) === 'undefined') { var plt = {}; }

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

    var delimiter = "[\\s\\\(\\\)\\\[\\\]\\\{\\\}\\\"\\\,\\\'\\\`\\\;]";
    var nondelimiter = "[^\\s\\\(\\\)\\\[\\\]\\\{\\\}\\\"\\\,\\\'\\\`\\\;]";


    var numberHeader = ("(?:(?:\\d+\\/\\d+)|"+
			(  "(?:(?:\\d+\\.\\d+|\\d+\\.|\\.\\d+)(?:[eE][+\\-]?\\d+)?)|")+
			(  "(?:\\d+(?:[eE][+\\-]?\\d+)?))"));


    var PATTERNS = [['whitespace' , /^(\s+)/],
		    ['#;', /^([#][;])/],
		    ['comment' , // piped comments
		     new RegExp("^([#][|]"+
				"(?:(?:\\|[^\\#])|[^\\|])*"+
				"[|][#])")],
		    ['comment' , /(^;[^\n]*)/],
		    ['(' , /^(\(|\[|\{)/],
		    [')' , /^(\)|\]|\})/],
		    ['\'' , /^(\')/],
		    ['`' , /^(`)/],
		    [',@' , /^(,@)/],
		    [',' , /^(,)/],
		    ['char', /^\#\\(newline|backspace)/],
		    ['char', /^\#\\(.)/],
		    ['string' , new RegExp("^\"((?:([^\\\\\"]|(\\\\.)))*)\"")],
		    ['symbol-or-number', new RegExp("^(" + nondelimiter + "+)")]
		    ];


    var numberPatterns = [['complex' , new RegExp("^((?:(?:\\#[ei])?[+\\-]?" + numberHeader +")?"
						  + "(?:[+\\-]" + numberHeader + ")i$)")],
			  ['number' , /^((?:\#[ei])?[+-]inf.0)$/],
			  ['number' , /^((?:\#[ei])?[+-]nan.0)$/],
			  ['number' , new RegExp("^((?:\\#[ei])?[+\\-]?" + numberHeader + "$)")]];
	

    var tokenize = function(s, source) {

	var offset = 0;
	var line = 1;
	var tokens = [];

	if (! source) { source = ""; }
	
	while (true) {
	    var shouldContinue = false;
	    for (var i = 0; i < PATTERNS.length; i++) {
		var patternName = PATTERNS[i][0];
		var pattern = PATTERNS[i][1]
		var result = s.match(pattern);
		if (result != null) {
		    var wholeMatch = result[0];
		    var tokenText = result[1];
		    if (patternName == 'string') {
			tokenText = replaceEscapes(tokenText);
		    } 
		    if (patternName == "symbol-or-number") {
			var isNumber = false;
			for (var j = 0; j < numberPatterns.length; j++) {
			    var numberMatch = tokenText.match(numberPatterns[j][1]);
			    if (numberMatch) {
				tokens.push([numberPatterns[j][0], 
					     tokenText,
					     new Loc(offset,
						     line,
						     wholeMatch.length, 
						     source)]);
				isNumber = true;
				break;
			    }
			}
			if (! isNumber) {
			    tokens.push(["symbol", 
					 tokenText,
					 new Loc(offset,
						 line,
						 wholeMatch.length, 
						 source)]);
			}
		    } else if (patternName != 'whitespace' && patternName != 'comment') {
			tokens.push([patternName, 
				     tokenText,
				     new Loc(offset,
					     line,
					     wholeMatch.length, 
					     source)]);
		    }

		    offset = offset + wholeMatch.length;
		    line = line + countLines(wholeMatch);
		    s = s.substring(wholeMatch.length);
		    shouldContinue = true;
		    break;
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
	var unquoteSplicingSymbol = plt.types.Symbol.makeInstance("unquote-splicing");
	var empty = plt.types.Empty.EMPTY;

	var isType = function(type) {
	    return (tokens.length > 0 && tokens[0][0] == type);
	}
	
	var eat = function(expectedType) {
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
	    default: throw new Error();
	    }
	}



	// parseBasicNumber: string -> plt.types.Number
	// Reads a non-complex number.
	var parseBasicNumber = function(text, isExact) {
	    var rationalMatch = text.match(/([+\-]?\d+)\/(\d+)/);
	    if (text == '+inf.0') {
		return plt.types.FloatPoint.makeInstance(Number.POSITIVE_INFINITY);
	    } else if (text == '-inf.0') {
		return plt.types.FloatPoint.makeInstance(Number.NEGATIVE_INFINITY);
	    } else if (text == "+nan.0" || text == '-nan.0') {
		return plt.types.FloatPoint.makeInstance(Number.NaN);
	    } else if (text.match(/[eE]/)) {
		return plt.types.FloatPoint.makeInstance(parseFloat(text));
	    } else if (text.match(/\./)) {
		if (isExact) {
		    var decimalMatch = text.match("^(.*)[.](.*)$");
		    var whole = plt.types.Rational.makeInstance(parseInt(decimalMatch[1] || "0") || 0);
		    if (decimalMatch[1].match(/-/)) {
			return plt.types.NumberTower.subtract(whole,
			  plt.types.Rational.makeInstance(
			      parseInt(decimalMatch[2]), 
			      Math.pow(10, decimalMatch[2].length)));
		    } else {
			return plt.types.NumberTower.add(whole,
			  plt.types.Rational.makeInstance(
			      parseInt(decimalMatch[2]), 
			      Math.pow(10, decimalMatch[2].length)));
		    }
		} else {
		    return plt.types.FloatPoint.makeInstance(parseFloat(text));
		}
	    } else if (rationalMatch) {
		if (isExact) {
		    return plt.types.Rational.makeInstance(parseInt(rationalMatch[1]),
							   parseInt(rationalMatch[2]));
		} else {
		    return plt.types.FloatPoint.makeInstance(parseInt(rationalMatch[1])/
							     parseInt(rationalMatch[2]));
		}
	    } else {
		if (isExact) {
		    return plt.types.Rational.makeInstance(parseInt(text), 1);
		} else {
		    return plt.types.FloatPoint.makeInstance(parseInt(text));
		}
	    }
	};


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
		var rparen = eat(')');
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

	    case ',@':
		return readQuoted(",@", unquoteSplicingSymbol);


	    case 'number':
		var t = eat('number');
		var exactnessMatch = t[1].match(/^(\#[ie])(.+)$/);
		if (exactnessMatch) {
		    if (exactnessMatch[1] == "#i") {
			return makeAtom(parseBasicNumber(exactnessMatch[2], false),
					t[2]);
		    } else {
			return makeAtom(parseBasicNumber(exactnessMatch[2], true),
					t[2]);
		    }
		} else {
		    return makeAtom(parseBasicNumber(t[1], true),
				    t[2]);
		}

	    case 'complex':
		var t = eat('complex');
		var complexMatch = t[1].match(/^((?:\#[ei])?)([+\-]?(?:\d+\/\d+|\d+\.\d+|\d+\.|\.\d+|\d+)?)([+\-](?:\d+\/\d+|\d+\.\d+|\d+\.|\.\d+|\d+))i/);
		var exactness = (complexMatch[1] == "#i" ? false : true);
		var a = (complexMatch[2] != "" ?
			 parseBasicNumber(complexMatch[2], exactness) :
			 plt.types.Rational.ZERO);
		var b = parseBasicNumber(complexMatch[3], exactness);
		// FIXME: Complex needs to be changed so it takes in either
		// exact or inexact basic values.
	        var newAtom = makeAtom(plt.types.Complex.makeInstance(a, b));
  	        return newAtom;

	    case 'string':
		var t = eat('string');
		return makeAtom(plt.types.String.makeInstance(t[1]),
				t[2]);
	    case 'char':
		var t = eat('char');
		if (t[1] == 'newline') {
		    return makeAtom(plt.types.Char.makeInstance('\n'), t[2]);
		} else if (t[1] == 'backspace') {
		    // FIXME: add more character constants.
		    // See: http://docs.plt-scheme.org/reference/reader.html#%28idx._%28gentag._172._%28lib._scribblings/reference/reference..scrbl%29%29%29
		    return makeAtom(plt.types.Char.makeInstance(String.fromCharCode(8)), t[2]);
		} else {
		    return makeAtom(plt.types.Char.makeInstance(t[1]), t[2]);
		}

	    case 'symbol':
		var t = eat('symbol');
		if (t[1] == '.') {
		    throw new plt.Kernel.MobyParserError
		    ("Dotted pairs are not currently accepted by Moby", t[2]);
		}
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
		} else if (isType('#;')){
		    eat('#;');
		    var skippingExpr = readExpr();
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
