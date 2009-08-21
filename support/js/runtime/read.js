var readSchemeExpressions;
var tokenize;
var StxLocation;



(function(){

    function StxLoc(offset, span) {
	this.offset = offset;
	this.span = span;
    }


    // replaceEscapes: string -> string
    function replaceEscapes(s) {
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


    tokenize = function(s) {
	var offset = 0;
	var tokens = [];
	var PATTERNS = [['whitespace' , /^(\s+)/],
			['comment' , /(^;[^\n]*)/],
			['(' , /^(\(|\[)/],
			[')' , /^(\)|\])/],
			['\'' , /^(\')/],
			['`' , /^(`)/],
			[',' , /^(,)/],
			['char', /^\#\\(newline)/],
			['char', /^\#\\(.)/],
			['number' , /^([+\-]?(?:\d+\.\d+|\d+\.|\.\d+|\d+))/],
			['string' , new RegExp("^\"((?:([^\\\\\"]|(\\\\.)))*)\"")],      
			['symbol' ,/^([a-zA-Z\:\+\=\~\_\?\!\@\#\$\%\^\&\*\-\/\.\>\<][\w\:\+\=\~\_\?\!\@\#\$\%\^\&\*\-\/\.\>\<]*)/]
		       ];
	
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
				     new StxLoc(offset,
						result[0].length)]);
		    }
		    offset = offset + result[0].length;
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



    readSchemeExpressions = function(s) {
	var makeList = make_dash_stx_colon_list;
	var makeAtom = make_dash_stx_colon_atom;

	var tokensAndError = tokenize(s);
	var tokens = tokensAndError[0];
	if (tokensAndError[1].length > 0) {
	    throw new Error("Error while tokenizing: the rest of the stream is: " + tokensAndError[1]);
	}

	var quoteSymbol = plt.types.Symbol.makeInstance("quote");
	var quasiquoteSymbol = plt.types.Symbol.makeInstance("quasiquote");
	var unquoteSymbol = plt.types.Symbol.makeInstance("unquote");
	var empty = plt.types.Empty.EMPTY;

	function isType(type) {
	    return (tokens.length > 0 && tokens[0][0] == type);
	}
	
	function eat(expectedType) {
	    if (tokens.length == 0)
		throw new Error("token stream exhausted while trying to eat " +
				expectedType);
	    var t = tokens.shift();
	    if (t[0] == expectedType) {
		return t;
	    } else {
		throw new Error("Unexpected token " + t);
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
			    new StxLoc(leadingQuote[2].offset,
				       (quoted.loc.offset -
					leadingQuote[2].offset +
					quoted.loc.span)));
	};


	// readExpr: -> stx
	readExpr = function() {
	    if (tokens.length == 0) {
		throw new Error("Parse broke with token stream " + tokens);
	    }

	    switch(tokens[0][0]) {

	    case '(': 
		var lparen = eat('(');
		var result = readExprs();
		var rparen = eat(')');
		return make_dash_stx_colon_list(
		    result,
		    new StxLoc(lparen[2].offset,
			       rparen[2].offset - lparen[2].offset + 1));

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
		throw new Error("Parse broke with token stream " + tokens);
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
	    throw new Error("More elements in the program's token stream than expected: the next unconsumed token is: "  + tokens[0][1])
	}
	return result;
    }
    
}());
