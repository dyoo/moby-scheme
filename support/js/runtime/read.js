// Depends on types.js, stx.js
// goog.provide('plt.reader');




    if (typeof(plt) === 'undefined') { plt = {}; }
if (! plt.reader) { plt.reader = {}; }


(function(){

    var stxModule = plt.Kernel.invokeModule("moby/runtime/stx");
    var Loc = stxModule.EXPORTS['Loc'];
    var datumToStx = function(thing, loc) {
	var context = false;
	return stxModule.EXPORTS['datum->stx'](context, 
					       thing, 
					       loc);
    };
    var stx_dash_loc = stxModule.EXPORTS['stx-loc'];

    var Loc_dash_offset = stxModule.EXPORTS['Loc-offset'];
    var Loc_dash_line = stxModule.EXPORTS['Loc-line'];
    var Loc_dash_column = stxModule.EXPORTS['Loc-column'];
    var Loc_dash_offset = stxModule.EXPORTS['Loc-offset'];
    var Loc_dash_span = stxModule.EXPORTS['Loc-span'];


    var num = plt.types.Rational.makeInstance;



    //////////////////////////////////////////////////////////////////////
    // Tokenizer

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


    var computeColumn = function(s, col) {
	var i;
	for(i = 0; i < s.length; i++) {
	    if (s[i] == '\n') { 
		col = 0; 
	    } else {
		col++;
	    }
	}
	return col;
    }


    var nondelimiter = "[^\\s\\\(\\\)\\\[\\\]\\\{\\\}\\\"\\\,\\\'\\\`\\\;]";



    // error tokens have the type "incomplete-token".

    var PATTERNS = [['whitespace' , /^(\s+)/],
		    ['#;', /^([#][;])/],
		    ['comment' , // piped comments
		     new RegExp("^([#][|]"+
				"(?:(?:\\|[^\\#])|[^\\|])*"+
				"[|][#])")],
		    ['comment' , /(^;[^\n]*)/],
		    ['incomplete-pipe-comment', new RegExp("^[#][|]")],  // unclosed pipe comment
		    ['(' , /^(\(|\[|\{)/],
		    [')' , /^(\)|\]|\})/],
		    ['\'' , /^(\')/],
		    ['`' , /^(`)/],
		    [',@' , /^(,@)/],
		    [',' , /^(,)/],
		    ['char', /^\#\\(newline|backspace)/],
		    ['char', /^\#\\(.)/],
		    ['string' , new RegExp("^\"((?:([^\\\\\"]|(\\\\.)))*)\"")],
		    ['incomplete-string', new RegExp("^\"")],      // unclosed string
		    ['symbol-or-number', new RegExp("^(" + nondelimiter + "+)")]

		   ];


    var numberHeader = ("(?:(?:\\d+\\/\\d+)|"+
			(  "(?:(?:\\d+\\.\\d+|\\d+\\.|\\.\\d+)(?:[eE][+\\-]?\\d+)?)|")+
			(  "(?:\\d+(?:[eE][+\\-]?\\d+)?))"));

    var numberPatterns = [['complex' , new RegExp("^((?:(?:\\#[ei])?[+\\-]?" + numberHeader +")?"
						  + "(?:[+\\-]" + numberHeader + ")i$)")],
			  ['number' , /^((?:\#[ei])?[+-]inf.0)$/],
			  ['number' , /^((?:\#[ei])?[+-]nan.0)$/],
			  ['number' , new RegExp("^((?:\\#[ei])?[+\\-]?" + numberHeader + "$)")]];
    

    var tokenize = function(s, source) {

	var offset = 0;
	var line = 1;
	var column = 0;
	var tokens = [];

	if (! source) { source = ""; }
	
	while (true) {
	    var shouldContinue = false;
	    for (var i = 0; i < PATTERNS.length; i++) {
		var patternName = PATTERNS[i][0];
		var pattern = PATTERNS[i][1];
		var result = s.match(pattern);
		if (result != null) {
		    var wholeMatch = result[0];
		    var tokenText = result[1];
		    if (patternName === "incomplete-string") {
			plt.types.throwMobyError(new Loc(num(offset),
							 num(line),
							 num(column),
							 num(wholeMatch.length),
							 source),
						 "make-moby-error-type:unclosed-lexical-token",
						 ["string",
						  plt.types.Symbol.makeInstance("\""),
						  plt.types.Symbol.makeInstance("\"")]);
		    }
		    if (patternName === "incomplete-pipe-comment") {
			plt.types.throwMobyError(new Loc(num(offset),
							 num(line),
							 num(column),
							 num(wholeMatch.length),
							 source),
						 "make-moby-error-type:unclosed-lexical-token",
						 ["comment",
						  plt.types.Symbol.makeInstance("#|"),
						  plt.types.Symbol.makeInstance("|#")]);
		    }
		    
		    if (patternName == 'string') {
			tokenText = replaceEscapes(tokenText);
		    } 

		    
		    if (patternName == "symbol-or-number") {
			var isNumber = false;
			for (var j = 0; j < numberPatterns.length; j++) {
			    var numberMatch = tokenText.match(numberPatterns[j][1]);
			    if (numberMatch) {
				tokens.push({type: numberPatterns[j][0],
					     text: tokenText,
					     loc: new Loc(num(offset),
							  num(line),
							  num(column),
							  num(wholeMatch.length), 
							  source)});
				isNumber = true;
				break;
			    }
			}
			if (! isNumber) {
			    tokens.push({type: "symbol", 
					 text: tokenText,
					 loc: new Loc(num(offset),
						      num(line),
						      num(column),
						      num(wholeMatch.length), 
						      source)});
			}
		    } else if (patternName != 'whitespace' && patternName != 'comment') {
			tokens.push({type: patternName, 
				     text: tokenText,
				     loc: new Loc(num(offset),
						  num(line),
						  num(column),
						  num(wholeMatch.length), 
						  source)});
		    }

		    offset = offset + wholeMatch.length;
		    column = computeColumn(wholeMatch, column);
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



    //////////////////////////////////////////////////////////////////////

    // Parser



    // readSchemeExpressions: string string -> (listof stx)
    var readSchemeExpressions = function(s, source) {
	if (source === undefined) {
	    source = "<unknown source>";
	}
	var stxLoc = stx_dash_loc;

	var tokensAndError = tokenize(s, source);
	var tokens = tokensAndError[0];


	if (tokensAndError[1].length > 0) {
	    var aLoc = new Loc(num(s.length - tokensAndError[1].length),
 			       num(countLines(s.substring(0, s.length - tokensAndError[1].length))),
 			       num(computeColumn(s.substring(0, s.length - tokensAndError[1].length), 0)),
 			       num(tokensAndError[1].length),
 			       source);
	    plt.types.throwMobyError(aLoc, 
				     "make-moby-error-type:unrecognized-lexical-token",
				     [plt.types.Symbol.makeInstance(tokensAndError[1])]);
	}
	
	var quoteSymbol = plt.types.Symbol.makeInstance("quote");
	var quasiquoteSymbol = plt.types.Symbol.makeInstance("quasiquote");
	var unquoteSymbol = plt.types.Symbol.makeInstance("unquote");
	var unquoteSplicingSymbol = plt.types.Symbol.makeInstance("unquote-splicing");
	var empty = plt.types.Empty.EMPTY;


	var isType = function(type) {
	    return (tokens.length > 0 && tokens[0].type == type);
	}
	

	// peek: -> (token | false)
	var peek = function() {
	    if (tokens.length > 0) 
		return tokens[0];
	    else
		return false;
	}


	// eat: -> token 
	var eat = function() {
	    var t = tokens.shift();
	    return t;
	}







	// NOTE: we define things in this funny way because of a bug in 
	// Firefox 3.5.1 that says the error "can't access optimized closure"
	var readExpr;
	var readExprs;
	var readQuoted;

	readQuoted = function(quoteChar, quoteSymbol) {
	    var leadingQuote = eat(quoteChar);
	    var quoted = readExpr();
	    if (quoted === false) {
		plt.types.throwMobyError(leadingQuote.loc,
					 "make-moby-error-type:missing-expression-following-quote",
					 [datumToStx(plt.types.Symbol.makeInstance(leadingQuote.text),
						     leadingQuote.loc)]);
	    }

	    return datumToStx(plt.types.Cons.makeInstance(
		datumToStx(quoteSymbol, leadingQuote.loc),
		plt.types.Cons.makeInstance(quoted, empty)),
			      new Loc(Loc_dash_offset(leadingQuote.loc),
				      Loc_dash_line(leadingQuote.loc),
				      Loc_dash_column(leadingQuote.loc),
				      (plt.types.NumberTower.add(plt.types.NumberTower.subtract(Loc_dash_offset(stxLoc(quoted)),
												Loc_dash_offset(leadingQuote.loc)),
								 Loc_dash_span(stxLoc(quoted)))),
				      source));
	};

	var getParenRShape = function(lparen) {
	    switch(lparen) {
	    case '(': return ')';
	    case '[' : return ']';
	    case '{' : return '}';
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



	// readExpr: (-> void) -> (or stx false)
	// Returns a syntax, or false if the stream gets exhausted before then.
	readExpr = function() {
	    if (peek() === false) {
		return false;
	    }

	    switch(peek().type) {

	    case '(': 
		var lparen = eat('(');
		var lshape = lparen.text;
		var rshape = getParenRShape(lparen.text);
		var result = readExprs();
		if (peek() === false) {
		    plt.types.throwMobyError(lparen.loc,
					     "make-moby-error-type:unclosed-parentheses",
					     [plt.types.Symbol.makeInstance(lshape), 
					      plt.types.Symbol.makeInstance(rshape)]);
		} else if (peek().text != rshape) {
		    plt.types.throwMobyError(peek().loc,
					     "make-moby-error-type:unbalanced-parentheses",
					     [plt.types.Symbol.makeInstance(lshape), 
					      plt.types.Symbol.makeInstance(rshape),
					      plt.types.Symbol.makeInstance(peek().text),
					      lparen.loc]);
		} else {
		    var rparen = eat(')');
		    return datumToStx(
			result,
			new Loc(Loc_dash_offset(lparen.loc),
				Loc_dash_line(lparen.loc),
				Loc_dash_column(lparen.loc),
				plt.types.NumberTower.add(
				    plt.types.NumberTower.subtract(
					Loc_dash_offset(rparen.loc),
					Loc_dash_offset(lparen.loc)), 
				    plt.types.Rational.ONE),
				source));
		}

	    case '#;':
		var hashcomment = eat('#;');
		var skippingExpr = readExpr();
		if (skippingExpr) {
		    var nextExpr = readExpr();
		    if (nextExpr) {
			return nextExpr;
		    } else {
			return false;
		    }
		} else {
		    plt.types.throwMobyError(
			hashcomment.loc,
			"make-moby-error-type:missing-expression-following-quote",
			[datumToStx(plt.types.Symbol.makeInstance(hashcomment.text),
				    hashcomment.loc)]);
		}

		
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
		var exactnessMatch = t.text.match(/^(\#[ie])(.+)$/);
		if (exactnessMatch) {
		    if (exactnessMatch[1] == "#i") {
			return datumToStx(parseBasicNumber(exactnessMatch[2], false),
					  t.loc);
		    } else {
			return datumToStx(parseBasicNumber(exactnessMatch[2], true),
					  t.loc);
		    }
		} else {
		    return datumToStx(parseBasicNumber(t.text, true),
				      t.loc);
		}

	    case 'complex':
		var t = eat('complex');
		var complexMatch = t.text.match(/^((?:\#[ei])?)([+\-]?(?:\d+\/\d+|\d+\.\d+|\d+\.|\.\d+|\d+)?)([+\-](?:\d+\/\d+|\d+\.\d+|\d+\.|\.\d+|\d+))i/);
		var exactness = (complexMatch[1] == "#i" ? false : true);
		var a = (complexMatch[2] != "" ?
			 parseBasicNumber(complexMatch[2], exactness) :
			 plt.types.Rational.ZERO);
		var b = parseBasicNumber(complexMatch[3], exactness);
		// FIXME: Complex needs to be changed so it takes in either
		// exact or inexact basic values.
	        var newAtom = datumToStx(plt.types.Complex.makeInstance(a, b));
  	        return newAtom;

	    case 'string':
		var t = eat('string');
		return datumToStx(plt.types.String.makeInstance(t.text),
				  t.loc);
	    case 'char':
		var t = eat('char');
		if (t.text == 'newline') {
		    return datumToStx(plt.types.Char.makeInstance('\n'), t.loc);
		} else if (t.text == 'backspace') {
		    // FIXME: add more character constants.
		    // See: http://docs.plt-scheme.org/reference/reader.html#%28idx._%28gentag._172._%28lib._scribblings/reference/reference..scrbl%29%29%29
		    return datumToStx(plt.types.Char.makeInstance(String.fromCharCode(8)), t.loc);
		} else {
		    return datumToStx(plt.types.Char.makeInstance(t.text), t.loc);
		}

	    case 'symbol':
		var t = eat('symbol');
		if (t.text === '.') {
		    plt.types.throwMobyError(t.loc,
					     "make-moby-error-type:unsupported-lexical-token",
					     [plt.types.Symbol.makeInstance(t.text)])
		} else if (t.text === 'true' || t.text === '#t') {
		    return datumToStx(plt.types.Logic.TRUE, t.loc);
		} else if (t.text === 'false' || t.text === '#f') {
		    return datumToStx(plt.types.Logic.FALSE, t.loc);
		} else {
		    return datumToStx(plt.types.Symbol.makeInstance(t.text), t.loc);
		}

	    case ')':
		return false;
		
	    default:
		plt.types.throwMobyError(peek().loc,
					 "make-moby-error-type:unrecognized-lexical-token",
					 [plt.types.Symbol.makeInstance(peek().text)]);
	    }
	};



	// readExprs: (listof stx)
	// Reads a list of expressions up to a closing paren.
	readExprs = function() {
	    var result = plt.types.Empty.EMPTY;
	    while (true) {
		if (peek() == false || isType(')')) {
		    break;
		} else {
		    var nextElt = readExpr();
		    if (nextElt) {
			result = plt.types.Cons.makeInstance(nextElt, result);
		    }
		}
	    }
	    return plt.types.Cons.reverse(result);
	};
	

	var result = readExprs();

	if (tokens.length > 0) {
	    switch (peek().type) {
	    case '(':
		plt.types.throwMobyError(peek().loc,
					 "make-moby-error-type:unclosed-parentheses",
					 [plt.types.Symbol.makeInstance(peek().text),
					  plt.types.Symbol.makeInstance(getParenRShape(peek().text))]);
	    case ')':
		plt.types.throwMobyError(peek().loc,
					 "make-moby-error-type:closing-parenthesis-before-opener",
					 [plt.types.Symbol.makeInstance(peek().text)]);

	    case "'":
	    case "`":
	    case ",":
	    case ",@":
		plt.types.throwMobyError(peek().loc,
					 "make-moby-error-type:missing-expression-following-quote",
					 [datumToStx(plt.types.Symbol.makeInstance(peek().text),
						     peek().loc)]);

	    case 'whitespace':
	    case '#;':
	    case 'comment':
	    case "char":
	    case "string":
	    case "symbol-or-number":
	    default:
		// This should never happen, given that whitespace
		// is filtered out, and the other token types are expressions.
		// Still, we'd better make sure!
		plt.types.throwMobyError(peek().loc,
					 "make-moby-error-type:generic-syntactic-error",
					 ["The parse unexpectedly failed as it was at " +
					  peek().text + ".",
					  plt.types.Empty.EMPTY]);
	    }
	}
	return result;
    }
    



    // provides:
    plt.reader.tokenize = tokenize;
    plt.reader.readSchemeExpressions = readSchemeExpressions;
}());
