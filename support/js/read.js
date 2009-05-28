var readSchemeExpressions;


function tokenize(s) {
  var tokens = [];

  var PATTERNS = [['whitespace' , /^(\s+)/],
		  ['comment' , /(^;[^\n]*)/],
		  ['(' , /^(\(|\[)/],
		  [')' , /^(\)|\])/],
		  ['number' , /^([+\-]?(?:\d+\.\d+|\d+\.|\.\d+|\d+))/],
		  ['symbol' ,/^([a-zA-Z\+\=\?\!\@\#\$\%\^\&\*\-\/\.\>\<][\w\+\=\?\!\@\#\$\%\^\&\*\-\/\.\>\<]*)/],
		  ['string' , /^"((?:[^\"]|\\")*)"/],      // comment (emacs getting confused with quote): " 
	          ['\'' , /^(\')/],
		  ['`' , /^(`)/],
		  [',' , /^(,)/]
		 ];

  while (true) {
    var shouldContinue = false;
    for (var i = 0; i < PATTERNS.length; i++) {
      var patternName = PATTERNS[i][0];
      var pattern = PATTERNS[i][1]
      var result = s.match(pattern);
      if (result != null) {
	if (patternName != 'whitespace' && patternName != 'comment') {
	  tokens.push([patternName, result[1]]);
	}
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




(function(){
  readSchemeExpressions = function(s) {
    return org.plt.Kernel.cons("42",
			       org.plt.types.Empty.EMPTY);
    
    
    
    
  }}());
