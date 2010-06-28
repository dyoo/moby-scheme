// Defines an Evaluator class, with the following constructor:
//
//
// Evaluator(options)
//     options: { write: dom -> void,
//                writeError: dom -> void,
//                compilationServletUrl: string}
//
// Constructs a new evaluator.
// 
//
// and the main method:
//
//
// Evaluator.prototype.executeProgram: [name string] [program string] [onDone (-> void)] -> void
//
// Executes the program with the given name.  When the program is done evaluating,
// calls onDone.
//
// 
// WARNING: this code assumes that there's toplevel access to:
//
//     Evaluator.compilation_success_callback__
//     Evaluator.compilation_failure_callback__
//
// because we use SCRIPT injection to work around the same-origin
// policy.




var Evaluator = (function() {

    var DEFAULT_COMPILATION_SERVLET_URL = "/servlets/standalone.ss";


    var Evaluator = function(options) {
	var that = this;

	if (options.write) {
	    this.write = options.write;
	} else {
	    this.write = function(dom) {
	    };
	}

	if (options.compilationServletUrl) {
	    this.compilationServletUrl = options.compilationServletUrl;
	} else {
	    this.compilationServletUrl = DEFAULT_COMPILATION_SERVLET_URL;
	}


	this.aState = new state.State();

	this.aState.setPrintHook(function(thing) {
	    var dom = types.toDomNode(thing);
	    that.write(dom);	
	});
		
	this.aState.setDisplayHook(function(thing) {
	    var dom = types.toDisplayedString(thing);
	    that.write(dom);	
	});
	
	this.aState.setToplevelNodeHook(function() {
	    var innerDom = document.createElement("div");
	    var dom = document.createElement("div");
	    dom.appendChild(innerDom);
	    that.write(dom);	
	    return innerDom;
	});
    };



    // compileCode: string string (-> void) (exn -> void) -> void
    Evaluator.prototype.executeProgram = function(programName, code,
						  onDone,
						  onDoneError) {
	var that = this;

	Evaluator.compilation_success_callback__ = function(compiledCode) {
	    that._onCompilationSuccess(compiledCode, onDone, onDoneError);
	};

	Evaluator.compilation_failure_callback__ = function(errorMessage) {
	    that._onCompilationFailure(errorMessage, onDoneError);
	};
	
	loadScript(this.compilationServletUrl + "?" +
		   encodeUrlParameters({ 'name': programName,
					 'program': code,
					 'callback': 'Evaluator.compilation_success_callback__',
					 'on-error': 'Evaluator.compilation_failure_callback__'}));
    };
    

    var encodeUrlParameters = function(hash) {
	var chunks = [];
	for (var key in hash) {
	    if (hash.hasOwnProperty(key)) {
		chunks.push(encodeURIComponent(key) +"="+ encodeURIComponent(hash[key]));
	    }
	}
	return chunks.join('&');
    };
        

	


    var STACK_KEY = types.symbol("moby-stack-record-continuation-mark-key");

    Evaluator.prototype.getTraceFromExn = function(exn) {
	var results = [];
	var errorValue = exn.val;
	if (types.isExn(errorValue)) {
	    if (types.exnContMarks(errorValue)) {
		var contMarkSet = types.exnContMarks(errorValue);
		var stackTrace = contMarkSet.ref(STACK_KEY);
		// KLUDGE: the first element in the stack trace
		// is weird because it's due to the print-values
		// introduced by a macro.
		stackTrace.shift();			
		for (var i = stackTrace.length - 1; 
		     i >= 0; i--) {
		    var callRecord = stackTrace[i];
		    var id = callRecord.ref(0);
		    var offset = callRecord.ref(1);
		    var line = callRecord.ref(2);
		    var column = callRecord.ref(3);
		    var span = callRecord.ref(4);
		    results.push({'id': id, 
				  'offset': offset,
				  'line': line, 
				  'column': column,
				  'span': span});

		}
	    }
	}
	return results;
    };


    Evaluator.prototype.getMessageFromExn = function(exn) {
	if (types.isSchemeError(exn)) {
	    var errorValue = exn.val;
	    if (types.isExn(errorValue)) {
		return types.exnMessage(errorValue);
	    } else {
		return errorValue + '';
	    }
	} else {
	    return exn.message;
	}
    };



    Evaluator.prototype._onCompilationSuccess = function(compiledBytecode,
							 onDoneSuccess,
							 onDoneFail) {
	var that = this;
	

// 	var onFail = function(exn) {
// 	    // Under google-chrome, this will produce a nice error stack
// 	    // trace that we can deal with.
// 	    if (typeof(console) !== 'undefined' && console.log &&
// 		exn && exn.stack) {
// 		console.log(exn.stack);
// 	    }
	    
// 	    if (types.isSchemeError(exn)) {
// 		var errorValue = exn.val;
// 		if (types.isExn(errorValue)) {
// 		    that._reportError(types.exnMessage(errorValue) + '');
// 		    if (types.exnContMarks(errorValue)) {
// 			var contMarkSet = types.exnContMarks(errorValue);
// 			var stackTrace = contMarkSet.ref(types.symbol("moby-stack-record-continuation-mark-key"));
// 			// KLUDGE: the first element in the stack trace
// 			// is weird because it's due to the print-values
// 			// introduced by a macro.
// 			stackTrace.shift();
			
// 			for (var i = stackTrace.length - 1; 
// 			     i >= 0; i--) {
// 			    var callRecord = stackTrace[i];
// 			    var id = callRecord.ref(0);
// 			    var offset = callRecord.ref(1);
// 			    var line = callRecord.ref(2);
// 			    var column = callRecord.ref(3);
// 			    var span = callRecord.ref(4);
// 			    that._reportError("    in " + id + 
// 					      ", at: line " + line + 
// 					      ", column " + column + 
// 					      ", span " + span); 
// 			}
// 		    }
// 		} else {
// 		    that._reportError(exn+'');
// 		}
// 	    } else {
// 		that._reportError(exn+'');
// 	    }
// 	    onDoneSuccess();
// 	};

	this.aState.clearForEval();
	interpret.load(compiledBytecode, this.aState);
	interpret.run(this.aState, onDoneSuccess, onDoneFail);
    };
    


    Evaluator.prototype._onCompilationFailure = function(errorMessage, onDoneError) {
	onDoneError(new Error(errorMessage));
    };






    //////////////////////////////////////////////////////////////////////
    /* Lesser General Public License for more details.
	*
	* You should have received a copy of the GNU Lesser General Public
	* License along with this library; if not, write to the Free Software
	* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
	*
	* Contact information:
	*   Dao Gottwald  <dao at design-noir.de>
	*
	* @version  1.6
	* @url      http://design-noir.de/webdev/JS/loadScript/
    */

    var loadScript = function(url, callback) {
	var f = arguments.callee;
	if (!("queue" in f))
	    f.queue = {};
	var queue = f.queue;
	if (url in queue) { // script is already in the document
	    if (callback) {
		if (queue[url]) // still loading
		    queue[url].push(callback);
		else // loaded
		    callback();
	    }
	    return;
	}
	queue[url] = callback ? [callback] : [];
	var script = document.createElement("script");
	script.type = "text/javascript";
	script.onload = script.onreadystatechange = function() {
	    if (script.readyState && script.readyState != "loaded" && script.readyState != "complete")
		return;
	    script.onreadystatechange = script.onload = null;
	    document.getElementsByTagName("head")[0].removeChild(script);
	    var work = queue[url];
	    delete(queue[url]);
	    while (work.length)
		work.shift()();
	};
	script.src = url;
	document.getElementsByTagName("head")[0].appendChild(script);
    }
    //////////////////////////////////////////////////////////////////////





    return Evaluator;
})();




Evaluator.compilation_success_callback__ = function() {}
Evaluator.compilation_failure_callback__ = function() {}
