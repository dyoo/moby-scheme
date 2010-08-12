// Defines an Evaluator class, with the following constructor:
//
//
// Evaluator(options)
//     options: { write: dom -> void,
//                compilationServletUrl: string,
//                scriptCompilationServletUrl: string}
//
// Constructs a new evaluator.
// 
//
// and the main method:
//
//
// Evaluator.prototype.executeProgram: [name string] [program string] [onDone (-> void)] -> void
//
//
// Executes the program with the given name.  When the program is done evaluating,
// calls onDone.
//
//
// Evaluator.prototype.requestBreak: -> void
//
// Triggers scheme evaluation to raise an exn:break.
//
//
// Evaluator.prototype.executeCompiledProgram: bytecode (-> void) (exn -> void) -> void 
//
// Execute a compiled program, given the bytecode already.
//
//
//
// Evaluator.prototype.getMessageFromExn
//
// Evaluator.prototype.getStackTraceFromExn
//
// 
// WARNING: this code assumes that there's toplevel access to:
//
//     Evaluator.compilation_success_callback__
//     Evaluator.compilation_failure_callback__
//
// because we will use SCRIPT injection to work around the same-origin
// policy for direct access to the compilation server.




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

	if (options.scriptCompilationServletUrl) {
	    this.scriptCompilationServletUrl = options.scriptCompilationServletUrl;
	} else {
	    this.scriptCompilationServletUrl = DEFAULT_COMPILATION_SERVLET_URL;
	}

	if (options.transformDom) {
	    this.transformDom = options.transformDom;
	} else {
	    this.transformDom = function(dom) {
		if (helpers.isLocationDom(dom)) {
		    dom = rewriteLocationDom(dom);
		}
		return dom;
	    }
	}



	this.aState = new state.State();

	this.aState.setPrintHook(function(thing) {
	    var dom = types.toDomNode(thing);
	    dom = that.transformDom(dom);
	    that.write(dom);
	    helpers.maybeCallAfterAttach(dom);
	});
		
	this.aState.setDisplayHook(function(aStr) {
	    var dom = document.createElement("span");
            dom.style["white-space"] = "pre";	
	    var node = document.createTextNode(aStr);
	    dom.appendChild(node);
	    dom = that.transformDom(dom);
	    that.write(dom);	
	    helpers.maybeCallAfterAttach(dom);
	});
	
	this.aState.setToplevelNodeHook(function() {
	    // KLUDGE: special hook to support jsworld.
	    return that.makeToplevelNode();
	});
    };


    // Toplevel nodes are constructed for world programs.
    Evaluator.prototype.makeToplevelNode = function() {
	var innerDom = document.createElement("div");
	var dom = document.createElement("div");
	dom.appendChild(innerDom);
	this.write(dom);	
	helpers.maybeCallAfterAttach(dom);
	return innerDom;
    };


    Evaluator.prototype.requestBreak = function() {
	this.aState.requestBreak();
    };



    var rewriteLocationDom = function(dom) {
	var newDom = document.createElement("span");
	var children = dom.children;
	var line, column, id;
	for (var i = 0; i < children.length; i++) {
	    if (children[i]['class'] === 'location-id') {
		id = children[i].textContent;
	    }
	    if (children[i]['class'] === 'location-offset') {
		// ignore for now
	    }
	    if (children[i]['class'] === 'location-line') {
		line = children[i].textContent;
	    }
	    if (children[i]['class'] === 'location-column') {
		column = children[i].textContent;
	    }
	    if (children[i]['class'] === 'location-span') {
		// ignore for now
	    }
	}
	newDom.appendChild(document.createTextNode('at line: ' + line + ', column: ' + column + ', in ' + id));
	return newDom;
    };




    var encodeScriptParameters = function(programName, code) {
	return encodeUrlParameters({ 'name': programName,
				     'program': code,
				     'callback': 'Evaluator.compilation_success_callback__',
				     'on-error': 'Evaluator.compilation_failure_callback__'});
    };


    // The limit till script breaks is about 2000 characters, according to:
    // http://stackoverflow.com/questions/2659952/maximum-length-of-http-get-request
    var MAX_CHARACTERS_TILL_SCRIPT_BREAKS = 2000;


    // Returns true if the length of the SCRIPT src is short enough that
    // we can safely use SCRIPT to talk directly to the compilation server.
    Evaluator.prototype._isScriptRequestPossible = function(programName, code) {
	var encodedParams = encodeScriptParameters(programName, code);
	return (encodedParams.length +
		this.scriptCompilationServletUrl.length + 1 
		< MAX_CHARACTERS_TILL_SCRIPT_BREAKS);
    };


    // executeProgram: string string (-> void) (exn -> void) -> void
    Evaluator.prototype.executeProgram = function(programName, code,
						  onDone,
						  onDoneError) {
	if (! this._isScriptRequestPossible(programName, code)) {
	    this._executeProgramWithAjax(programName, code, onDone, onDoneError);
	} else {
	    this._executeProgramWithScript(programName, code, onDone, onDoneError);
	}
    };

    // _executeProgramWithScript: string string (-> void) (exn -> void) -> void
    Evaluator.prototype._executeProgramWithScript = function(programName, code,
							     onDone, onDoneError) {
	var that = this;
	var params = encodeScriptParameters(programName, code);
	Evaluator.compilation_success_callback__ = function(compiledCode) {
	    that._onCompilationSuccess(compiledCode, onDone, onDoneError);
	};

	Evaluator.compilation_failure_callback__ = function(errorMessage) {
	    that._onCompilationFailure(errorMessage, onDoneError);
	};
	loadScript(this.scriptCompilationServletUrl + "?" + params);
    };


    // _executeProgramWithAjax: string string (-> void) (exn -> void) -> void
    Evaluator.prototype._executeProgramWithAjax = function(programName, code,
							   onDone, onDoneError) {
	var that = this;
	var params = encodeUrlParameters({'name': programName,
					  'program': code });
	var xhr = new XMLHttpRequest();
	xhr.onreadystatechange = function() {
	    if (xhr.readyState == 4) {
		if (xhr.status === 200) {
		    that._onCompilationSuccess(eval('(' + xhr.responseText + ')'), 
					       onDone, onDoneError);
		} else {
		    that._onCompilationFailure(xhr.responseText, 
					       onDoneError);
		}
	    }
	};
	xhr.open("POST", this.compilationServletUrl, true);
	xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
	xhr.send(params);
    };

    

    Evaluator.prototype.executeCompiledProgram = function(compiledBytecode,
							  onDoneSuccess, onDoneFail) {
	this.aState.clearForEval();
	try {
	    interpret.load(compiledBytecode, this.aState);
	} catch(e) {
	    onDoneFail(e);
	    return;
	}
	this.aState.onSuccess = onDoneSuccess;
	this.aState.onFail = onDoneFail;
	interpret.run(this.aState);
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
        

	



    Evaluator.prototype.getTraceFromExn = function(exn) {
	if (types.isSchemeError(exn)) {
	    var errorValue = exn.val;
	    if (types.isExn(errorValue)) {
		if (types.exnContMarks(errorValue)) {
		    return getTraceFromContinuationMarks(
			types.exnContMarks(errorValue));
		}
	    } else {
		return [];
	    }
	} else if (types.isInternalError(exn)) {
	    return getTraceFromContinuationMarks(exn.contMarks);
	}	
	return [];
    };


    var getTraceFromContinuationMarks = function(contMarkSet) {
	return state.getStackTraceFromContinuationMarks(contMarkSet);
    };



    var isEqualHash = function(hash1, hash2) {
	for (var key in hash1) {
	    if (hash1.hasOwnProperty(key)) {
		if (hash2.hasOwnProperty(key)) {
		    if (hash1[key] !== hash2[key]) {
			return false;
		    }
		} else {
		    return false;
		}
	    }
	}
	for (var key in hash2) {
	    if (hash2.hasOwnProperty(key)) {
		if (hash1.hasOwnProperty(key)) {
		    if (hash1[key] !== hash2[key]) {
			return false;
		    }
		} else {
		    return false;
		}
	    }
	}
	return true;
    };


    Evaluator.prototype.getMessageFromExn = function(exn) {
	if (types.isSchemeError(exn)) {
	    var errorValue = exn.val;
	    if (types.isExn(errorValue)) {
		return types.exnMessage(errorValue);
	    } else {
		return errorValue + '';
	    }
	} else if (types.isInternalError(exn)) {
	    return exn.val + '';
	} else {
	    return exn.message;
	}
    };



    Evaluator.prototype._onCompilationSuccess = function(compiledBytecode,
							 onDoneSuccess,
							 onDoneFail) {
	this.executeCompiledProgram(compiledBytecode, onDoneSuccess, onDoneFail);

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
