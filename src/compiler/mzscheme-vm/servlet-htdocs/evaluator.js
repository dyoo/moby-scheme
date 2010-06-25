// Defines an Evaluator class.


// Evaluator(options)
//     options: { write: dom -> void,
//                writeError: dom -> void }
//
// Constructs a new evaluator.
// 
//
//
// Evaluator.prototype.executeProgram: [name string] [program string] [onDone (-> void)] -> void
// Executes the program with the given name.  When the program is done evaluating,
// calls onDone.





var Evaluator = (function() {

    var COMPILATION_SERVLET_URL = "/servlets/standalone.ss";


    var Evaluator = function(options) {
	var that = this;

	if (options.write) {
	    this.write = options.write;
	} else {
	    this.write = function(dom) {
	    };
	}

	if (options.writeError) {
	    this.writeError = options.writeError;
	} else {
	    this.writeError = function(dom) {
	    };
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


    Evaluator.prototype._reportError = function(thing) {
	var errorDom = document.createElement("div");
	errorDom.style.color = "red";
	if (typeof thing === 'string') {
	    errorDom.appendChild(document.createTextNode(thing+''));
	} else {
	    errorDom.appendChild(thing);
	}
	this.writeError(errorDom);
    };




    // compileCode: string string continuation -> void
    Evaluator.prototype.executeProgram = function(programName, code, onDone) {
	var that = this;

	Evaluator.compilation_success_callback__ = function(compiledCode) {
	    that._onCompilationSuccess(compiledCode, onDone);
	};

	Evaluator.compilation_failure_callback__ = function(errorMessage) {
	    that._onCompilationFailure(errorMessage, onDone);
	};
	
	loadScript(COMPILATION_SERVLET_URL + "?" +
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
        

	

    Evaluator.prototype._onCompilationSuccess = function(compiledBytecode, contK) {

	var that = this;
	
	var onSuccess =  function(lastResult) {
	    // Do nothing; side effects will have printed values of toplevel
	    // expressions already.
	    contK();
	};

	var onFail = function(exn) {
	    // Under google-chrome, this will produce a nice error stack
	    // trace that we can deal with.
	    if (typeof(console) !== 'undefined' && console.log &&
		exn && exn.stack) {
		console.log(exn.stack);
	    }
	    
	    if (types.isSchemeError(exn)) {
		var errorValue = exn.val;
		if (types.isExn(errorValue)) {
		    that._reportError(types.exnValue(errorValue) + '');
		} else {
		    that._reportError(exn+'');
		}
	    } else {
		that._reportError(exn+'');
	    }
	    contK();
	};

	this.aState.clearForEval();
	interpret.load(compiledBytecode, this.aState);
	interpret.run(this.aState, onSuccess, onFail);
    };
    


    Evaluator.prototype._onCompilationFailure = function(errorMessage, contK) {
	this._reportError(errorMessage);
	contK();
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
	    while (queue[url].length)
		queue[url].shift()();
	    delete(queue[url]);
	    document.getElementsByTagName("head")[0].removeChild(script);
	};
	script.src = url;
	document.getElementsByTagName("head")[0].appendChild(script);
    }
    //////////////////////////////////////////////////////////////////////





    return Evaluator;
})();




Evaluator.compilation_success_callback__ = function() {}
Evaluator.compilation_failure_callback__ = function() {}
