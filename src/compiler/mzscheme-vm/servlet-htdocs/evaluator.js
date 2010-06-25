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
	var interactionText = document.getElementById('textarea');
	var executeButton = document.getElementById('executeButton');
	jQuery.ajax({ url: "/servlets/standalone.ss",
		      data: {name: programName,
		             program : code},
		      dataType: 'text',
		      success: function(code, status, xhr){ 
			  that._onCompilationSuccess(code, status, xhr, onDone);
	              },
		      error: function(xhr, status, errorThrown) { 
			  that._onCompilationFailure(xhr, status, errorThrown, onDone);
		      }
		    });
    };
    
        

    Evaluator.prototype._onCompilationSuccess = 
	function(compiledBytecode, textStatus, xhr, contK) {
	var that = this;
	var interactionText = document.getElementById('textarea');
	var executeButton = document.getElementById('executeButton');
	
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
	interpret.load(eval('(' + compiledBytecode + ')'), this.aState);
	interpret.run(this.aState, onSuccess, onFail);
    };
    


    Evaluator.prototype._onCompilationFailure = function(xhr, textStatus, errorThrown, contK) {
	this._reportError(xhr.statusText);
	contK();
    };



    return Evaluator;
})();