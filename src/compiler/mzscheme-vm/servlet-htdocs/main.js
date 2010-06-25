var aState = new state.State();

aState.setPrintHook(function(thing) {
	var dom = types.toDomNode(thing);
	addToHistory(dom);	
    });


aState.setDisplayHook(function(thing) {
	var dom = types.toDisplayedString(thing);
	addToHistory(dom);	
    });

aState.setToplevelNodeHook(function() {
	var innerDom = document.createElement("div");
	var dom = document.createElement("div");
	dom.appendChild(innerDom);
	addToHistory(dom);	
	return innerDom;
    });





var executeButtonPressed = function() {
    var interactionText = document.getElementById('textarea');
    addToHistory(document.createTextNode(interactionText.value));
    blockInput();
    executeProgram("interaction",
		   interactionText.value,
		   function() { unblockInput() });
};



var addToHistory = function(thing) {
    if (typeof thing === 'string' || typeof thing === 'number') {
	thing = document.createTextNode(thing + '');
    }
    var history = document.getElementById('history');
    history.appendChild(thing);
    history.appendChild(document.createElement('br'));
};


// compileCode: string continuation -> void
var executeProgram = function(programName, code, onDone) {
    var interactionText = document.getElementById('textarea');
    var executeButton = document.getElementById('executeButton');
    jQuery.ajax({ url: "/servlets/standalone.ss",
		  data: {name: programName,
		         program : code},
		  dataType: 'text',
		  success: function(code, status, xhr){ 
		      onCompilationSuccess(code, status, xhr, onDone);
	          },
		  error: function(xhr, status, errorThrown) { 
		      onCompilationFailure(xhr, status, errorThrown, onDone);
		  }
		});
};
    

var reportError = function(thing) {
    var errorDom = document.createElement("div");
    errorDom.style.color="red";
    if (typeof thing === 'string') {
	errorDom.appendChild(document.createTextNode(thing+''));
    } else {
	errorDom.appendChild(thing);
    }
    addToHistory(errorDom);
}


var onCompilationSuccess = function(compiledBytecode, textStatus, xhr, contK) {
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
		reportError(types.exnValue(errorValue) + '');
	    } else {
		reportError(exn+'');
	    }
	} else {
	    reportError(exn+'');
	}
	contK();
    };

    aState.clearForEval();
    interpret.load(eval('(' + compiledBytecode + ')'), aState);
    interpret.run(aState, onSuccess, onFail);
};
    


var onCompilationFailure = function(xhr, textStatus, errorThrown, contK) {
    reportError(xhr.statusText);
    contK();
};



var unblockInput = function() {
    var interactionText = document.getElementById('textarea');
    var executeButton = document.getElementById('executeButton');
    interactionText.disabled = false;
    executeButton.disabled = false;
    interactionText.value = '';
    interactionText.focus();
};


var blockInput = function() {
    var interactionText = document.getElementById('textarea');
    var executeButton = document.getElementById('executeButton');
    interactionText.disabled = true;
    executeButton.disabled = true;
};




google.load("jquery", "1");
google.setOnLoadCallback(function() {
    var interactionText = document.getElementById('textarea');
    var executeButton = document.getElementById('executeButton');
    unblockInput();
});