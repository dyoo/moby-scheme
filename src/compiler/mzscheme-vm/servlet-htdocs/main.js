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

    executeProgram(interactionText.value);
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
var executeProgram = function(code, k) {
    var interactionText = document.getElementById('textarea');
    var executeButton = document.getElementById('executeButton');
    interactionText.disabled = true;
    executeButton.disabled = true;
    jQuery.ajax({ url: "/servlets/standalone.ss",
		  data: {program : code},
		  dataType: 'text',
		  success: onCompilationSuccess,
		  error: onCompilationFailure});
};
    

var onCompilationSuccess = function(compiledBytecode, textStatus, xhr) {
    var interactionText = document.getElementById('textarea');
    var executeButton = document.getElementById('executeButton');
    
    var onSuccess =  function(lastResult) {
  	interactionText.disabled = false;
  	executeButton.disabled = false;
	interactionText.value = '';
	interactionText.focus();
    };

    var onFail = function(exn) {
  	interactionText.disabled = false;
  	executeButton.disabled = false;
	interactionText.value = '';
	interactionText.focus();

	// Under google-chrome, this will produce a nice error stack
	// trace that we can deal with.
	if (console && console.log && exn && exn.stack) {
	    console.log(exn.stack);
	}

	var errorDom = document.createElement("div");
	errorDom.style.color="red";
	errorDom.appendChild(document.createTextNode(exn+''));
	addToHistory(errorDom);

	throw exn;
    };

    aState.clearForEval();
    interpret.load(eval('(' + compiledBytecode + ')'), aState);
    interpret.run(aState, onSuccess, onFail);
};
    

var onCompilationFailure = function(xhr, textStatus, errorThrown) {
    console.log("here");
};





    var oldCode = '{"$" : "compilation-top" ,"max-let-depth" : 0}';


google.load("jquery", "1");
google.setOnLoadCallback(function() {
	var interactionText = document.getElementById('textarea');
	var executeButton = document.getElementById('executeButton');
	interactionText.disabled = false;
	executeButton.disabled = false;
    });