var state = new state.State();

state.setPrintHook(function(thing) {
	var dom = types.toDomNode(thing);
	addToHistory(dom);	
    });


state.setDisplayHook(function(thing) {
	var dom = types.toDisplayedString(thing);
	addToHistory(dom);	
    });

state.setToplevelNodeHook(function() {
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
		  success: onCompilation });
};
    

var onCompilation = function(compiledBytecode, textStatus, xhr) {
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

	if (console && console.log && exn.stack) {
	    console.log(exn.stack);
	}

	throw exn;
    };

    interpret.load(eval('(' + compiledBytecode + ')'), state);
    interpret.run(state, onSuccess, onFail);
};
    



    var oldCode = '{"$" : "compilation-top" ,"max-let-depth" : 0}';


google.load("jquery", "1");
google.setOnLoadCallback(function() {
	var interactionText = document.getElementById('textarea');
	var executeButton = document.getElementById('executeButton');
	interactionText.disabled = false;
	executeButton.disabled = false;
    });