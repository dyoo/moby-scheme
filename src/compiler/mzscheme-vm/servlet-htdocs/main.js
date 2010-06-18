var state = new state.State();

state.setPrintHook(function(thing) {
	var dom = types.toDomNode(thing);
	addToHistory(dom);	
    });


state.setDisplayHook(function(thing) {
	var dom = types.toDisplayedString(thing);
	addToHistory(dom);	
    });




var executeButtonPressed = function() {
    var interactionText = document.getElementById('textarea');
    addToHistory(document.createTextNode(interactionText.value));

    compileCode(interactionText.value);

    interactionText.value = '';
    interactionText.focus();
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
var compileCode = function(code, k) {
    jQuery.ajax({
	    url: "/servlets/standalone.ss",
	    data: {program : code},
	    dataType: 'text',
	    success: function(compiledBytecode, textStatus, xhr) {
		//addToHistory("emitted bytecode is " + compiledBytecode);
		interpret.load(eval('(' + compiledBytecode + ')'), state);
		interpret.run(state, function(lastResult) {
		    });	    
	    }});
};
    


    var oldCode = '{"$" : "compilation-top" ,"max-let-depth" : 0}';


google.load("jquery", "1");
google.setOnLoadCallback(function() {
	// allow interaction with the interface after the page has loaded.
    });