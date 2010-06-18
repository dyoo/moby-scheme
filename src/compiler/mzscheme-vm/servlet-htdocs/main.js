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
    jQuery.ajax({
	    url: "/servlets/standalone.ss",
	    data: {program : code},
	    dataType: 'text',
	    success: function(compiledBytecode, textStatus, xhr) {
		interpret.load(eval('(' + compiledBytecode + ')'), state);
		interpret.run(state, function(lastResult) {
  			interactionText.disabled = false;
  			executeButton.disabled = false;
			interactionText.value = '';
			interactionText.focus();
		    });
	    }});
};
    


    var oldCode = '{"$" : "compilation-top" ,"max-let-depth" : 0}';


google.load("jquery", "1");
google.setOnLoadCallback(function() {
	var interactionText = document.getElementById('textarea');
	var executeButton = document.getElementById('executeButton');
	interactionText.disabled = false;
	executeButton.disabled = false;
    });