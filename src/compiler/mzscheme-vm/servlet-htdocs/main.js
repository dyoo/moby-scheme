var evaluator = new Evaluator(
    { write: function(x) { writeToInteractions(x) },
      writeError: function(err) { reportError(err) },
      compilationServletUrl: "/servlets/standalone.ss"
    });



var executeButtonPressed = function() {
    var interactionText = document.getElementById('textarea');
    writeToInteractions(document.createElement("br"));
    writeToInteractions(document.createTextNode(interactionText.value));
    writeToInteractions(document.createElement("br"));
    blockInput();
    evaluator.executeProgram("interactions",
			     interactionText.value,
			     function() {
				 unblockInput() },
			     function(exn) { reportError(exn);
					     unblockInput() });
};


var writeToInteractions = function(thing) {
    if (typeof thing === 'string' || typeof thing === 'number') {
	thing = document.createElement('div');
	thing.style['white-space'] = 'pre';
	div.appendChild(document.createTextNode(thing + ''));
    }
    var history = document.getElementById('history');
    history.appendChild(thing);
}



var reportError = function(exn) {
    var domElt = document.createElement('div');
    domElt.style['color'] = 'red';
    domElt.appendChild(document.createTextNode(evaluator.getMessageFromExn(exn)+""));

    var stacktrace = evaluator.getTraceFromExn(exn);
    for (var i = 0; i < stacktrace.length; i++) {
	domElt.appendChild(document.createElement("br"));
	domElt.appendChild(document.createTextNode(
			     "in " + stacktrace[i].id +
			     ", at offset " + stacktrace[i].offset +
			     ", line " + stacktrace[i].line +
			     ", column " + stacktrace[i].column +
			     ", span " + stacktrace[i].span));
    };

    writeToInteractions(domElt);
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
