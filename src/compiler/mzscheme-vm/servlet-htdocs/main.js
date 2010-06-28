var writeToInteractions = function(thing) {
    if (typeof thing === 'string' || typeof thing === 'number') {
	thing = document.createTextNode(thing + '');
    }
    var history = document.getElementById('history');
    history.appendChild(thing);
}


var evaluator = new Evaluator(
    { write: writeToInteractions,
      compilationServletUrl: "/servlets/standalone.ss"
    });


var executeButtonPressed = function() {
    var interactionText = document.getElementById('textarea');
    evaluator.compilationServletUrl = (document.getElementById('compilationServletUrl').value);

    var theCodeElement = document.createElement('pre');
    theCodeElement.appendChild(document.createTextNode(interactionText.value));
    writeToInteractions(theCodeElement);
    blockInput();
    evaluator.executeProgram("interaction",
			     interactionText.value,
			     function() { unblockInput() },
			     function(exn) {
				 reportError(exn);
				 unblockInput() });
};


var reportError = function(exn) {
    var domElt = document.createElement('div');
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
