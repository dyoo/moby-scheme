var evaluator = new Evaluator(
    { write: function(x) { writeToInteractions(x) },
      writeError: function(err) { reportError(err) },
      compilationServletUrl: "/servlets/standalone.ss",
      scriptCompilationServletUrl: "/servlets/standalone.ss"
    });



var executeButtonPressed = function() {
    evaluator.compilationServletUrl = document.getElementById('compilationServletUrl').value;
    var interactionText = document.getElementById('textarea');
    writeToInteractions(interactionText.value);
    blockInput();
    evaluator.executeProgram("interactions",
			     interactionText.value,
			     function() {
				 unblockInput() },
			     function(exn) { reportError(exn);
					     unblockInput() });
};


var breakButtonPressed = function() {
    evaluator.requestBreak();
};


var writeToInteractions = function(thing) {
    var history = document.getElementById('history');
    if (typeof thing === 'string' || typeof thing === 'number') {
	var dom = document.createElement('div');
	dom.style['white-space'] = 'pre';
	dom.appendChild(document.createTextNode(thing + ''));
	history.appendChild(dom);
    } else {
	history.appendChild(thing);
    }
};


var reportError = function(exn) {
    // Under google-chrome, this will produce a nice error stack
    // trace that we can deal with.
    if (typeof(console) !== 'undefined' && console.log &&
	exn && exn.stack) {
	console.log(exn.stack);
    }


    var domElt = document.createElement('div');
    domElt.style['color'] = 'red';

    if (exn.domMessage) {
	domElt.appendChild(exn.domMessage);
    } else {
	domElt.appendChild(document.createTextNode(evaluator.getMessageFromExn(exn)+""));
    }

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




var mainPageLoad = function() {
    var interactionText = document.getElementById('textarea');
    var executeButton = document.getElementById('executeButton');
    unblockInput();
};
