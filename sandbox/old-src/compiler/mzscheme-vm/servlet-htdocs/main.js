var writeToInteractions = function(thing) {
    if (typeof thing === 'string' || typeof thing === 'number') {
	thing = document.createTextNode(thing + '');
    }
    var history = document.getElementById('history');
    history.appendChild(thing);
    history.appendChild(document.createElement('br'));
}


var evaluator = new Evaluator(
    { write: writeToInteractions,
      writeError: writeToInteractions
    });


var executeButtonPressed = function() {
    var interactionText = document.getElementById('textarea');
    writeToInteractions(document.createTextNode(interactionText.value));
    blockInput();
    evaluator.executeProgram("interaction",
			     interactionText.value,
			     function() { unblockInput() });
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
