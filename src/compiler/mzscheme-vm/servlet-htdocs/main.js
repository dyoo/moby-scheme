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
	    addToHistory("emitted bytecode is " + compiledBytecode);
	}});
    console.log();
}






google.load("jquery", "1");
google.setOnLoadCallback(function() {
    // allow interaction with the interface after the page has loaded.
});