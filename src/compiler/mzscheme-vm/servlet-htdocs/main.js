var executeButtonPressed = function() {
    var interactionText = document.getElementById('textarea');
    var history = document.getElementById('history');
    history.appendChild(document.createTextNode(interactionText.value));
    history.appendChild(document.createElement('br'));

    interactionText.value = '';
    interactionText.focus();
};

