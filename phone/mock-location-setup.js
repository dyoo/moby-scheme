
// on setup, create a form attached to the document body
EXPORTS['mock-location-setup'] = 
    new types.PrimProc(
	"mock-location-setup", 1,
	false, false,
	function(onNewLocation) {
	    var mockLocationSetter = document.createElement("div");
	    
	    var latInput = document.createElement("input");
	    latInput.type = "text";
	    
	    var latOutput = document.createElement("input");
	    latOutput.type = "text";

	    var submitButton = document.createElement("input");
	    submitButton.type = "button";
	    submitButton.value = "send lat/lng";
	    submitButton.onclick = function() {
		setTimeout(
		    function() {
			onNewLocation.val(parseFloat(latInput.value),
					  parseFloat(latOutput.value));
		    },
		    0);
		return false;
	    };
	    
	    mockLocationSetter.style.border = "1pt solid black";
	    mockLocationSetter.appendChild(
		document.createTextNode("mock location setter"));
	    mockLocationSetter.appendChild(latInput);
	    mockLocationSetter.appendChild(latOutput);
	    mockLocationSetter.appendChild(submitButton);

	    document.body.appendChild(mockLocationSetter);
	    
	    var shutdownThunk = new types.PrimProc("clearMock",
				      0,
				      false,
				      false,
				      function() {
					  document.body.removeChild(
					      mockLocationSetter);
				      });
	    return shutdownThunk;
	});