// Creates mocks for the acceleration, tilt, and shake.


// FIXME: all of these mocks look the same.  We need
// a nice library to produce these mocks faster.



// on setup, create a form attached to the document body
EXPORTS['mock-tilt-setup'] = 
    new types.PrimProc(
	"mock-tilt-setup", 1,
	false, false,
	function(onNewTilt) {
	    var mockTiltSetter = document.createElement("div");


	    var xInput = document.createElement("input");
	    xInput.type = "text";

	    var yInput = document.createElement("input");
	    yInput.type = "text";

	    var zInput = document.createElement("input");
	    zInput.type = "text";

	    var submitButton = document.createElement("input");
	    submitButton.type = "button";
	    submitButton.value = "send tilt";
	    submitButton.onclick = function() {
		setTimeout(
		    function() {
			onNewTilt.val(parseFloat(xInput.value),
				      parseFloat(yInput.value),
				      parseFloat(zInput.value));
		    },
		    0);
		return false;
	    };

	    mockTiltSetter.style.border = "1pt solid black";
	    mockTiltSetter.appendChild(
		document.createTextNode("mock tilt setter"));
	    mockTiltSetter.appendChild(xInput);
	    mockTiltSetter.appendChild(yInput);
	    mockTiltSetter.appendChild(zInput);
	    mockTiltSetter.appendChild(submitButton);

	    document.body.appendChild(mockTiltSetter);
	    
	    var shutdownThunk = new types.PrimProc("clearMock",
				      0,
				      false,
				      false,
				      function() {
					  document.body.removeChild(
					      mockTiltSetter);
				      });

	    return shutdownThunk;
	});




EXPORTS['mock-acceleration-setup'] = 
    new types.PrimProc(
	"mock-acceleration-setup", 1,
	false, false,
	function(onNewTilt) {
	    var mockTiltSetter = document.createElement("div");
	    
	    var xInput = document.createElement("input");
	    xInput.type = "text";

	    var yInput = document.createElement("input");
	    yInput.type = "text";

	    var zInput = document.createElement("input");
	    zInput.type = "text";
	    
	    var submitButton = document.createElement("input");
	    submitButton.type = "button";
	    submitButton.value = "send acceleration";
	    submitButton.onclick = function() {
		setTimeout(
		    function() {
			onNewTilt.val(parseFloat(xInput.value),
				      parseFloat(yInput.value),
				      parseFloat(zInput.value));
		    },
		    0);
		return false;
	    };

	    mockTiltSetter.style.border = "1pt solid black";
	    mockTiltSetter.appendChild(
		document.createTextNode("mock acceleration setter"));
	    mockTiltSetter.appendChild(xInput);
	    mockTiltSetter.appendChild(yInput);
	    mockTiltSetter.appendChild(zInput);
	    mockTiltSetter.appendChild(submitButton);

	    document.body.appendChild(mockTiltSetter);
	    
	    var shutdownThunk = new types.PrimProc("clearMock",
				      0,
				      false,
				      false,
				      function() {
					  document.body.removeChild(
					      mockTiltSetter);
				      });
	    return shutdownThunk;
	});





// on setup, create a form attached to the document body
EXPORTS['mock-shake-setup'] = 
    new types.PrimProc(
	"mock-shake-setup", 1,
	false, false,
	function(onNewTilt) {
	    var mockTiltSetter = document.createElement("div");
	    
	    var submitButton = document.createElement("input");
	    submitButton.type = "button";
	    submitButton.value = "send shake";
	    submitButton.onclick = function() {
		setTimeout(
		    function() {
			onNewTilt.val();
		    },
		    0);
		return false;
	    };

	    mockTiltSetter.style.border = "1pt solid black";
	    mockTiltSetter.appendChild(
		document.createTextNode("mock shaker"));
	    mockTiltSetter.appendChild(submitButton);

	    document.body.appendChild(mockTiltSetter);
	    
	    var shutdownThunk = new types.PrimProc("clearMock",
				      0,
				      false,
				      false,
				      function() {
					  document.body.removeChild(
					      mockTiltSetter);
				      });
	    return shutdownThunk;
	});
