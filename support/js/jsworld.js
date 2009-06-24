var org = org || {};
org.plt = org.plt || {};
org.plt.world = org.plt.world || {};
org.plt.world.MobyJsworld = {};

(function() {

    var Jsworld = org.plt.world.MobyJsworld;

    // The real low-level jsworld module:
    var _js = plt.Jsworld;


    //////////////////////////////////////////////////////////////////////
    //From this point forward, we define wrappers to integrate jsworld
    //with Moby.


    // deepListToArray: any -> any
    // Converts list structure to array structure.
    function deepListToArray(x) {
	var thing = x;
	if (org.plt.Kernel.empty_question_(thing)) {
	    return [];
	} else if (org.plt.Kernel.pair_question_(thing)) {
	    var result = [];
	    while (!thing.isEmpty()) {
		result.push(deepListToArray(thing.first()));
		thing = thing.rest();
	    }
	    return result;
	} else {
	    return x;
	}
    }

    // assocListToAssocArray: (listof (list X Y)) -> (hashof X Y)
    function assocListToAssocArray(aList) {
	var result = {};
	while (! aList.isEmpty()) {
	    var key = aList.first().first();
	    var val = aList.first().rest().first();
	    result[key] = val;
	    aList = aList.rest();
	}
	return result;
    }


    // getBigBangWindow: -> window
    function getBigBangWindow() {
        if (window.document.getElementById("jsworld-div") != undefined) {
	    return window;
	}

        var newWindow = window.open(
	    "big-bang.html",
	    "big-bang");
	    //"toolbar=false,location=false,directories=false,status=false,menubar=false,width="+width+",height="+height);
	if (newWindow == null) { 
            throw new Error("Error: Not allowed to create a new window."); }

	return newWindow;
    }


    // types are
    // sexp: (cons node (listof sexp))
    // css-style: (node (listof (list string string)))

    // Exports:


    // bigBang: world (listof handler) (listof (list string string)) -> world
    Jsworld.bigBang = function(initWorld, handlers, attribs) {

	var mainWindow = getBigBangWindow();
	// KLUDGE: check with Chris to get newest version of jsworld that
	// consumes the toplevel node.
	toplevelNode = mainWindow.document.getElementById("jsworld-div");

	function wrappedRedraw(w) {
	    var result = [toplevelNode, deepListToArray(redraw([w]))];
	    return result;
	}

	function wrappedRedrawCss(w) {
	    var result = deepListToArray(redrawCss([w]));
	    return result;
	}

	function wrappedTick(w) {
	    var result = tick([w]);
	    return result;
	}

	var wrappedDelay = delay.toInteger();


	return big_bang(initWorld,
			wrappedRedraw,
			wrappedRedrawCss,
			wrappedDelay, 
			wrappedTick, 
			assocListToAssocArray(attribs));
    }




    // p: assoc -> node
    Jsworld.p = function(attribsAssocList) {
	return p(assocListToAssocArray(attribsAssocList));
    };

    // div: assoc -> node
    Jsworld.div = function (attribsAssocList) {
	return div(assocListToAssocArray(attribsAssocList));
    };

    // button: (world -> world) assoc -> node
    Jsworld.button = function(f, attribsAssocList) {
	return button(f, assocListToAssocArray(attribsAssocList));
    };

    // input: string assoc -> node
    Jsworld.input = function(type, attribsAssocList) {
	return input(type, assocListToAssocArray(attribsAssocList));
    };

    // text: string assoc -> node
    Jsworld.text = function(s, attribsAssocList) {
	return text(s, assocListToAssocArray(attribsAssocList));
    }



})();