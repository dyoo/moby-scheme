var plt = plt || {};
plt.world = plt.world || {};
plt.world.MobyJsworld = {};

(function() {

    var Jsworld = plt.world.MobyJsworld;

    // The real low-level jsworld module:
    var _js = plt.Jsworld;


    //////////////////////////////////////////////////////////////////////
    //From this point forward, we define wrappers to integrate jsworld
    //with Moby.


    // deepListToArray: any -> any
    // Converts list structure to array structure.
    function deepListToArray(x) {
	var thing = x;
	if (plt.Kernel.empty_question_(thing)) {
	    return [];
	} else if (plt.Kernel.pair_question_(thing)) {
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




    // bigBang: world (listof (list string string)) (listof handler) -> world
    Jsworld.bigBang = function(initWorld, attribs, handlers) {

	var mainWindow = getBigBangWindow();
	var toplevelNode = mainWindow.document.getElementById("jsworld-div");

	var config = new plt.world.config.WorldConfig();
	for(var i = 0; i < handlers.length; i++) {
	  config = handlers[i](config);
	}
	config = config.update('changeWorld', Jsworld.updateWorld);
	plt.world.config.CONFIG = config;
	
	var wrappedHandlers = [];
	

	if (config.lookup('onDraw')) {
	  function wrappedRedraw(w) {
	    var result = [toplevelNode, 
			  deepListToArray(config.lookup('onDraw')([w]))];
	    return result;
	  }

	  function wrappedRedrawCss(w) {
	    var result = deepListToArray(config.lookup('onDrawCss')([w]));
	    return result;
	  }
	  wrappedHandlers.push(_js.on_draw(wrappedRedraw, wrappedRedrawCss));
	}


	if (config.lookup('onTick')) {
	    function wrappedTick(w) {
		if (config.lookup('onTickEffect')) {
		    var effect = config.lookup('onTickEffect')([w]);
		    plt.world.Kernel.applyEffect(effect);
                }

		var result = config.lookup('onTick')([w]);
		return result;
	    }
	  
	  var wrappedDelay = config.lookup('tickDelay');
	  wrappedHandlers.push(_js.on_tick(wrappedDelay, wrappedTick));
	}


	if (config.lookup('initialEffect')) {
	    plt.world.Kernel.applyEffect(config.lookup('initialEffect'));
	}

	return _js.big_bang(toplevelNode,
			    initWorld,
			    wrappedHandlers,
			    assocListToAssocArray(attribs));
    }



    function arrayToList(anArray) {
	var result = plt.types.Empty.EMPTY;
	for(var i = 0; i < anArray.length; i++) {
	    result = plt.types.Cons.makeInstance(anArray[length-i-1],
						     result);
	}
	return result;
    }

    // updateWorld: (world -> world) -> void
    Jsworld.updateWorld = _js.change_world;


    function getAttribs(args) {
	if (args.length == 0) {
	    return []
	}
	if (args.length == 1) {
	    return assocListToAssocArray(args[0]);
	} else {
	    throw new Error();
	}
    }

    // p: assoc -> node
    Jsworld.p = function(args) {
	var attribs = getAttribs(args);
	return _js.p(attribs);
    };

    // div: assoc -> node
    Jsworld.div = function(args) {
	var attribs = getAttribs(args);
	return _js.div(attribs);
    };

    // button: (world -> world) assoc -> node
    Jsworld.button = function(f, args) {
	var noneF = function(world) {
	    return plt.world.Kernel.make_dash_effect_colon_none();
	};
	return Jsworld.buttonStar(f, 
				  noneF,
				  args);
    };

    Jsworld.buttonStar = function(worldUpdateF, effectF, args) {
	var attribs = getAttribs(args);
	function wrappedF(world, evt) {
	    plt.world.Kernel.applyEffect(effectF([world]));
	    return worldUpdateF([world]);
	}
	// fixme: we need to wrap the function
	return _js.button(wrappedF, attribs);
    };
    
    // input: string assoc -> node
    Jsworld.input = function(type, args) {
	var attribs = getAttribs(args);
	return _js.input(type, attribs);
    };

    // BidirectionalInput
    Jsworld.bidirectionalInput = function(type, args) {
	throw new Error("FIXME: not implemented yet.");
    };


    // text: string assoc -> node
    Jsworld.text = function(s, args) {
	var attribs = getAttribs(args);
	return _js.text(s, attribs);
    };



    // fixme: add support for select, option, textarea, h1, canvas


})();
