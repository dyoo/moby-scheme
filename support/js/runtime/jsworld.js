var plt = plt || {};
plt.world = plt.world || {};
plt.world.MobyJsworld = {};

// Depends on world.js, world-config.js

(function() {

    var Jsworld = plt.world.MobyJsworld;

    // The real low-level jsworld module:
    var _js = plt.Jsworld;



    var MobyTypeError = plt.Kernel.MobyTypeError;





    // isHandler: X -> boolean
    // Right now, a handler is a function that consumes and produces
    // configs.  We should tighten up the type check eventually.
    function isHandler(x) {
	return typeof(x) == 'function';
    }




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



    // FIXME: document how we may want to create and destroy toplevel
    // nodes on bigBang.
    Jsworld.makeToplevelNode = function() {
	return getBigBangWindow().document.getElementById("jsworld-div");
    };



    function isList(x) {
	return (x instanceof plt.types.Cons) || (x instanceof plt.types.Empty);
    }




    // The default printWorldHook will write the written content of the node.
    // We probably want to invoke the pretty printer here instead!
    Jsworld.printWorldHook = function(world, node) {
	if(node.lastChild == null) {
	    node.appendChild(document.createTextNode(plt.Kernel.toWrittenString(world)));
	} else {
	    node.replaceChild(document.createTextNode(plt.Kernel.toWrittenString(world)),
			      node.lastChild);
	}
    };



    // bigBang: world (listof (list string string)) (listof handler) -> world
    Jsworld.bigBang = function(initWorld, attribs, handlers) {
	plt.Kernel.checkList(attribs, "js-big-bang: 2nd argument must be a list of global attributes, i.e. empty");
	plt.Kernel.arrayEach(handlers,
			     function(x) {
				 plt.Kernel.check(x, isHandler, 
				       "js-big-bang: expects handler") });
	var toplevelNode = Jsworld.makeToplevelNode();

	var config = new plt.world.config.WorldConfig();
	for(var i = 0; i < handlers.length; i++) {
	  config = handlers[i](config);
	}
	config = config.updateAll({'changeWorld': Jsworld.updateWorld});
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
	} else {
	    wrappedHandlers.push(_js.on_world_change(
		function(w) { Jsworld.printWorldHook(w, toplevelNode); }));
	}


	if (config.lookup('tickDelay')) {
	    function wrappedTick(w) {
		setTimeout(function() {plt.world.stimuli.onTick()}, 0);
		return w;
	    }
	    var wrappedDelay = config.lookup('tickDelay');
	    wrappedHandlers.push(_js.on_tick(wrappedDelay, wrappedTick));
	}


	if (config.lookup('initialEffect')) {
	    plt.world.Kernel.applyEffect(config.lookup('initialEffect'));
	}

	// Fixme: handle stopwhen.

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
	var node = _js.p(attribs);
	node.toWrittenString = function() { 
	    return plt.Kernel.format("(js-p)", []);
	};
	node.toDisplayedString = node.toWrittenString;
	return node;
    };

    // div: assoc -> node
    Jsworld.div = function(args) {
	var attribs = getAttribs(args);
	var node = _js.div(attribs);
	node.toWrittenString = function() { 
	    return plt.Kernel.format("(js-div)", []);
	};
	node.toDisplayedString = node.toWrittenString;
	return node;
    };

    // button: (world -> world) assoc -> node
    Jsworld.button = function(f, args) {
	var noneF = function(world) {
	    return plt.world.Kernel.make_dash_effect_colon_none();
	};
	var node = Jsworld.buttonStar(f, 
				      noneF,
				      args);
	node.toWrittenString = function() { return "(js-button ...)"; }
	node.toDisplayedString = node.toWrittenString;
	return node;
    };

    Jsworld.buttonStar = function(worldUpdateF, effectF, args) {
	var attribs = getAttribs(args);
	function wrappedF(world, evt) {
	    plt.world.Kernel.applyEffect(effectF([world]));
	    return worldUpdateF([world]);
	}
	var node = _js.button(wrappedF, attribs);
	node.toWrittenString = function() { return "(js-button ...)"; }
	node.toDisplayedString = node.toWrittenString;
	return node;
    };
    


    // BidirectionalInput
    Jsworld.bidirectionalInput = function(type, valF, updateF, args) {
	var attribs = getAttribs(args);
	var node = _js.bidirectional_input(type,
				       function(w) { return valF([w]) },
				       function(w, v) { 
					   return updateF([w, v])
				       },
				       attribs);
	node.toWrittenString = function() { return "(js-bidirectional-input ...)"; }
	node.toDisplayedString = node.toWrittenString;
	return node;
    };

    // Images.
    Jsworld.img = function(src, args) {
	var attribs = getAttribs(args);
	var node = _js.img(src, attribs);
	node.toWrittenString = function() { return "(js-img ...)"; }
	node.toDisplayedString = node.toWrittenString;
	return node;
    };


    // text: string assoc -> node
    Jsworld.text = function(s, args) {
	var attribs = getAttribs(args);
	var node = _js.text(s, attribs);
	node.toWrittenString = function() { return "(js-img ...)"; }
	node.toDisplayedString = node.toWrittenString;
	return node;
    };



    // fixme: add support for select, option, textarea, h1, canvas


})();
