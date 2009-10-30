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
	var newNode;
	if(node.lastChild == null) {
	    newNode = plt.Kernel.toDomNode(world);
	    node.appendChild(newNode);
	} else {
	    newNode = plt.Kernel.toDomNode(world);
	    node.replaceChild(newNode, node.lastChild);
	}
	if (newNode.afterAttach) {
	    newNode.afterAttach();
	}
    };



    // bigBang: world (listof (list string string)) (listof handler) -> world
    Jsworld.bigBang = function(initWorld, handlers) {
	var attribs = plt.types.Empty.EMPTY;
	plt.Kernel.arrayEach(handlers,
			     function(x, i) {
				 plt.Kernel.check(x, function(x) { 
				     return isHandler(x) || isList(x) },
						  "js-big-bang", 
						  "handler or attribute list",
						  i+2) });
	var toplevelNode = Jsworld.makeToplevelNode();

	var config = new plt.world.config.WorldConfig();
	for(var i = 0; i < handlers.length; i++) {
	    if (isList(handlers[i])) {
		attribs = handlers[i];
	    } else if (isHandler(handlers[i])) {
		config = handlers[i](config);
	    }
	}
	config = config.updateAll({'changeWorld': Jsworld.updateWorld,
				   'shutdownWorld': Jsworld.shutdownWorld});
	plt.world.config.CONFIG = config;
	
	var wrappedHandlers = [];
	

	if (config.lookup('onDraw')) {
	    var wrappedRedraw = function(w) {
		var newDomTree = config.lookup('onDraw')([w]);
		var result = [toplevelNode, 
			      deepListToArray(newDomTree)];
		return result;
	    }

	    var wrappedRedrawCss = function(w) {
		var result = deepListToArray(config.lookup('onDrawCss')([w]));
		return result;
	    }
	    wrappedHandlers.push(_js.on_draw(wrappedRedraw, wrappedRedrawCss));
	} else if (config.lookup('onRedraw')) {
	    var reusableCanvas = undefined;
	    var reusableCanvasNode = undefined;
	    var wrappedRedraw = function(w) {
		var aScene = config.lookup('onRedraw')([w]);
		// Performance hack: if we're using onRedraw, we know
		// we've got a scene, so we optimize away the repeated
		// construction of a canvas object.
		if (aScene != null && aScene != undefined && 
		    aScene instanceof plt.Kernel.BaseImage) {
		    var width = 
			plt.world.Kernel.imageWidth(aScene).toInteger();
		    var height = 
			plt.world.Kernel.imageHeight(aScene).toInteger();

		    if (! reusableCanvas) {
			reusableCanvas = plt.Kernel._makeCanvas(width, height);
			reusableCanvasNode = _js.node_to_tree(reusableCanvas);
		    }
 		    reusableCanvas.width = width;
 		    reusableCanvas.height = height;
 		    reusableCanvas.style.setProperty("width",
						     reusableCanvas.width + "px", "");
 		    reusableCanvas.style.setProperty("height", reusableCanvas.height + "px", "");
 		    var ctx = reusableCanvas.getContext("2d");

		    reusableCanvas.style.setProperty("display", "none", "");
		    document.body.appendChild(reusableCanvas);
		    aScene.render(ctx, 0, 0);
		    document.body.removeChild(reusableCanvas);
		    reusableCanvas.style.removeProperty("display");
		    return [toplevelNode, reusableCanvasNode];
		} else {
		    return [toplevelNode, 
			    _js.node_to_tree(
				plt.Kernel.toDomNode(
				    aScene))];
		}
	    }
	    
	    var wrappedRedrawCss = function(w) {
		return [];
	    }
	    wrappedHandlers.push(_js.on_draw(wrappedRedraw, wrappedRedrawCss));
	} else {
	    wrappedHandlers.push(_js.on_world_change
				 (function(w) { 
				     Jsworld.printWorldHook(w, toplevelNode); 
				 }));
	}


	if (config.lookup('tickDelay')) {
	    var wrappedTick = function(w) {
		setTimeout(function() {plt.world.stimuli.onTick()}, 0);
		return w;
	    }
	    var wrappedDelay = config.lookup('tickDelay');
	    wrappedHandlers.push(_js.on_tick(wrappedDelay, wrappedTick));
	}

	if (config.lookup('stopWhen')) {
	    wrappedHandlers.push(_js.stop_when(function(w) { 
			return config.lookup('stopWhen')([w]);
		    }));
	}
	

	if (config.lookup('onKey')) {
	    window.onkeydown = function(e) {
		plt.world.stimuli.onKey(e);
	    }
	}



	if (config.lookup('initialEffect')) {
	    var updaters =
		plt.world.Kernel.applyEffect(config.lookup('initialEffect'));
	    for (var i = 0 ; i < updaters.length; i++) {
		if (config.lookup('stopWhen') && 
		    config.lookup('stopWhen')([initWorld])) {
		    break;
		} else {
		    initWorld = updaters[i](initWorld);
		}
	    }
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
    Jsworld.updateWorld = function(updater) {
	function wrappedUpdater(world) {
	    try {
		return updater(world);
	    } catch (e) {
		plt.Kernel.reportError(e);
		return world;
	    }
	}

	_js.change_world(wrappedUpdater);
    }
    


    // shutdownWorld: -> void
    // Shut down all world computations.
    Jsworld.shutdownWorld = function() {
	_js.shutdown();
    };


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
	node.toDomNode = function() { return node; }
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
	node.toDomNode = function() { return node; }
	return node;
    };

    // button: (world -> world) assoc -> node
    Jsworld.button = function(f, args) {
	var noneF = function(world) {
	    return make_dash_effect_colon_none();
	};
	var node = Jsworld.buttonStar(f, 
				      noneF,
				      args);
	node.toWrittenString = function() { return "(js-button ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function() { return node; }
	return node;
    };


    Jsworld.buttonStar = function(worldUpdateF, effectF, args) {
	var attribs = getAttribs(args);
	function wrappedF(world, evt) {
	    try {
		plt.world.Kernel.applyEffect(effectF([world]));
		var result = worldUpdateF([world]);
		return result;
	    } catch (e) {
		plt.Kernel.reportError(e);
		return world;
	    }
	}
	var node = _js.button(wrappedF, attribs);
	node.toWrittenString = function() { return "(js-button ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function() { return node; }
	return node;
    };
    

    // input.
    Jsworld.input = function(type, updateF, args) {
	plt.Kernel.check(type, plt.Kernel.isString, "js-input", "string", 1);
	plt.Kernel.check(updateF, plt.Kernel.isFunction, "js-input", "(world string -> world)", 1);
	var attribs = getAttribs(args);
	var node = _js.input(type,
			     function(w, v) { 
				 return updateF([w, v])
			     },
			     attribs);
	node.toWrittenString = function() { return "(js-input ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function() { return node; }
	return node;
    };


    Jsworld.get_dash_input_dash_value = function(node) {
	plt.Kernel.check(node, 
			 function(x) { return (plt.Kernel.isString(node) ||
					       node.nodeType == 
					       Node.ELEMENT_NODE) }, 
			 "get-input-value",
			 "dom-node",
			 1);
	if (plt.Kernel.isString(node)) {
	    return plt.types.String.makeInstance(document.getElementById(node).value || "");
	} else {
	    return plt.types.String.makeInstance(node.value || "");
	}

    };


//     // BidirectionalInput
//     Jsworld.bidirectionalInput = function(type, valF, updateF, args) {
// 	var attribs = getAttribs(args);
// 	var node = _js.bidirectional_input(type,
// 				       function(w) { return valF([w]) },
// 				       function(w, v) { 
// 					   return updateF([w, v])
// 				       },
// 				       attribs);
// 	node.toWrittenString = function() { return "(js-bidirectional-input ...)"; }
// 	node.toDisplayedString = node.toWrittenString;
// 	node.toDomNode = function() { return node; }
// 	return node;
//     };

    // Images.
    Jsworld.img = function(src, args) {
	var attribs = getAttribs(args);
	var node = _js.img(src, attribs);
	node.toWrittenString = function() { return "(js-img ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function() { return node; }
	return node;
    };


    // text: string -> node
    Jsworld.text = function(s) {
	var node = _js.text(s, []);
	node.toWrittenString = function() { return "(js-img ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function() { return node; }
	return node;
    };



    // fixme: add support for select, option, textarea, h1, canvas


    // raw_node: scheme-value assoc -> node
    Jsworld.rawNode = function(x, args) {
	var attribs = getAttribs(args);
	var node = _js.raw_node(plt.Kernel.toDomNode(x), attribs);
	node.toWrittenString = function() { return "(js-img ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function() { return node; }
	return node;
    };



})();
