if (typeof(plt) == 'undefined') { plt = {}; }
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
    var isHandler = function(x) {
	return typeof(x) == 'function';
    }




    //////////////////////////////////////////////////////////////////////
    //From this point forward, we define wrappers to integrate jsworld
    //with Moby.


    // deepListToArray: any -> any
    // Converts list structure to array structure.
    var deepListToArray = function(x) {
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
    var assocListToAssocArray = function(aList) {
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
    var getBigBangWindow = function() {
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



    var isList = function(x) {
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
    };



    // Figure out the target of an event.
    // http://www.quirksmode.org/js/events_properties.html#target
    var findEventTarget = function(e) {
	var targ;
	if (e.target) 
	    targ = e.target;
	else if (e.srcElement) 
	    targ = e.srcElement;
	if (targ.nodeType == 3) // defeat Safari bug
	    targ = targ.parentNode;
	return targ;
    }

    // isNode: any -> boolean
    // Returns true if the thing has a nodeType.
    var isNode = function(thing) {
	return typeof(thing.nodeType) != 'undefined';
    }


    // checkWellFormedDomTree: X X (or number undefined) -> void
    // Check to see if the tree is well formed.  If it isn't,
    // we need to raise a meaningful error so the user can repair
    // the structure.
    //
    // Invariants:
    // The dom tree must be a pair.
    // The first element must be a node.
    // Each of the rest of the elements must be dom trees.
    // If the first element is a text node, it must NOT have children.
    var checkWellFormedDomTree = function(x, top, index) {
	if (plt.Kernel.pair_question_(x)) {
	    var firstElt = plt.Kernel.first(x)
	    var restElts = plt.Kernel.rest(x)

	    if (! isNode(firstElt)) {
		throw new MobyTypeError(
		    plt.Kernel.format(
		         "on-draw: expected a dom-element, but received ~s instead, the first element within ~s",
			 [firstElt, top]));
	    }

	    if (firstElt.nodeType == Node.TEXT_NODE &&
		! plt.Kernel.empty_question_(restElts)) {
		throw new MobyTypeError(
		    plt.Kernel.format(
			"on-draw: the text node ~s must not have children.  It has ~s", 
			[firstElt, restElts]));
	    }

	    var i = 2;
	    while(! plt.Kernel.empty_question_(restElts)) {
		checkWellFormedDomTree(plt.Kernel.first(restElts),
				       x,
				       i);
		restElts = plt.Kernel.rest(restElts);
		i++;
	    }
	} else {
	    throw new MobyTypeError(
		plt.Kernel.format(
		    "on-draw: expected a dom-s-expression, but received ~s instead~a",
		    [x,
		     (index != undefined ? 
		      plt.Kernel.format(", the ~a element within ~s.", [plt.Kernel.ordinalize(index), top])
		      : 
		      ".")]));
	}
    };

    var attachEvent = function(node, eventName, fn) {
	plt.Kernel.attachEvent(node, eventName, fn);
    }

    var preventDefault = function(event) {
	if (event.preventDefault) {
	    event.preventDefault();
	} else {
	    event.returnValue = false;
	}
    }

    var stopPropagation = function(event) {
	if (event.stopPropagation) {
	    event.stopPropagation();
	} else {
	    event.cancelBubble = true;
	}
    }


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
	
	// Ensure that the toplevelNode can be focused by mouse or keyboard
	toplevelNode.tabIndex = 0;
	// Absorb all click events so they don't bubble up.
	attachEvent(toplevelNode,
		    'click',
		    function(e) {
			preventDefault(e);
			stopPropagation(e);
			return false;
		    },
		    false);
	

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
	var wrappedRedrawCss;
	

	if (config.lookup('onDraw')) {
	    wrappedRedraw = function(w) {
		try {
		    var newDomTree = config.lookup('onDraw')([w]);
		    plt.Kernel.setLastLoc(undefined);
		    checkWellFormedDomTree(newDomTree, newDomTree, undefined);
		    var result = [toplevelNode, 
				  deepListToArray(newDomTree)];
		    return result;
		} catch (e) {
		    handleError(e);
		    throw e;
		}
	    }

	    wrappedRedrawCss = function(w) {
		try {
		    var result = deepListToArray(config.lookup('onDrawCss')([w]));
		    plt.Kernel.setLastLoc(undefined);
		    return result;
		} catch (e) {
		    handleError(e);
		    throw e;
		}
	    }
	    wrappedHandlers.push(_js.on_draw(wrappedRedraw, wrappedRedrawCss));
	} else if (config.lookup('onRedraw')) {
	    var reusableCanvas = undefined;
	    var reusableCanvasNode = undefined;
	    
	    wrappedRedraw = function(w) {
		try {
		    var aScene = config.lookup('onRedraw')([w]);
		    // Performance hack: if we're using onRedraw, we know
		    // we've got a scene, so we optimize away the repeated
		    // construction of a canvas object.
		    if (aScene != null && aScene != undefined && 
			aScene instanceof plt.world.Kernel.BaseImage) {
			var width = 
			    plt.world.Kernel.imageWidth(aScene).toFixnum();
			var height = 
			    plt.world.Kernel.imageHeight(aScene).toFixnum();

			if (! reusableCanvas) {
			    reusableCanvas = plt.world.Kernel.makeCanvas(width, height);
			    // Note: the canvas object may itself manage objects,
			    // as in the case of an excanvas.  In that case, we must make
			    // sure jsworld doesn't try to disrupt its contents!
			    reusableCanvas.jsworldOpaque = true;
			    reusableCanvasNode = _js.node_to_tree(reusableCanvas);
			}

			reusableCanvas.width = width;
			reusableCanvas.height = height;			
			var ctx = reusableCanvas.getContext("2d");
			aScene.render(ctx, 0, 0);

			return [toplevelNode, reusableCanvasNode];
		    } else {
			return [toplevelNode, 
				_js.node_to_tree(
						 plt.Kernel.toDomNode(
								      aScene))];
		    }
		} catch (e) {
		    handleError(e);
		    throw e;
		}
	    }
	    
	    wrappedRedrawCss = function(w) {
		return [[reusableCanvas, 
			 ["width", reusableCanvas.width + "px"],
			 ["height", reusableCanvas.height + "px"]]];
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
	    // Add event handlers that listen in on key events that are applied
	    // directly on the toplevelNode.  We pay attention to keydown, and
	    // omit keypress.
	    attachEvent(toplevelNode,
			'keydown',
			function(e) {
			    plt.world.stimuli.onKey(e);
			    preventDefault(e);
			    stopPropagation(e);
			    return false;
			});
	    attachEvent(toplevelNode,
			'keypress',
			function(e) {
			    preventDefault(e);
			    stopPropagation(e);
			    return false;
			});
	    toplevelNode.focus();
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



    var arrayToList = function(anArray) {
	var result = plt.types.Empty.EMPTY;
	for(var i = 0; i < anArray.length; i++) {
	    result = plt.types.Cons.makeInstance(anArray[length-i-1],
						     result);
	}
	return result;
    }


    var handleError = function(e) {
	plt.Kernel.reportError(e);
	// When something bad happens, shut down 
	// the world computation.
	plt.Kernel.reportError("Shutting down jsworld computations");
	plt.world.stimuli.onShutdown(); 
    }
    


    // updateWorld: (world -> world) -> void
    Jsworld.updateWorld = function(updater) {
	var wrappedUpdater = function(world) {
	    try {
		return updater(world);
	    } catch (e) {
		handleError(e);
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


    var getAttribs = function(args) {
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
	node.toWrittenString = function(cache) { 
	    return plt.Kernel.format("(js-p)", []);
	};
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
	return node;
    };

    // div: assoc -> node
    Jsworld.div = function(args) {
	var attribs = getAttribs(args);
	var node = _js.div(attribs);
	node.toWrittenString = function(cache) { 
	    return plt.Kernel.format("(js-div)", []);
	};
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
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
	node.toWrittenString = function(cache) { return "(js-button ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
	return node;
    };


    Jsworld.buttonStar = function(worldUpdateF, effectF, args) {
	var attribs = getAttribs(args);
	var wrappedF = function(world, evt) {
	    try {
		var effect = effectF([world]);
		var newWorld = worldUpdateF([world]);
		plt.world.Kernel.applyEffect(effect);
		return newWorld;
	    } catch (e) {
		handleError(e);
		return world;
	    }
	}
	var node = _js.button(wrappedF, attribs);
	node.toWrittenString = function(cache) { return "(js-button ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
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
	node.toWrittenString = function(cache) { return "(js-input ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
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



    // Images.
    Jsworld.img = function(src, args) {
	plt.Kernel.check(src, plt.Kernel.isString, "js-img", "string", 1);
	var attribs = getAttribs(args);
	var node = _js.img(src, attribs);
	node.toWrittenString = function(cache) { return "(js-img ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
	return node;
    };


    // text: string -> node
    Jsworld.text = function(s) {
	plt.Kernel.check(s, plt.Kernel.isString, "js-text", "string", 1);
	var node = _js.text(s, []);
	node.toWrittenString = function(cache) { return "(js-text ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
	return node;
    };



    Jsworld.select = function(options, updateF, args) { 
	plt.Kernel.checkListof(options, plt.Kernel.isString, "js-select", "string", 1);
	plt.Kernel.check(updateF, plt.Kernel.isFunction, "js-select", "function", 2);
	var attribs = getAttribs(args);
	// convert options to array
	var optionsArray = deepListToArray(options);
	var node = _js.select(attribs, 
			      optionsArray,
			      function(w, selectEvent) { 
				  return updateF([w, node.value])
			      });
	node.toWrittenString = function(cache) { return "(js-select ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
	return node;
    };


    // fixme: add support for textarea, h1, canvas


    // raw_node: scheme-value assoc -> node
    Jsworld.rawNode = function(x, args) {
	var attribs = getAttribs(args);
	var node = _js.raw_node(plt.Kernel.toDomNode(x), attribs);
	node.toWrittenString = function(cache) { return "(js-raw-node ...)"; }
	node.toDisplayedString = node.toWrittenString;
	node.toDomNode = function(cache) { return node; }
	return node;
    };



})();
