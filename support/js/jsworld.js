org.plt.world.Jsworld = {};



// Stuff here is copy-and-pasted from Chris's JSWorld.  We
// namespace-protect it, and add the Javascript <-> Moby wrapper
// functions here.

(function() {

    var Jsworld = org.plt.world.Jsworld;



    var toplevelNode = document.body;





    //
    // STUFF THAT SHOULD REALLY BE IN ECMASCRIPT
    //

    function map(a, f) {
	var b = new Array(a.length);
	for (var i = 0; i < a.length; i++) b[i] = f(a[i]);
	return b;
    }

    function concat_map(a, f) {
	var b = [];
	for (var i = 0; i < a.length; i++) b = b.concat(f(a[i]));
	return b;
    }

    function mapi(a, f) {
	var b = new Array(a.length);
	for (var i = 0; i < a.length; i++) b[i] = f(a[i], i);
	return b;
    }

    function fold(a, x, f) {
	for (var i = 0; i < a.length; i++)
	    x = f(a[i], x);
	return x;
    }

    function augment(o, a) {
	var oo = {};
	for (var e in o)
	    oo[e] = o[e];
	for (var e in a)
	    oo[e] = a[e];
	return oo;
    }

    function assoc_cons(o, k, v) {
	var oo = {};
	for (var e in o)
	    oo[e] = o[e];
	oo[k] = v;
	return oo;
    }

    function cons(array, value) {
	return array.concat([value]);
    }

    function removeq(array, value) {
	for (var i = 0; i < array.length; i++)
	    if (array[i] === value)
		return array.slice(0, i).concat(array.slice(i+1));
	return array;
    }

    function removef(array, f) {
	for (var i = 0; i < array.length; i++)
	    if (f(array[i]))
		return array.slice(0, i).concat(array.slice(i+1));
	return array;
    }

    function without(obj, attrib) {
	var o = {};
	for (var a in obj)
	    if (a != attrib)
		o[a] = obj[a];
	return o;
    }

    function memberq(a, x) {
	for (var i = 0; i < a.length; i++)
	    if (a[i] === x) return true;
	return false;
    }

    //
    // DOM UPDATING STUFFS
    //

    // tree(N): { node: N, children: [tree(N)] }
    // relation(N): { relation: 'parent', parent: N, child: N } | { relation: 'neighbor', left: N, right: N }
    // relations(N): [relation(N)]
    // nodes(N): [N]
    // css(N): [css_node(N)]
    // css_node(N): { node: N, attribs: attribs } | { className: string, attribs: attribs }
    // attrib: { attrib: string, values: [string] }
    // attribs: [attrib]

    // treeable(nodes(N), relations(N)) = bool
    /*function treeable(nodes, relations) {
    // for all neighbor relations between x and y
    for (var i = 0; i < relations.length; i++)
    if (relations[i].relation == 'neighbor') {
    var x = relations[i].left, y = relations[i].right;
			
    // there does not exist a neighbor relation between x and z!=y or z!=x and y
    for (var j = 0; j < relations.length; j++)
    if (relations[j].relation === 'neighbor')
    if (relations[j].left === x && relations[j].right !== y ||
    relations[j].left !== x && relations[j].right === y)
    return false;
    }
	
    // for all parent relations between x and y
    for (var i = 0; i < relations.length; i++)
    if (relations[i].relation == 'parent') {
    var x = relations[i].parent, y = relations[i].child;
			
    // there does not exist a parent relation between z!=x and y
    for (var j = 0; j < relations.length; j++)
    if (relations[j].relation == 'parent')
    if (relations[j].parent !== x && relations[j].child === y)
    return false;
    }
	
    // for all neighbor relations between x and y
    for (var i = 0; i < relations.length; i++)
    if (relations[i].relation == 'neighbor') {
    var x = relations[i].left, y = relations[i].right;
			
    // all parent relations between z and x or y share the same z
    for (var j = 0; j < relations.length; j++)
    if (relations[j].relation == 'parent')
    for (var k = 0; k < relations.length; k++)
    if (relations[k].relation == 'parent')
    if (relations[j].child === x && relations[k].child === y &&
    relations[j].parent !== relations[k].parent)
    return false;
    }
	
    return true;
    }*/

    // nodes(tree(N)) = nodes(N)
    function nodes(tree) {
	var ret = [tree.node];
	
	for (var i = 0; i < tree.children.length; i++)
	    ret = ret.concat(nodes(tree.children[i]));
	
	return ret;
    }

    // relations(tree(N)) = relations(N)
    function relations(tree) {
	var ret = [];
	
	for (var i = 0; i < tree.children.length; i++)
	    ret.push({ relation: 'parent', parent: tree.node, child: tree.children[i].node });
	
	for (var i = 0; i < tree.children.length - 1; i++)
	    ret.push({ relation: 'neighbor', left: tree.children[i].node, right: tree.children[i + 1].node });
	
	for (var i = 0; i < tree.children.length; i++)
	    ret = ret.concat(relations(tree.children[i]));
	
	return ret;
    }

    // pc stuff

    var pc_times = {}, pc_counts = {};
    var pc_time;

    function pcClear() {
	pc_time = new Date().getTime();
    }

    function pcRead(label) {
	var then = pc_time;
	pc_time = new Date().getTime();
	
	if (label in pc_times) {
	    pc_times[label] += pc_time - then;
	    pc_counts[label]++;
	}
	else {
	    pc_times[label] = pc_time - then;
	    pc_counts[label] = 1;
	}
    }

    function pcDump() {
	var s = [];
	
	for (label in pc_times)
	    s.push(label + ": " + (pc_times[label] / pc_counts[label]));
	
	return s;
    }

    // update_dom(nodes(Node), relations(Node)) = void
    function update_dom(nodes, relations) {
	pcClear();
	
	// move all children to their proper parents
	for (var i = 0; i < relations.length; i++)
	    if (relations[i].relation == 'parent') {
		var parent = relations[i].parent, child = relations[i].child;
			
		if (child.parentNode === null || !parent.isSameNode(child.parentNode))
		    parent.appendChild(child);
	    }
	
	pcRead('move');
	
	// arrange siblings in proper order
	// truly terrible... BUBBLE SORT
	for (;;) {
	    var unsorted = false;
		
	    for (var i = 0; i < relations.length; i++)
		if (relations[i].relation == 'neighbor') {
		    var left = relations[i].left, right = relations[i].right;
				
		    if (left.nextSibling !== right) {
			left.parentNode.insertBefore(left, right)
			    unsorted = true;
		    }
		}
		
	    if (!unsorted) break;
	}
	
	pcRead('sort');
	
	// remove dead nodes
	var live_nodes;
	
	// it is my hope that by sorting the nodes we get the worse of
	// O(n*log n) or O(m) performance instead of O(n*m)
	// for all I know Node.compareDocumentPosition is O(m)
	// and now we get O(n*m*log n)
	function positionComparator(a, b) {
	    var rel = a.compareDocumentPosition(b);
	    // children first
	    if (rel & a.DOCUMENT_POSITION_CONTAINED_BY) return 1;
	    if (rel & a.DOCUMENT_POSITION_CONTAINS) return -1;
	    // otherwise use precedes/follows
	    if (rel & a.DOCUMENT_POSITION_FOLLOWING) return -1;
	    if (rel & a.DOCUMENT_POSITION_PRECEDING) return 1;
	    // otherwise same node or don't care, we'll skip it anyway
	    return 0;
	}
	
	try {
	    // don't take out concat, it doubles as a shallow copy
	    // (as well as ensuring we keep document.body)
	    live_nodes = nodes.concat(toplevelNode).sort(positionComparator);
	}
	catch (e) {
	    // probably doesn't implement Node.compareDocumentPosition
	    live_nodes = null;
	}
	
	pcRead('prune sort');
	
	var node = toplevelNode, stop = toplevelNode.parentNode;
	while (node !== stop) {
	    for (;;) {
		// process first
		// move down
		if (node.firstChild === null) break;
		node = node.firstChild;
	    }
		
	    while (node !== stop) {
		var next = node.nextSibling, parent = node.parentNode;
			
		// process last
		var found = false;
			
		if (live_nodes !== null)
		    while (live_nodes.length > 0 && node.isSameNode(live_nodes[0])) {
			var other_node = live_nodes.shift();
			found = true;
			break;
		    }
		else
		    for (var i = 0; i < nodes.length; i++)
			if (nodes[i] === node) {
			    found = true;
			    break;
			}
			
		if (!found) {
		    // reparent children, remove node
		    while (node.firstChild !== null)
			node.parentNode.appendChild(node.firstChild);
				
		    next = node.nextSibling; // HACKY
				
		    node.parentNode.removeChild(node);
		}
			
		// move sideways
		if (next === null) node = parent;
		else { node = next; break; }
	    }
	}
	
	pcRead('prune');
    }

    function set_css_attribs(node, attribs) {
	for (var j = 0; j < attribs.length; j++)
	    node.style.setProperty(attribs[j].attrib, attribs[j].values.join(" "), '');
    }

    function update_css(nodes, css) {
	// clear CSS
	for (var i = 0; i < nodes.length; i++)
	    if ('style' in nodes[i])
		nodes[i].style.cssText = "";
	
	// set CSS
	for (var i = 0; i < css.length; i++)
	    if ('className' in css[i]) {
		for (var j = 0; j < nodes.length; j++)
		    if (nodes[j].className == css[i].className)
			set_css_attribs(nodes[j], css[i].attribs);
	    }
	    else set_css_attribs(css[i].node, css[i].attribs);
    }

    //
    // WORLD STUFFS
    //

    var world, redraw_func, redraw_css_func, tick_func;


    function sexp2tree(sexp) {
	return { node: sexp[0], children: map(sexp.slice(1), sexp2tree) };
    }

    function sexp2attrib(sexp) {
	return { attrib: sexp[0], values: sexp.slice(1) };
    }

    function sexp2css_node(sexp) {
	var attribs = map(sexp.slice(1), sexp2attrib);
	if (typeof sexp[0] == 'string')
	    return { className: sexp[0], attribs: attribs };
	else if ('length' in sexp[0])
	    return map(sexp[0], function (node) { return { node: node, attribs: attribs } });
	else
	    return { node: sexp[0], attribs: attribs };
    }

    function sexp2css(sexp) {
	return concat_map(sexp, sexp2css_node);
    }

    function do_redraw() {
	var t = sexp2tree(redraw_func(world));
	var ns = nodes(t);
	update_dom(ns, relations(t));
	update_css(ns, sexp2css(redraw_css_func(world)));
    }

    function big_bang(init_world, redraw, redraw_css, delay, tick, attribs) {
	world = init_world;
	redraw_func = redraw;
	redraw_css_func = redraw_css;
	tick_func = tick;
	do_redraw();
	setInterval(function () { world = tick(world); do_redraw(); }, delay);
	// do we want something for body too?
	//copy_attribs(toplevelNode, attribs);
	copy_attribs(window, attribs);
    }

    //
    // DOM CREATION STUFFS
    //

    // apparently add_event is taken...
    function add_ev(node, event, f) {
	node.addEventListener(event, function (e) { world = f(world, e); do_redraw(); }, false);
    }

    function copy_attribs(node, attribs) {
	if (attribs)
	    for (a in attribs)
		if (typeof attribs[a] == 'function')
		    add_ev(node, a, attribs[a]);
		else
		    node[a] = attribs[a];
	return node;
    }

    function p(attribs) {
	return copy_attribs(document.createElement('p'), attribs);
    }

    function div(attribs) {
	return copy_attribs(document.createElement('div'), attribs);
    }

    function button(f, attribs) {
	var n = document.createElement('button');
	add_ev(n, 'click', f);
	return copy_attribs(n, attribs);
    }

    function input(type, attribs) {
	var n = document.createElement('input');
	n.type = type;
	return copy_attribs(n, attribs);
    }

    function text(s, attribs) {
	return copy_attribs(document.createTextNode(s), attribs);
    }






    //////////////////////////////////////////////////////////////////////
    //From this point forward, we define wrappers to integrate jsworld
    //with Moby.


    // deepListToArray: any -> any
    // Converts list structure to array structure.
    function deepListToArray(thing) {
	if (org.plt.Kernel.empty_question_(thing)) {
	    return [];
	} else if (org.plt.Kernel.pair_question_(thing)) {
	    var result = [];
	    while (!thing.isEmpty()) {
		result.push(deepListToArray(aList.first()));
		thing = thing.rest();
	    }
	    return result;
	} else {
	    return thing;
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


    // changeWorld: (world -> world) -> void
    // Trigger a change in the world due to an updater, and redraw.
    function changeWorld(f) {
	world = f(world); 
	do_redraw();	
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



    // Exports:
    // bigBang: world (world -> dom) (world -> (listof css-style)) number (world -> world) (listof (list string string)) -> world
    Jsworld.bigBang = function(initWorld,
			       redraw, 
			       redrawCss, 
			       delay, 
			       tick, 
			       attribs) {

	var mainWindow = getBigBangWindow();
	// KLUDGE: check with Chris to get newest version of jsworld that
	// consumes the toplevel node.
	toplevelNode = mainWindow.document.getElementById("jsworld-div");

	function wrappedRedraw(w) {
	    var result = [toplevelNode, 
			  [deepListToArray(redraw([w]))]];
	    return result;
	}

	function wrappedRedrawCss(w) {
	    return deepListToArray(redrawCss([w]));
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



    // p: assoc -> dom
    Jsworld.p = function(attribsAssocList) {
	return p(assocListToAssocArray(attribsAssocList));
    };

    // div: assoc -> dom
    Jsworld.div = function (attribsAssocList) {
	return div(assocListToAssocArray(attribsAssocList));
    };

    // button: (world -> world) assoc -> dom
    Jsworld.button = function(f, attribsAssocList) {
	return button(f, assocListToAssocArray(attribsAssocList));
    };

    // input: string assoc -> dom
    Jsworld.input = function(type, attribsAssocList) {
	return input(type, assocListToAssocArray(attribsAssocList));
    };

    // text: string assoc -> dom
    Jsworld.text = function(s, attribsAssocList) {
	return text(s, assocListToAssocArray(attribsAssocList));
    }



})();