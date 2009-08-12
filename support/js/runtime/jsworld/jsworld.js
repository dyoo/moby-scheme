
if(typeof(plt) == 'undefined') {   
    var plt = {};
}

plt.Jsworld = {};




// Stuff here is copy-and-pasted from Chris's JSWorld.  We
// namespace-protect it, and add the Javascript <-> Moby wrapper
// functions here.

(function() {

    var Jsworld = plt.Jsworld;


    var currentFocusedNode = false;



    //
    // WORLD STUFFS
    //

    function InitialWorld() {}

    var world = new InitialWorld();
    var worldListeners = [];
    var runningBigBangs = [];




    function add_world_listener(listener) {
	worldListeners.push(listener);
    }


    function remove_world_listener(listener) {
	var index = worldListeners.indexOf(listener);
	if (index != -1) {
	    worldListeners.splice(index, 1);
	}
    }

    function clear_running_state() {
	world = new InitialWorld();
	worldListeners = [];
    }



    // change_world: (world -> world) -> void
    // Adjust the world, and notify all listeners.
    function change_world(updater) {
	var originalWorld = world;
	try {
	    world = updater(world);
	} catch(e) {
	    return;
	}
	if (originalWorld != world) {
	    for(var i = 0; i < worldListeners.length; i++) {
		try {
		    worldListeners[i](world, originalWorld);
		} catch (e) {
		    alert(e);
		}
	    }
	}
    }
    Jsworld.change_world = change_world;




    //
    // STUFF THAT SHOULD REALLY BE IN ECMASCRIPT
    //
    Number.prototype.NaN0=function(){return isNaN(this)?0:this;}
    function getPosition(e){
	var left = 0;
	var top  = 0;
	while (e.offsetParent){
	    left += e.offsetLeft + (e.currentStyle?(parseInt(e.currentStyle.borderLeftWidth)).NaN0():0);
	    top  += e.offsetTop  + (e.currentStyle?(parseInt(e.currentStyle.borderTopWidth)).NaN0():0);
	    e     = e.offsetParent;
	}
	left += e.offsetLeft + (e.currentStyle?(parseInt(e.currentStyle.borderLeftWidth)).NaN0():0);
	top  += e.offsetTop  + (e.currentStyle?(parseInt(e.currentStyle.borderTopWidth)).NaN0():0);
	return {x:left, y:top};	
    }
    Jsworld.getPosition = getPosition;


    var gensym_counter = 0;
    function gensym(){ return gensym_counter++;}
    Jsworld.gensym = gensym;


    function map(a, f) {
	var b = new Array(a.length);
	for (var i = 0; i < a.length; i++) b[i] = f(a[i]);
	return b;
    }
    Jsworld.map = map;



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
    Jsworld.mapi = mapi;


    function fold(a, x, f) {
	for (var i = 0; i < a.length; i++)
	    x = f(a[i], x);
	return x;
    }
    Jsworld.fold = fold;


    function augment(o, a) {
	var oo = {};
	for (var e in o)
	    oo[e] = o[e];
	for (var e in a)
	    oo[e] = a[e];
	return oo;
    }
    Jsworld.augment = augment;


    function assoc_cons(o, k, v) {
	var oo = {};
	for (var e in o)
	    oo[e] = o[e];
	oo[k] = v;
	return oo;
    }
    Jsworld.assoc_cons = assoc_cons;


    function cons(array, value) {
	return array.concat([value]);
    }
    Jsworld.cons = cons;


    function append(array1, array2){
	return array1.concat(array2);
    }
    Jsworld.append = append;

    function array_join(array1, array2){
	var joined = [];
	for (var i = 0; i < array1.length; i++)
	    joined.push([array1[i], array2[i]]);
	return joined;
    }
    Jsworld.array_join = array_join;


    function removeq(array, value) {
	for (var i = 0; i < array.length; i++)
	    if (array[i] === value){
		return array.slice(0, i).concat(array.slice(i+1));
	    }			
	return array;
    }
    Jsworld.removeq = removeq;

    function removef(array, f) {
	for (var i = 0; i < array.length; i++)
	    if (f(array[i]))
		return array.slice(0, i).concat(array.slice(i+1));
	return array;
    }
    Jsworld.removef = removef;


    function filter(array, f) {
	for (var i = 0; i < array.length; i++)
	    if (f(array[i]))
		return array.slice(0, i).concat(filter(array.slice(i+1), f));
	return array;
    }
    Jsworld.filter = filter;


    function without(obj, attrib) {
	var o = {};
	for (var a in obj)
	    if (a != attrib)
		o[a] = obj[a];
	return o;
    }
    Jsworld.without = without;


    function memberq(a, x) {
	for (var i = 0; i < a.length; i++)
	    if (a[i] === x) return true;
	return false;
    }
    Jsworld.memberq = memberq;


    function head(a){
	return a[0];
    }
    Jsworld.head = head;


    function tail(a){
	return a.slice(1, a.length);
    }
    Jsworld.tail = tail;

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


    // node_to_tree: dom -> dom-tree
    // Given a native dom node, produces the appropriate tree.
    function node_to_tree(domNode) {
	var result = [domNode];
	for (var c = domNode.firstChild; c != null; c = c.nextSibling) {
	    result.push(node_to_tree(c));
	}
	return result;
    }
    Jsworld.node_to_tree = node_to_tree;



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


    // update_dom(nodes(Node), relations(Node)) = void
    function update_dom(toplevelNode, nodes, relations) {

	// TODO: rewrite this to move stuff all in one go... possible? necessary?
	
	// move all children to their proper parents
	for (var i = 0; i < relations.length; i++)
	    if (relations[i].relation == 'parent') {
		var parent = relations[i].parent, child = relations[i].child;
			
		if (child.parentNode !== parent) {
		    parent.appendChild(child);
		}
	    }
	
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
	
	var node = toplevelNode, stop = toplevelNode.parentNode;
	while (node != stop) {
	    for (;;) {
		// process first
		// move down
		if (node.firstChild == null) break;
		node = node.firstChild;
	    }
		
	    while (node != stop) {
		var next = node.nextSibling, parent = node.parentNode;
			
		// process last
		var found = false;
			
		if (live_nodes != null)
		    while (live_nodes.length > 0 && positionComparator(node, live_nodes[0]) >= 0) {
			var other_node = live_nodes.shift();
			if (other_node === node) {
			    found = true;
			    break;
			}
			// need to think about this
			//live_nodes.push(other_node);
		    }
		else
		    for (var i = 0; i < nodes.length; i++)
			if (nodes[i] === node) {
			    found = true;
			    break;
			}
			
		if (!found) {
		    // reparent children, remove node
		    while (node.firstChild != null)
			node.parentNode.appendChild(node.firstChild);
				
		    next = node.nextSibling; // HACKY
				
		    node.parentNode.removeChild(node);
		}
			
		// move sideways
		if (next == null) node = parent;
		else { node = next; break; }
	    }
	}
	
	refresh_node_values(nodes);
    }

    function set_css_attribs(node, attribs) {
	for (var j = 0; j < attribs.length; j++){
	    node.style.setProperty(attribs[j].attrib, attribs[j].values.join(" "), "");
	}
		
    }

    function update_css(nodes, css) {
	// clear CSS
	for (var i = 0; i < nodes.length; i++)
	    if ('style' in nodes[i])
		nodes[i].style.cssText = "";
	
	// set CSS
	for (var i = 0; i < css.length; i++)
	    if ('id' in css[i]) {
		for (var j = 0; j < nodes.length; j++)
		    if ('id' in nodes[j] && nodes[j].id == css[i].id){
			set_css_attribs(nodes[j], css[i].attribs);
		    }
	    }
	    else set_css_attribs(css[i].node, css[i].attribs);
    }



    // If any node cares about the world, send it in.
    function refresh_node_values(nodes) {
	for (var i = 0; i < nodes.length; i++) {
	    if (nodes[i].onWorldChange) {
		nodes[i].onWorldChange(world);
	    }
	}
    }



    function do_redraw(world, oldWorld, toplevelNode, redraw_func, redraw_css_func) {
	if (oldWorld instanceof InitialWorld) {
	    // Simple path
	    var t = sexp2tree(redraw_func(world));
	    var ns = nodes(t);
	    update_dom(toplevelNode, ns, relations(t));
	    update_css(ns, sexp2css(redraw_css_func(world)));
	    return;
	} else {
	    var currentFocusedSelection = getCurrentFocusedSelection();

	    // We try to avoid updating the dom if the value
	    // hasn't changed.
 	    var oldRedraw = redraw_func(oldWorld);
 	    var newRedraw = redraw_func(world);	    
 	    var oldRedrawCss = redraw_css_func(oldWorld);
	    var newRedrawCss = redraw_css_func(world);
	    var t = sexp2tree(newRedraw);
 	    var ns = nodes(t);

	    // Try to save the current selection and preserve it across
	    // dom updates.

 	    if(oldRedraw != newRedraw) {
 		update_dom(toplevelNode, ns, relations(t));
 		update_css(ns, sexp2css(newRedrawCss));
 	    } else {
		if(oldRedrawCss != newRedrawCss) {
 		    update_css(ns, sexp2css(newRedrawCss));
		}
 	    }
	    currentFocusedSelection.restore();
	}
    }


    function FocusedSelection() {
	this.focused = currentFocusedNode;
	this.selectionStart = currentFocusedNode.selectionStart;
	this.selectionEnd = currentFocusedNode.selectionEnd;
    }

    // Try to restore the focus.
    FocusedSelection.prototype.restore = function() {
	// FIXME: if we're scrolling through, what's visible
	// isn't restored yet.
	if (this.focused.parentNode) {
	    this.focused.selectionStart = this.selectionStart;
	    this.focused.selectionEnd = this.selectionEnd;
	    this.focused.focus();
	} else if (this.focused.id) {
	    var matching = document.getElementById(this.focused.id);
	    if (matching) {
		matching.selectionStart = this.selectionStart;
		matching.selectionEnd = this.selectionEnd;
		matching.focus();
	    }
	}
    };

    function getCurrentFocusedSelection() {
	return new FocusedSelection();
    }



    //////////////////////////////////////////////////////////////////////

    function BigBangRecord(top, world, handlerCreators, handlers, attribs) {    
	this.top = top;
	this.world = world;
	this.handlers = handlers;
	this.handlerCreators = handlerCreators;
	this.attribs = attribs;
    }

    BigBangRecord.prototype.restart = function() {
	big_bang(this.top, this.world, this.handlerCreators, this.attribs);
    }
    
    BigBangRecord.prototype.pause = function() {
	for(var i = 0 ; i < this.handlers.length; i++) {
	    if (this.handlers[i] instanceof StopWhenHandler) {
		// Do nothing for now.
	    } else {
		this.handlers[i].onUnregister(top);
	    }
	}
    };
    //////////////////////////////////////////////////////////////////////

    // Notes: big_bang maintains a stack of activation records; it should be possible
    // to call big_bang re-entrantly.
    function big_bang(top, init_world, handlerCreators, attribs) {
	clear_running_state();

	// Construct a fresh set of the handlers.
	var handlers = map(handlerCreators, function(x) { return x();} );
	if (runningBigBangs.length > 0) { 
	    runningBigBangs[runningBigBangs.length - 1].pause();
	}

	// Create an activation record for this big-bang.
	var activationRecord = 
	    new BigBangRecord(top, init_world, handlerCreators, handlers, attribs);
	runningBigBangs.push(activationRecord);
	function keepRecordUpToDate(w, oldW) {
	    activationRecord.world = w;
	}
	add_world_listener(keepRecordUpToDate);



	// Monitor for termination and register the other handlers.
	var stopWhen = new StopWhenHandler(function(w) { return false; },
					   function(w) {});
	for(var i = 0 ; i < handlers.length; i++) {
	    if (handlers[i] instanceof StopWhenHandler) {
		stopWhen = handlers[i];
	    } else {
		handlers[i].onRegister(top);
	    }
	}
	function watchForTermination(w, oldW) {
	    if (stopWhen.test(w)) {
		stopWhen.receiver(world);		    
		var currentRecord = runningBigBangs.pop();
		currentRecord.pause();
		if(runningBigBangs.length > 0) {
		    var restartingBigBang = runningBigBangs.pop();
		    restartingBigBang.restart();
		}
	    }
	};
	add_world_listener(watchForTermination);


	// Finally, begin the big-bang.
	copy_attribs(top, attribs);
	change_world(function(w) { return init_world; });


    }
    Jsworld.big_bang = big_bang;





    // on_tick: number (world -> world) -> handler
    function on_tick(delay, tick) {
	return function() {
	    var ticker = {
		watchId: -1,
		onRegister: function (top) { 
		    ticker.watchId = setInterval(function() { change_world(tick); },
						 delay);
		},

		onUnregister: function (top) {
		    clearInterval(ticker.watchId);
		}
	    };
	    return ticker;
	};
    }
    Jsworld.on_tick = on_tick;


    
    //  on_draw: (world -> (sexpof node)) (world -> (sexpof css-style)) -> handler
    function on_draw(redraw, redraw_css) {
	function wrappedRedraw(w) {
	    var newDomTree = redraw(w);
	    checkDomSexp(newDomTree);
	    return newDomTree;
	}

	return function() {
	    var drawer = {
		_top: null,
		_listener: function(w, oldW) { 
		    do_redraw(w, oldW, drawer._top, wrappedRedraw, redraw_css); 
		},
		onRegister: function (top) { 
		    drawer._top = top;
		    add_world_listener(drawer._listener);
		},

		onUnregister: function (top) {
		    remove_world_listener(drawer._listener);
		}
	    };
	    return drawer;
	};
    }
    Jsworld.on_draw = on_draw;



    function StopWhenHandler(test, receiver) {
	this.test = test;
	this.receiver = receiver;
    }
    // stop_when: (world -> boolean) (world -> boolean) -> handler
    function stop_when(test, receiver) {
	return function() {
	    if (receiver == undefined) {
		receiver = function(world) {};
	    }
	    return new StopWhenHandler(test, receiver);
	};
    }
    Jsworld.stop_when = stop_when;




    //
    // DOM CREATION STUFFS
    //

    // add_ev: node string (world event -> world) -> void
    // Attaches a world-updating handler when the world is changed.
    function add_ev(node, event, f) {
	node.addEventListener(event, 
			      function (e) { 
				  change_world(function(w) { 
					  return f(w, e); 
				      }) }, 
			      false);
    }

    // add_ev_after: node string (world event -> world) -> void
    // Attaches a world-updating handler when the world is changed, but only
    // after the fired event has finished.
    function add_ev_after(node, event, f) {
	node.addEventListener(event, 
			      function (e) {
				  setTimeout(
				      function() {
					  change_world(function(w) { 
					      return f(w, e);
					  });

				      }, 0);
			      },
			      false);
    }


    function addFocusTracking(node) {
	node.addEventListener("focus",
			      function(e) {
				  currentFocusedNode = node;
			      },
			      false);

	node.addEventListener("blur",
			      function(e) {
				  currentFocusedNode = undefined;
			      },
			      false);
	return node;
    }





    //
    // WORLD STUFFS
    //


    function sexp2tree(sexp) {
	if(sexp.length == undefined) return { node: sexp, children: [] };
	else return { node: sexp[0], children: map(sexp.slice(1), sexp2tree) };
    }

    function sexp2attrib(sexp) {
	return { attrib: sexp[0], values: sexp.slice(1) };
    }

    function sexp2css_node(sexp) {
	var attribs = map(sexp.slice(1), sexp2attrib);
	if (typeof sexp[0] == 'string'){
	    return [{ id: sexp[0], attribs: attribs }];
	} else if ('length' in sexp[0]){
	    return map(sexp[0], function (id) { return { id: id, attribs: attribs } });
	} else {
	    return [{ node: sexp[0], attribs: attribs }];
	}
    }

    function sexp2css(sexp) {
	return concat_map(sexp, sexp2css_node);
    }






    // checkDomSexp: X -> boolean
    // Checks to see if thing is a DOM-sexp.  If not,
    // throws an object that explains why not.
    function checkDomSexp(thing) {
	if (! thing instanceof Array) {
	    throw new JsworldDomError("Expected a non-empty array",
				      thing);
	}
	if (thing.length == 0) {
	    throw new JsworldDomError("Expected a non-empty array",
				      thing);
	}

	// Check that the first element is a Text or an element.
	if (thing[0] instanceof Text) {
	    if (thing.length > 1) {
		throw new JsworldDomError("Text nodes can not have children",
					  thing);
	    }
	} else if (thing[0] instanceof Element) {
	    for (var i = 1; i < thing.length; i++) {
		checkDomSexp(thing[i]);
	    }
	} else {
	    throw new JsworldDomError("expected a Text or an Element",
				      thing[0]);
	}
    }

    function JsworldDomError(msg, elt) {
	this.msg = msg;
	this.elt = elt;
    }
    JsworldDomError.prototype.toString = function() {
	return this.msg + ": " + this.elt;
    }





    //
    // DOM CREATION STUFFS
    //


    function copy_attribs(node, attribs) {
	if (attribs)
	    for (a in attribs)
		if (typeof attribs[a] == 'function')
		    add_ev(node, a, attribs[a]);
		else{
		    node[a] = attribs[a];//eval("node."+a+"='"+attribs[a]+"'");
		}
	return node;
    }


    //
    // NODE TYPES
    //

    function p(attribs) {
	return addFocusTracking(copy_attribs(document.createElement('p'), attribs));
    }
    Jsworld.p = p;

    function div(attribs) {
	return addFocusTracking(copy_attribs(document.createElement('div'), attribs));
    }
    Jsworld.div = div;

    function button(f, attribs) {
	var n = document.createElement('button');
	add_ev(n, 'click', f);
	return addFocusTracking(copy_attribs(n, attribs));
    }
    Jsworld.button = button;


    function bidirectional_input(type, toVal, updateVal, attribs) {
	var n = document.createElement('input');
	n.type = type;
	function onKey(w, e) {
	    return updateVal(w, n.value);
	}
	// This established the widget->world direction
	add_ev_after(n, 'keypress', onKey);
	// and this establishes the world->widget.
	n.onWorldChange = function(w) {n.value = toVal(w)};
	return addFocusTracking(copy_attribs(n, attribs));
    }
    Jsworld.bidirectional_input = bidirectional_input;
    

    function input(type, attribs) {
	var n = document.createElement('input');
	n.type = type;
	return addFocusTracking(copy_attribs(n, attribs));
    }
    Jsworld.input = input;

    function text(s, attribs) {
	return addFocusTracking(copy_attribs(document.createTextNode(s), attribs));
    }
    Jsworld.text = text;

    function select(attribs, opts, f){
	var n = document.createElement('select');
	for(var i = 0; i < opts.length; i++)
	    n.appendChild(option({value: opts[i]}));
	add_ev(n, 'change', f);
	return addFocusTracking(copy_attribs(n, attribs));
    }
    Jsworld.select = select;

    function option(attribs){
	return addFocusTracking(copy_attribs(document.createElement('option'), attribs));
    }
    Jsworld.option = option;

    function textarea(attribs){
	return addFocusTracking(copy_attribs(document.createElement('textarea'), attribs));
    }
    Jsworld.textarea = textarea;

    function h1(attribs){
	return addFocusTracking(copy_attribs(document.createElement('h1'), attribs));
    }
    Jsworld.h1 = h1;

    function canvas(attribs){
	return addFocusTracking(copy_attribs(document.createElement('canvas'), attribs));	
    }
    Jsworld.canvas = canvas;


    function img(src, attribs) {
	var n = document.createElement('img');
	n.src = src;
	return addFocusTracking(copy_attribs(n, attribs));
    }
    Jsworld.img = img;


})();