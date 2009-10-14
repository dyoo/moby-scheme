var jsworld = plt.Jsworld;


var init_world = {
	expressions:[], // list of expression trees
	counter:	1,
	styles:		[], // list of { node: node, x: int, y: int,... }
	targets:	[],
	
	drag_info: {
		dragging: [], // list of nodes
		init_x: 0, init_y: 0,
		delta_x: 0, delta_y: 0
	}
};

function worldToString(world){
	var exps	= jsworld.map(world.expressions, expToString);
	var styles	= jsworld.map(world.styles, function(s){
										var str = "";
										for (var e in s) str+=s+': '+s[e]
										  return '{'+str+'}';
					  });
	return '{expressions: '+exps+'\n\ncounter: '+world.counter+'\n\nstyles: '+styles+'\n\ntargets: '+world.targets+'}';
}

/***************************************************************
 *	EXPRESSION TREE STUFF
 ***************************************************************/
function expToString(exp){
	if(exp.exp == undefined) return '{exp: value, value:'+exp.value+', id: '+exp.id+'}';
	switch(exp.exp){
		case 'function':return '{exp:define, inputs: ['+jsworld.map(exp.inputs, expToString)+'], output:'+expToString(exp.output)+', body: '+expToString(exp.body)+'}'; break;
		case 'define':	return '{exp:define, id: '+exp.id+' value:'+expToString(exp.value); break;
		case 'apply':	return '{exp:apply, id: '+exp.id+' fn:'+exp.fn+', inputs: ['+jsworld.map(exp.inputs, expToString)+']}'; break;
		default: return 'ERROR - invalid expression type';		
	}
        return 'ERROR - invalid expression type';
}

function styleToString(s){
	var str = 'className:'+s.className+',position:'+s.position+',left:'+s.left+',top'+s.top+',opacity';
	return str;
}

function findExpById(id, world){
	var find = function(exp, acc){
		if(exp.exp == undefined) return (exp.id == id)? jsworld.cons(acc, exp) : acc;
		switch(exp.exp){
			case 'function':return (exp.id == id)? jsworld.cons(acc, exp) : jsworld.append(acc, jsworld.fold([exp.body], [], find)); break;
			case 'define':	return (exp.id == id)? jsworld.cons(acc, exp) : jsworld.append(acc, jsworld.fold([exp.value], [], find)); break;
			case 'apply':	return (exp.id == id)? jsworld.cons(acc, exp) : jsworld.append(acc, jsworld.fold(exp.inputs, [], find)); break;
			default: return 'ERROR - invalid expression type';		
		}
                return 'ERROR - invalid expression type';	
	}
	return jsworld.fold(world.expressions, [], find)[0];
}

function findById(array, id){
	return jsworld.fold(array, [], function(elt, acc){ return (elt.id == id)? jsworld.cons(acc,elt) : acc})[0];
}
/***************************************************************
 * DRAGGING STUFF
 ***************************************************************/
function dragging(world) {
	return (world.drag_info.dragging.length > 0);
}

function apply_drag(world) {
	var styleDrag = function (style) {
		return (world.drag_info.dragging.length > 0 && world.drag_info.dragging[0].id == style.id)?
				jsworld.augment(style, {left: style.left + world.drag_info.delta_x, top: style.top + world.drag_info.delta_y})
				: style;
	}
	return jsworld.augment(world, {styles: jsworld.map(world.styles, styleDrag)});
}

function stop_drag(world, ev) {
	// is a node being hovered over a given target?
	var hovered = function(target){
		var pos		= jsworld.getPosition(target);
		var xMin	= pos.x;
		var yMin	= pos.y;
		var xMax	= xMin+target.offsetWidth;
		var yMax	= yMin+target.offsetHeight;
		return (ev.clientX > xMin && ev.clientX < xMax && ev.clientY > yMin && ev.clientY < yMax);
	}

	var target	= jsworld.fold(world.targets, [], function(target, acc){ return hovered(target)? jsworld.cons(acc,target) : acc});

	if(dragging(world) && target.length > 0 ){
		var dragId	= world.drag_info.dragging[0].id;
		var dragExp	= findById(world.expressions, dragId);
	
		// recursively crawl down the tree, and replace a hovered target
		var updateExp = function(exp){
			if(exp.exp == undefined) return (exp.id == target[0].id)? dragExp : exp;
			switch(exp.exp){
				case 'define':		break;
				case 'function':	break;
				case 'apply':		return jsworld.augment(exp, {inputs: jsworld.map(exp.inputs, updateExp)}); break;
				default:			return 'ERROR - invalid expression type';		
			}
		        return 'ERROR - invalid expression type';
		}
		// return a world in which the dragExp has been removed as a top-level exp,
		// the styles for the target has been removed, the dragExp is no longer absolutely positioned,
		// the target itself has also been removed, and the dragging array is reset to null
		var newStyles	= jsworld.map(world.styles, function(s){return (s.id==dragId)? jsworld.augment(s, {position:'', opacity:''}) : s});
		return jsworld.augment(world, {	expressions:removef(jsworld.map(world.expressions, updateExp),function(exp){return exp.id == dragId}),
											styles:		removef(newStyles,	function(s){return s.id==target[0].id}),
											targets:	removef(world.targets,	function(t){return t.id == target[0].id}),
											drag_info:	jsworld.augment(world.drag_info, { dragging: [] })
											});

	} else {
		// return to full opacity
		var unStyleDrag = function (style) {
			return jsworld.augment(style, { opacity: "1.00", zIndex: "", background: ''});
		}
		return jsworld.augment(world, {	styles:		jsworld.map(world.styles, unStyleDrag),
								drag_info:	jsworld.augment(world.drag_info, { dragging: [] })
							});
	}
}

// 1) Style the exp being dragged
// 2) If the exp is NOT top-level, replace it with an empty target
function start_drag(node) {
	var styleDrag = function (style) {
		return (node.id == style.id)? jsworld.augment(style, { opacity: ".80", zIndex: "999"}) : style;
	}
	
	return function (world, ev) {
		var dragExp		= findExpById(node.id, world);
		var dragStyle	= findById(world.styles, node.id);
		var newStyles	= jsworld.map(world.styles, function(s){return (s.id==dragStyle.id)? jsworld.augment(s, {position: 'absolute',opacity: '.8', zIndex:'999'}) : s});
		var targetExp	= makeValue(null, null);
		var targetStyle	= jsworld.augment(dragStyle, {id: targetExp.id});
		
		
		// recursively crawl down the tree, and replace the dragged exp with the new target
		var updateExp = function(exp){
			if(exp.exp == undefined) return exp;
			switch(exp.exp){
				case 'define':		break;
				case 'function':	break;
				case 'apply':		return (exp.id==node.id)? targetExp : jsworld.augment(exp, {inputs: jsworld.map(exp.inputs, updateExp)}); break;
				default:			return 'ERROR - invalid expression type';		
			}
                        return 'ERROR - invalid expression type';
		}

		// if a matching ID is found in world.expressions, it's top level.
		var topLevel	= (findById(world.expressions, node.id) !== undefined);
		return jsworld.augment(world, {	expressions:topLevel? world.expressions : jsworld.cons(jsworld.map(world.expressions, updateExp),dragExp),
								styles:		topLevel? newStyles : jsworld.cons(newStyles, targetStyle),
								targets:	topLevel? world.targets : jsworld.cons(world.targets, targetExp),
								drag_info: {
									dragging: jsworld.cons(world.drag_info.dragging, node),
									init_x: ev.clientX, init_y: ev.clientY,
									delta_x: 0, delta_y: 0
								   }
							   });
	};
}

function continue_drag(world, ev) {
	//  highlight any target that is being hovered over
	var udpateStyle = function(style){
		if( style.id.search("val_") == -1 ) return style;
		else return hovered(findById(world.targets, style.id))? jsworld.augment(style, {background:'gray'}): jsworld.augment(style, {background:''});
	}
	// is the mouse hovering over a target?
	var hovered = function(target){
		var pos		= jsworld.getPosition(target);
		var xMin	= pos.x;
		var yMin	= pos.y;
		var xMax	= xMin+target.offsetWidth;
		var yMax	= yMin+target.offsetHeight;
		return ev.clientX > xMin && ev.clientX < xMax && ev.clientY > yMin && ev.clientY < yMax;
	}

	return dragging(world)? jsworld.augment(world, {
									styles:		jsworld.map(world.styles, udpateStyle),
									drag_info:	jsworld.augment(world.drag_info, {
													   delta_x: ev.clientX - world.drag_info.init_x,
													   delta_y: ev.clientY - world.drag_info.init_y
													   })
									})
	: world;
}

function complete_drag(world, ev) {
	world = continue_drag(world, ev);
	world = apply_drag(world);
	return typecheck(stop_drag(world, ev));
}


/***************************************************************
 * GUI STUFF
 ***************************************************************/
// addExp: op_name [Input List] [Constraint List] Output -> (World -> World)
// generate an id, populate with children of appropriate type/constraint, and add the tree to the world
function addExp(op_name, inputs, constraints, output){
	return function(world){
		var unique	= 'exp_'+jsworld.gensym();
		var params	= jsworld.array_join(inputs, constraints);
		var children= jsworld.map(params, function(param){return makeValue(param[0], param[1])});
		var c_styles= jsworld.map(children,function(child){ return {id: child.id}});
		var exp		= {exp: 'apply', fn: op_name, type: output, id: unique, inputs: children };
		var e_style	= {id: unique, position: 'absolute', left: 175, top: world.counter * 100};
		var styles	= jsworld.cons(c_styles, e_style);
		return jsworld.augment(world, {
					   counter:		world.counter + 1,
					   expressions:	jsworld.cons(world.expressions, exp),
					   styles:		jsworld.append(world.styles, styles),
					   targets:		jsworld.append(world.targets, children)
					   });
	}	
}

// addValue: World -> World
function addValue(world){
	var exp		= makeValue(null,null);
	var style	= {id: exp.id, position: 'absolute', left: 175, top: world.counter * 100};
	return jsworld.augment(world, {
				   counter:		world.counter + 1,
				   expressions:	jsworld.cons(world.expressions, exp),
				   styles:		jsworld.cons(world.styles, style),
				   targets:		jsworld.cons(world.targets, exp)
				   });
}

// makeValue: Type Constraint -> DOM
function makeValue(type, constraint){
	var unique	= 'val_'+jsworld.gensym();
	switch (constraint){
		case "number":	value = Math.floor(Math.random()*10) - 5; break;
		case "positive":value = Math.floor(Math.random()*10);  break;
		case "string":
			var vals = new Array("\"apple\"","\"banana\"","\"orange\"","\"rubber\"","\"asia\"","\"duck\"","\"space\"");
			value = vals[Math.floor(Math.random()*vals.length)]; break;
		case "boolean": value = (Math.random()>0.5)? "true":"false"; break;
		case "fill":	value = (Math.random()>0.5)? "\"solid\"":"\"outline\""; break;
		case "color":
			var vals = new Array("\"red\"","\"black\"","\"blue\"","\"green\"","\"purple\"","\"pink\"","\"yellow\"");
			value = vals[Math.floor(Math.random()*vals.length)]; break;
		default: value = Math.floor(Math.random()*100) - 50; break;
	}
	if(type == "image"){
		var vals = new Array('images/cat.png', 'images/dog.png', 'images/lambda.png', 'images/sundae.png');
		value = vals[Math.floor(Math.random()*vals.length)];
		var node = jsworld.img({src: value, mousedown: function(world){return false;}});
		return node;
	}
	return jsworld.textarea({ value: value, className: type, id: unique, cols: 6});
}

// makeEvaluator: String -> (World -> World)
function makeEvaluator(op_name){
	var functionInfo = getFunctionInfo(op_name);
	return addExp(op_name, functionInfo.inputs, functionInfo.constraints, functionInfo.output);
}

function getFunctionInfo(op_name){
	switch(op_name){
		case "+":			return {inputs:['number','number'], constraints: ['number','number'], output:'number'};
		case "-":			return {inputs:['number','number'], constraints: ['number','number'], output:'number'}
		case "/":			return {inputs:['number','number'], constraints: ['number','number'], output:'number'}
		case "*":			return {inputs:['number','number'], constraints: ['number','number'], output:'number'}
		case "sqrt":		return {inputs:['number'], constraints: ['number'], output:'number'}
		case "expt":		return {inputs:['number','number'], constraints: ['number','number'], output:'number'}
		case "string-length":return {inputs:['string'], constraints: ['string'], output:'number'}
		case "string-append":return {inputs:['string','string'], constraints: ['string','string'], output:'string'};
		case "image-width":	return {inputs:['image'], constraints: ['image'], output:'number'}
		case "image-height":return {inputs:['image'], constraints: ['image'], output:'number'}
		case "rotate":		return {inputs:['image'], constraints: ['image'], output:'image'}
		case "circle":		return {inputs:['number','string','string'], constraints: ['positive','fill','color'], output:'image'};
		case "rectangle":	return {inputs:['number','number','string','string'], constraints: ['positive','positive','fill','color'], output:'image'};
		case "place-image":	return {inputs:['image','number','number','image'], constraints: ['image','number','number','image'], output:'image'};
		case ">":			return {inputs:['number','number'], constraints: ['number','number'], output:'boolean'};
		case "<":			return {inputs:['number','number'], constraints: ['number','number'], output:'boolean'};
		case "=":			return {inputs:['number','number'], constraints: ['number','number'], output:'boolean'};
		case "or":			return {inputs:['boolean','boolean'], constraints: ['boolean','boolean'], output:'boolean'};
		case "and":			return {inputs:['boolean','boolean'], constraints: ['boolean','boolean'], output:'boolean'};
		case "not":			return {inputs:['boolean'], constraints: ['boolean'], output:'boolean'};
		case "string=?":	return {inputs:['string','string'], constraints: ['string','string'], output:'boolean'};
		default: alert('no match!');
	}
    throw new Error("getFunctionInfo for " + op_name);
}

// compute_event: World -> World
function compute_event(world) {
	var compute_exp	= function (exp){var v = evaluateExp(world, exp); alert(v); return {exp: 'value', id: exp.id, value: v, type: "image"}};
	var values		= jsworld.map(world.expressions, compute_exp);
	return jsworld.augment(world,  {expressions:	values});
}

// evaluate depeding on expression type
function evaluateExp(world, exp){	
	if(exp.exp == undefined) return exp.value;
	switch(exp.exp){
		case 'function':return exp;
		case 'value':	
			var regExp_Number	= /^(\-)?[0-9]+(\.[0-9]*)?$/;
			var regExp_String	= /^".*"$/;
			var regExp_Variable	= /^(?![0-9])\w+$/;
			if(typeof exp.value == 'string' && exp.value.search(regExp_Variable) == 0) return lookup(world.bindings(exp.value));
			else return exp.value;			
		case 'define':	return jsworld.augment(world,  {bindings: jsworld.cons(world.bindings, [exp.id, evaluateExp(exp.value)])});
		case 'apply':
			var inputs = exp.inputs;
			switch(exp.fn){
				case "+": return parseFloat(evaluateExp(world,inputs[0]))+parseFloat(evaluateExp(world, inputs[1]));
				case "-": return parseFloat(evaluateExp(world,inputs[0]))-parseFloat(evaluateExp(world, inputs[1]));
				case "*": return parseFloat(evaluateExp(world,inputs[0]))*parseFloat(evaluateExp(world,inputs[1]));
				case "/": return parseFloat(evaluateExp(world,inputs[0]))/parseFloat(evaluateExp(world, inputs[1]));
				case "expt":  return Math.pow(parseFloat(evaluateExp(world, inputs[0])),parseFloat(evaluateExp(world, inputs[1])));
				case "sqrt":  return Math.sqrt(parseFloat(evaluateExp(world,inputs[0])));
				case "string-length": return evaluateExp(world,inputs[0]).length-2;
				case "string-append": 
					var a = evaluateExp(world,inputs[0]);
					var b = evaluateExp(world,inputs[1]);
					return '"'+a.substring(1,a.length-1)+b.substring(1,b.length-1)+'"';
				case "image-width": return evaluateExp(world,inputs[0]).getAttribute('width');
				case "image-height":return evaluateExp(world,inputs[0]).getAttribute('height'); 
				case "rotate": 
					var canvas	= evaluateExp(world, inputs[0]);
					var ctx		= canvas.getContext('2d');
					ctx.save();
					//				ctx.clearRect(0,0,canvas.width,canvas.height);
					ctx.translate(canvas.width/2,canvas.height/2);
					ctx.rotate(evaluateExp(world,inputs[1]) * Math.PI / 180);
					ctx.drawImage(canvas,0,0);
					ctx.restore();
					return canvas;
				case "circle": 
					var radius	= evaluateExp(world, inputs[0]);
					var fill	= evaluateExp(world, inputs[1]);
					var color	= evaluateExp(world, inputs[2]);
					fill		= fill.substring(1,fill.length-1);
					color		= color.substring(1,color.length-1)
					var canvas	= document.createElement('canvas');
					canvas.setAttribute('width',radius*2);
					canvas.setAttribute('height',radius*2);
					var ctx = canvas.getContext('2d');
					ctx.beginPath();
					ctx.arc(radius, radius, radius, 0, Math.PI*2, true); 
					ctx.closePath();
					ctx.fillStyle = color;
					ctx.strokeStyle = color;
					if(fill =='solid') ctx.fill(); if(fill == 'outline') ctx.stroke();
					return canvas;
				case "rectangle": 
					var width	= evaluateExp(world, inputs[0]);
					var height	= evaluateExp(world, inputs[0]);
					var fill	= evaluateExp(world, inputs[1]);
					var color	= evaluateExp(world, inputs[2]);
					fill		= fill.substring(1,fill.length-1);
					color		= color.substring(1,color.length-1)
					var canvas	= document.createElement('canvas');
					canvas.setAttribute('width',width);
					canvas.setAttribute('height',height);
					var ctx = canvas.getContext('2d');
					ctx.fillStyle = color;
					ctx.strokeStyle = color;
					if(fill =='solid') ctx.fillRect(0,0,width,height); 
					else ctx.strokeRect(0,0,width,height);
					return canvas;
				case "place-image":
					var canvas1	= evaluateExp(world, inputs[0]);
					var xOffset = parseFloat(evaluateExp(world, inputs[2]));
					var yOffset = parseFloat(evaluateExp(world, inputs[3]));
					var	canvas2	= evaluateExp(world, inputs[3]);
					var new_canvas = document.createElement('canvas');
					new_canvas.height = canvas2.height;
					new_canvas.width = canvas2.width;
					var new_ctx = new_canvas.getContext('2d');
					new_ctx.save();
					new_ctx.drawImage(canvas2,0,0);
					new_ctx.drawImage(canvas1,xOffset,yOffset);
					new_ctx.save();
					return new_canvas;
				case "<":	return parseFloat(evaluateExp(world, inputs[0]))<parseFloat(evaluateExp(world, inputs[1]));
				case ">":	return parseFloat(evaluateExp(world, inputs[0]))>parseFloat(evaluateExp(world, inputs[1]));
				case "=":	return parseFloat(evaluateExp(world, inputs[0]))==parseFloat(evaluateExp(world, inputs[1]));
				case "string=?": return evaluateExp(world, inputs[0]) == evaluateExp(world, inputs[1]);
				case "and":	return evaluateExp(world, inputs[0]) && evaluateExp(world, inputs[1]);
				case "or":	return evaluateExp(world, inputs[0]) || evaluateExp(world, inputs[1]);
				case "not":	return ! evaluateExp(world, inputs[0]);
				case "if":	return (evaluateExp(world, inputs[0]))? evaluateExp(world, inputs[1]) : evaluateExp(world, inputs[2]);
				default: 
					var fn = evaluateExp(exp.fn);
					// create a closure, bind all the arg values to the variables, and evaluate the body
					var new_world =  jsworld.augment(world,  {bindings: jsworld.append(world.bindings, jsworld.array_join(fn.input_names, inputs))});
					var result = evaluateExp(new_world, fn.body);
					return result;
			}			
		default: return 'ERROR - invalid expression type';
	}
}

// typecheck a tree within a world, returning a list of styles
// typecheck : World -> World
function typecheck(world){
	var typecheckValue = function(val){
		var regExp_Number	= /^(\-)?[0-9]+(\.[0-9]*)?$/;
		var regExp_String	= /^".*"$/;
		var regExp_Variable	= /^(?![0-9])\w+$/;
		if(val.value == undefined)							var type = "image";
		else if(val.value == "")							var type = "error";
		else if(val.value.search(regExp_Number)   !== -1)	var type = "number";
//		else if(val.value.search(regExp_Variable) !== -1)	var type = "??";
		else if(val.value.search(regExp_String)   !== -1)	var type = "string";
		else if(val.value=="false" || val.value=="true")	var type = "boolean";
		val.className = type;
		return val;
	}
	
	// exp -> exp
	var typecheckExp = function(exp){
		if(exp.exp == undefined) return typecheckValue(exp);
		switch(exp.exp){
			case 'define':	return jsworld.augment(exp, {value: typecheckExp(exp.value)});
			case 'function':return jsworld.augment(exp, {inputs: jsworld.map(exp.inputs, typecheckExp), body: typecheckExp(exp.body)});
			case 'apply':
				var functionInfo= getFunctionInfo(exp.fn);
				var children	= jsworld.map(exp.inputs, typecheckExp);
				var childTypes	= jsworld.map(children, function (c){return (c.exp == undefined)? c.className : c.type});
				var mergedTypes	= jsworld.array_join(childTypes, functionInfo.inputs);
				var correctTypes= jsworld.fold(mergedTypes, true, function(types, acc){return (types[0]==types[1]) && acc});
				return jsworld.augment(exp, {type: (correctTypes? functionInfo.output : "error"), inputs: children});
			default: alert('ERROR - invalid expression type! exp.exp: '+exp.exp + ', exp: '+exp);
		}
	    throw new Error("typecheck (exp -> exp)");
	}

	return jsworld.augment(world, {expressions: jsworld.map(world.expressions, typecheckExp)});
}

// exp2tree: exp -> tree
function tree2sexp(exp){
	if(exp.exp == undefined) return exp;
	switch(exp.exp){
//		case 'function': break;
//		case 'define':	break;
		case 'value': 	return exp; break;
		case 'apply':
			var root	= jsworld.div({ className: "Expression "+exp.type, id: exp.id });
			var removeFromWorld	= function (world){
									return jsworld.augment(world, { expressions: removef(world.expressions, function(e){return e.id == exp.id}),
															styles:		removef(world.styles, function(s){return s[0] == exp.id})
												   });
									}
			var operator	= jsworld.div({ className: "operator", dblclick: removeFromWorld, mousedown: start_drag(root) });
			var op_text	= jsworld.text(exp.fn);
			var boxes	= jsworld.map(exp.inputs, function(input){return tree2sexp(input)});
			tree_node	= jsworld.append([root, [operator, [op_text]]], boxes);
			return tree_node;
			break;
		default: alert( 'ERROR - invalid expression type! exp.exp: '+exp.exp + ', exp: '+exp);
	}
    throw new Error("tree2sexp");
} 
 
// redraw: World -> DOM
function redraw(world) {
	var tools = [toolbar,
				 [heading, [heading_text]], 
				 [compute, [compute_text]],
				 [variable, [variable_text]],
				 [heading2, [heading2_text]]];
	return [document.body,
			jsworld.append(tools, all),
			[the_div].concat(jsworld.map(world.expressions, tree2sexp))];
}

// redraw_css: World -> CSS
function redraw_css(world) {
	return jsworld.map((dragging(world) ? apply_drag(world) : world).styles,
						function(style){
							 return [style.id, 
									 ["position",	style.position],
									 ["left",		style.left + "px"],
									 ["top",		style.top + "px"],
									 ["opacity",	style.opacity],
									 ["z-index",	style.zIndex],
									 ["background",	style.background]
									];
							 });
}

function tick(world) {
	return world;
}