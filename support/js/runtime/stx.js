
var stx_colon_atom = function (datum,loc) { 
    plt.types.Struct.call(this, "make-stx:atom", [datum,loc]);
    this.datum = datum;
    this.loc = loc; 
};
stx_colon_atom.prototype = new plt.types.Struct();

var make_dash_stx_colon_atom = function (id0,id1) {
    var atom = new stx_colon_atom(id0,id1);
    return atom;
};

var stx_colon_atom_dash_datum = function(obj) {
     if (stx_colon_atom_question_ (obj)) {
        return obj.datum;
     } else {
        throw new plt.Kernel.MobyRuntimeError(           
	    plt.Kernel.format('stx_colon_atom_dash_datum: not a stx:atom: ~s', [obj]));
     }
};

var stx_colon_atom_dash_loc = function(obj) {
     if (stx_colon_atom_question_ (obj)) {
        return obj.loc;
     } else {
        throw new plt.Kernel.MobyRuntimeError(
            plt.Kernel.format('stx_colon_atom_dash_loc: not a stx:atom: ~s', [obj]));
     }
};

var set_dash_stx_colon_atom_dash_datum_bang_ = function(obj,newVal) {
    if (stx_colon_atom_question_ (obj)) {
	     obj.datum = newVal;
     } else {
        throw new plt.Kernel.MobyRuntimeError(
            plt.Kernel.format('set_dash_stx_colon_atom_dash_datum_bang_: not a stx:atom: ~s', [obj]));
     }
};

var set_dash_stx_colon_atom_dash_loc_bang_ = function(obj,newVal) {
	 if (stx_colon_atom_question_ (obj)) {
		obj.loc = newVal;
     } else {
        throw new plt.Kernel.MobyRuntimeError(
            plt.Kernel.format('set_dash_stx_colon_atom_dash_loc_bang_: not a stx:atom: ~s', [obj]));
     }
};

var stx_colon_atom_question_ = function(obj) { 
    return (obj != null && obj != undefined &&
	    obj instanceof stx_colon_atom);
};

var stx_colon_list = function (elts,loc) { 
    plt.types.Struct.call(this, "make-stx:list", [elts,loc]);
    this.elts = elts;
    this.loc = loc; };
stx_colon_list.prototype = new plt.types.Struct();

var make_dash_stx_colon_list = function (id0,id1) { 
    return new stx_colon_list(id0,id1); };

var stx_colon_list_dash_elts = function(obj) {
    if (stx_colon_list_question_ (obj)) {
        return obj.elts;
    } else {
        throw new plt.Kernel.MobyRuntimeError(
            plt.Kernel.format('stx_colon_list_dash_elts: not a stx:list: ~s', [obj]));
    }
};

var stx_colon_list_dash_loc = function(obj) {
     if (stx_colon_list_question_ (obj)) {
        return obj.loc;
     } else {
        throw new plt.Kernel.MobyRuntimeError(
            plt.Kernel.format('stx_colon_list_dash_loc: not a stx:list: ~s', [obj]));
     }
};

var set_dash_stx_colon_list_dash_elts_bang_ = function(obj,newVal) {
	 if (stx_colon_list_question_ (obj)) {
		obj.elts = newVal;
     } else {
        throw new plt.Kernel.MobyRuntimeError(
            plt.Kernel.format('set_dash_stx_colon_list_dash_elts_bang_: not a stx:list: ~s', [obj]));
     }
};

var set_dash_stx_colon_list_dash_loc_bang_ = function(obj,newVal) {
	 if (stx_colon_list_question_ (obj)) {
		obj.loc = newVal;
     } else {
        throw new plt.Kernel.MobyRuntimeError(
            plt.Kernel.format('set_dash_stx_colon_list_dash_loc_bang_: not a stx:list: ~s', [obj]));
     }
};

var stx_colon_list_question_ = function(obj) { 
              return obj != null && obj != undefined && obj instanceof stx_colon_list; 
};

var Loc = function (offset,line,span,id) { 
    plt.types.Struct.call(this, "make-Loc", [offset,line,span,id]);this.offset = offset;
    this.line = line;
    this.span = span;
    this.id = id; };
Loc.prototype = new plt.types.Struct();

var make_dash_Loc = function (id0,id1,id2,id3) { 
    return new Loc(id0,id1,id2,id3); };
var Loc_dash_offset = function(obj) {
    if (Loc_question_ (obj)) {
        return obj.offset;
    } else {
        throw new plt.Kernel.MobyRuntimeError( 
            plt.Kernel.format('Loc_dash_offset: not a Loc: ~s', [obj]));
    }
};

var Loc_dash_line = function(obj) {
     if (Loc_question_ (obj)) {
        return obj.line;
     } else {
        throw new plt.Kernel.MobyRuntimeError(
            plt.Kernel.format('Loc_dash_line: not a Loc: ~s', [obj]));
     }
};

var Loc_dash_span = function(obj) {
     if (Loc_question_ (obj)) {
        return obj.span;
     } else {
        throw new plt.Kernel.MobyRuntimeError(
            plt.Kernel.format('Loc_dash_span: not a Loc: ~s', [obj]));
     }
};

var Loc_dash_id = function(obj) {
     if (Loc_question_ (obj)) {
        return obj.id;
     } else {
        throw new plt.Kernel.MobyRuntimeError( 
            plt.Kernel.format('Loc_dash_id: not a Loc: ~s', [obj]));
     }
};

var set_dash_Loc_dash_offset_bang_ = function(obj,newVal) {
	 if (Loc_question_ (obj)) {
		obj.offset = newVal;
     } else {
        throw new plt.Kernel.MobyRuntimeError(
            plt.Kernel.format('set_dash_Loc_dash_offset_bang_: not a Loc: ~s', [obj]));
     }
};

var set_dash_Loc_dash_line_bang_ = function(obj,newVal) {
	 if (Loc_question_ (obj)) {
		obj.line = newVal;
     } else {
        throw new plt.Kernel.MobyRuntimeError(
            plt.Kernel.format('set_dash_Loc_dash_line_bang_: not a Loc: ~s', [obj]));
     }
};

var set_dash_Loc_dash_span_bang_ = function(obj,newVal) {
	 if (Loc_question_ (obj)) {
		obj.span = newVal;
     } else {
        throw new plt.Kernel.MobyRuntimeError(
            plt.Kernel.format('set_dash_Loc_dash_span_bang_: not a Loc: ~s', [obj]));
     }
};

var set_dash_Loc_dash_id_bang_ = function(obj,newVal) {
	 if (Loc_question_ (obj)) {
		obj.id = newVal;
     } else {
        throw new plt.Kernel.MobyRuntimeError(
            plt.Kernel.format('set_dash_Loc_dash_id_bang_: not a Loc: ~s', [obj]));
     }
};

var Loc_question_ = function(obj) { 
              return obj != null && obj != undefined && obj instanceof Loc; };





var Loc_dash__greaterthan_string = function(a_dash_loc) { return (plt.Kernel.setLastLoc({offset:284, line:14, span:154, id:"stx.ss"}) && plt.Kernel.format((plt.types.String.makeInstance("offset=~a line=~a span=~a id=~s")), [(plt.Kernel.setLastLoc({offset:337, line:15, span:18, id:"stx.ss"})   && Loc_dash_offset(a_dash_loc)),(plt.Kernel.setLastLoc({offset:367, line:16, span:16, id:"stx.ss"})   && Loc_dash_line(a_dash_loc)),(plt.Kernel.setLastLoc({offset:395, line:17, span:16, id:"stx.ss"})   && Loc_dash_span(a_dash_loc)),(plt.Kernel.setLastLoc({offset:423, line:18, span:14, id:"stx.ss"})   && Loc_dash_id(a_dash_loc))])); };




var stx_question_ = function(x) { return ((plt.Kernel.setLastLoc({offset:489, line:23, span:13, id:"stx.ss"})   && stx_colon_atom_question_(x))||(plt.Kernel.setLastLoc({offset:509, line:24, span:13, id:"stx.ss"})   && stx_colon_list_question_(x))); };





var stx_dash_e = function(a_dash_stx) { return ((plt.Kernel.setLastLoc({offset:635, line:31, span:17, id:"stx.ss"})   && stx_colon_atom_question_(a_dash_stx)) ?
						(plt.Kernel.setLastLoc({offset:658, line:32, span:22, id:"stx.ss"})   && stx_colon_atom_dash_datum(a_dash_stx)) :
						((plt.Kernel.setLastLoc({offset:687, line:33, span:17, id:"stx.ss"})   && stx_colon_list_question_(a_dash_stx)) ?
						 (plt.Kernel.setLastLoc({offset:710, line:34, span:21, id:"stx.ss"})   && stx_colon_list_dash_elts(a_dash_stx)) :
						 (plt.Kernel.setLastLoc({offset:624, line:30, span:109, id:"stx.ss"})   && plt.Kernel.error((plt.types.Symbol.makeInstance("cond")),(plt.types.String.makeInstance("cond: fell out of cond around \"offset=624 line=30 span=109 id=\\\"stx.ss\\\"\"")))))); };





var stx_dash_loc = function(a_dash_stx) { return ((plt.Kernel.setLastLoc({offset:797, line:40, span:17, id:"stx.ss"})   && stx_colon_atom_question_(a_dash_stx)) ?
						  (plt.Kernel.setLastLoc({offset:820, line:41, span:20, id:"stx.ss"})   && stx_colon_atom_dash_loc(a_dash_stx)) :
						  ((plt.Kernel.setLastLoc({offset:847, line:42, span:17, id:"stx.ss"})   && stx_colon_list_question_(a_dash_stx)) ?
						   (plt.Kernel.setLastLoc({offset:870, line:43, span:20, id:"stx.ss"})   && stx_colon_list_dash_loc(a_dash_stx)) :
						   (plt.Kernel.setLastLoc({offset:786, line:39, span:106, id:"stx.ss"})   && plt.Kernel.error((plt.types.Symbol.makeInstance("cond")),(plt.types.String.makeInstance("cond: fell out of cond around \"offset=786 line=39 span=106 id=\\\"stx.ss\\\"\"")))))); };





var stx_dash_begins_dash_with_question_ = function(a_dash_stx, a_dash_sym) {
    return ((plt.Kernel.setLastLoc({offset:991, line:49, span:17, id:"stx.ss"})   && stx_colon_atom_question_(a_dash_stx)) ?
 plt.types.Logic.FALSE :
	    ((plt.Kernel.setLastLoc({offset:1023, line:51, span:17, id:"stx.ss"})   && stx_colon_list_question_(a_dash_stx)) ?
	     ((plt.Kernel.setLastLoc({offset:1051, line:52, span:36, id:"stx.ss"})   && plt.Kernel.not((plt.Kernel.setLastLoc({offset:1056, line:52, span:30, id:"stx.ss"})   && plt.Kernel.empty_question_((plt.Kernel.setLastLoc({offset:1064, line:52, span:21, id:"stx.ss"})   && stx_colon_list_dash_elts(a_dash_stx))))))&&(plt.Kernel.setLastLoc({offset:1098, line:53, span:47, id:"stx.ss"})   && plt.Kernel.symbol_question_((plt.Kernel.setLastLoc({offset:1107, line:53, span:37, id:"stx.ss"})   && stx_dash_e((plt.Kernel.setLastLoc({offset:1114, line:53, span:29, id:"stx.ss"})   && plt.Kernel.first((plt.Kernel.setLastLoc({offset:1121, line:53, span:21, id:"stx.ss"})   && stx_colon_list_dash_elts(a_dash_stx))))))))&&(plt.Kernel.setLastLoc({offset:1156, line:54, span:74, id:"stx.ss"})   && plt.Kernel.symbol_equal__question_((plt.Kernel.setLastLoc({offset:1166, line:54, span:37, id:"stx.ss"})   && stx_dash_e((plt.Kernel.setLastLoc({offset:1173, line:54, span:29, id:"stx.ss"})   && plt.Kernel.first((plt.Kernel.setLastLoc({offset:1180, line:54, span:21, id:"stx.ss"})   && stx_colon_list_dash_elts(a_dash_stx)))))),a_dash_sym))) :
	     (plt.Kernel.setLastLoc({offset:980, line:48, span:253, id:"stx.ss"})   && plt.Kernel.error((plt.types.Symbol.makeInstance("cond")),(plt.types.String.makeInstance("cond: fell out of cond around \"offset=980 line=48 span=253 id=\\\"stx.ss\\\"\"")))))); };





var datum_dash__greaterthan_stx = function(a_dash_datum, a_dash_loc) { return ((plt.Kernel.setLastLoc({offset:1461, line:65, span:14, id:"stx.ss"})   && stx_question_(a_dash_datum)) ?
 a_dash_datum :
									       (((plt.Kernel.setLastLoc({offset:1499, line:67, span:15, id:"stx.ss"})   && plt.Kernel.pair_question_(a_dash_datum))||(plt.Kernel.setLastLoc({offset:1515, line:67, span:16, id:"stx.ss"})   && plt.Kernel.empty_question_(a_dash_datum))) ?
										(plt.Kernel.setLastLoc({offset:1538, line:68, span:89, id:"stx.ss"})   && make_dash_stx_colon_list((plt.Kernel.setLastLoc({offset:1553, line:68, span:47, id:"stx.ss"}) && plt.Kernel.map(((function() {
										    plt.Kernel.setLastLoc({offset:1558, line:68, span:33, id:"stx.ss"});
   var result = (function(args4) {
var x = args4[0];
       return (plt.Kernel.setLastLoc({offset:1570, line:68, span:20, id:"stx.ss"})   && datum_dash__greaterthan_stx(x,a_dash_loc)); });
                      result.toWrittenString = function (cache) {
                          return '<function:lambda>';
                      };
                      result.toDisplayedString = result.toWrittenString;
                      return result;
                   })()), [a_dash_datum])),a_dash_loc)) :
 (plt.types.Logic.TRUE ?
  (plt.Kernel.setLastLoc({offset:1644, line:71, span:29, id:"stx.ss"})   && make_dash_stx_colon_atom(a_dash_datum,a_dash_loc)) :
  (plt.Kernel.setLastLoc({offset:1450, line:64, span:225, id:"stx.ss"})   && plt.Kernel.error((plt.types.Symbol.makeInstance("cond")),(plt.types.String.makeInstance("cond: fell out of cond around \"offset=1450 line=64 span=225 id=\\\"stx.ss\\\"\""))))))); };





var stx_dash__greaterthan_datum = function(a_dash_stx) { return ((plt.Kernel.setLastLoc({offset:1787, line:84, span:17, id:"stx.ss"})   && stx_colon_atom_question_(a_dash_stx)) ?
								 (plt.Kernel.setLastLoc({offset:1810, line:85, span:22, id:"stx.ss"})   && stx_colon_atom_dash_datum(a_dash_stx)) :
								 ((plt.Kernel.setLastLoc({offset:1839, line:86, span:17, id:"stx.ss"})   && stx_colon_list_question_(a_dash_stx)) ?
								  (plt.Kernel.setLastLoc({offset:1862, line:87, span:38, id:"stx.ss"}) && plt.Kernel.map((function() { var result = (function(args) {
                    return stx_dash__greaterthan_datum(args[0]);
                 }); result.toWrittenString = function(cache) {return '<function:stx->datum>'; }
                     result.toDisplayedString = function(cache) {return '<function:stx->datum>';}
																				       return result; })(), [(plt.Kernel.setLastLoc({offset:1878, line:87, span:21, id:"stx.ss"})   && stx_colon_list_dash_elts(a_dash_stx))])) :
								  (plt.Kernel.setLastLoc({offset:1776, line:83, span:126, id:"stx.ss"})   && plt.Kernel.error((plt.types.Symbol.makeInstance("cond")),(plt.types.String.makeInstance("cond: fell out of cond around \"offset=1776 line=83 span=126 id=\\\"stx.ss\\\"\"")))))); };
