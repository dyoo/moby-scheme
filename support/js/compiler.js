
function permission_colon_location() {  }
            permission_colon_location.prototype = new org.plt.Kernel.Struct();
permission_colon_location.prototype.isEqual = function(other) {
              if (other instanceof permission_colon_location) {
                return org.plt.types.Logic.TRUE;
              } else {
                return false;
              }
           } 
function make_dash_permission_colon_location() { return new permission_colon_location(); }

function permission_colon_location_question_(obj) { 
              return obj instanceof permission_colon_location ; 
            }
function permission_colon_sms() {  }
            permission_colon_sms.prototype = new org.plt.Kernel.Struct();
permission_colon_sms.prototype.isEqual = function(other) {
              if (other instanceof permission_colon_sms) {
                return org.plt.types.Logic.TRUE;
              } else {
                return false;
              }
           } 
function make_dash_permission_colon_sms() { return new permission_colon_sms(); }

function permission_colon_sms_question_(obj) { 
              return obj instanceof permission_colon_sms ; 
            }
function permission_colon_tilt() {  }
            permission_colon_tilt.prototype = new org.plt.Kernel.Struct();
permission_colon_tilt.prototype.isEqual = function(other) {
              if (other instanceof permission_colon_tilt) {
                return org.plt.types.Logic.TRUE;
              } else {
                return false;
              }
           } 
function make_dash_permission_colon_tilt() { return new permission_colon_tilt(); }

function permission_colon_tilt_question_(obj) { 
              return obj instanceof permission_colon_tilt ; 
            }
function permission_colon_internet() {  }
            permission_colon_internet.prototype = new org.plt.Kernel.Struct();
permission_colon_internet.prototype.isEqual = function(other) {
              if (other instanceof permission_colon_internet) {
                return org.plt.types.Logic.TRUE;
              } else {
                return false;
              }
           } 
function make_dash_permission_colon_internet() { return new permission_colon_internet(); }

function permission_colon_internet_question_(obj) { 
              return obj instanceof permission_colon_internet ; 
            }
function permission_question_(datum) { return (((permission_colon_location_question_(datum)))||((permission_colon_sms_question_(datum)))||((permission_colon_tilt_question_(datum)))||((permission_colon_internet_question_(datum)))); }
var PERMISSION_colon_LOCATION; 
var PERMISSION_colon_SMS; 
var PERMISSION_colon_TILT; 
var PERMISSION_colon_INTERNET; 
function permission_dash__greaterthan_android_dash_permissions(a_dash_permission) { return (((permission_colon_location_question_(a_dash_permission))) ? (org.plt.Kernel.list([])) : ((((permission_colon_sms_question_(a_dash_permission))) ? (org.plt.Kernel.list([])) : ((((permission_colon_tilt_question_(a_dash_permission))) ? (org.plt.Kernel.list([])) : ((((permission_colon_internet_question_(a_dash_permission))) ? (org.plt.Kernel.list([])) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))))))); }
function permission_dash__greaterthan_on_dash_start_dash_code(a_dash_permission) { return (((permission_colon_location_question_(a_dash_permission))) ? ((org.plt.types.String.makeInstance("org.plt.platform.Platform.getInstance().getLocationService().startService();\n      org.plt.platform.Platform.getInstance().getLocationService().addLocationChangeListener(listener);"))) : ((((permission_colon_sms_question_(a_dash_permission))) ? ((org.plt.types.String.makeInstance(""))) : ((((permission_colon_tilt_question_(a_dash_permission))) ? ((org.plt.types.String.makeInstance("org.plt.platform.Platform.getInstance().getTiltService().startService();\n      org.plt.platform.Platform.getInstance().getTiltService().addOrientationChangeListener(listener);\n      org.plt.platform.Platform.getInstance().getTiltService().addAccelerationChangeListener(listener);"))) : ((((permission_colon_internet_question_(a_dash_permission))) ? ((org.plt.types.String.makeInstance(""))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))))))); }
function permission_dash__greaterthan_on_dash_pause_dash_code(a_dash_permission) { return (((permission_colon_location_question_(a_dash_permission))) ? ((org.plt.types.String.makeInstance("org.plt.platform.Platform.getInstance().getLocationService().shutdownService();"))) : ((((permission_colon_sms_question_(a_dash_permission))) ? ((org.plt.types.String.makeInstance(""))) : ((((permission_colon_tilt_question_(a_dash_permission))) ? ((org.plt.types.String.makeInstance("org.plt.platform.Platform.getInstance().getTiltService().shutdownService();"))) : ((((permission_colon_internet_question_(a_dash_permission))) ? ((org.plt.types.String.makeInstance(""))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))))))); }
function permission_dash__greaterthan_on_dash_destroy_dash_code(a_dash_permission) { return (((permission_colon_location_question_(a_dash_permission))) ? ((org.plt.types.String.makeInstance("org.plt.platform.Platform.getInstance().getLocationService().shutdownService();"))) : ((((permission_colon_sms_question_(a_dash_permission))) ? ((org.plt.types.String.makeInstance(""))) : ((((permission_colon_tilt_question_(a_dash_permission))) ? ((org.plt.types.String.makeInstance("org.plt.platform.Platform.getInstance().getTiltService().shutdownService();"))) : ((((permission_colon_internet_question_(a_dash_permission))) ? ((org.plt.types.String.makeInstance(""))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))))))); }
function env(bindings) { this.bindings = bindings; }
            env.prototype = new org.plt.Kernel.Struct();
env.prototype.isEqual = function(other) {
              if (other instanceof env) {
                return (((org.plt.Kernel.equal_question_((env_dash_bindings(this)),(env_dash_bindings(other)))))&&(org.plt.types.Logic.TRUE));
              } else {
                return false;
              }
           } 
function make_dash_env(id0) { return new env(id0); }
function env_dash_bindings(obj) { return obj.bindings; }
function env_question_(obj) { 
              return obj instanceof env ; 
            }
var empty_dash_env; 
function binding_question_(datum) { return (((binding_colon_constant_question_(datum)))||((binding_colon_function_question_(datum)))); }
function binding_colon_constant(name,java_dash_string,permissions) { this.name = name;
this.java_dash_string = java_dash_string;
this.permissions = permissions; }
            binding_colon_constant.prototype = new org.plt.Kernel.Struct();
binding_colon_constant.prototype.isEqual = function(other) {
              if (other instanceof binding_colon_constant) {
                return (((org.plt.Kernel.equal_question_((binding_colon_constant_dash_permissions(this)),(binding_colon_constant_dash_permissions(other)))))&&((((org.plt.Kernel.equal_question_((binding_colon_constant_dash_java_dash_string(this)),(binding_colon_constant_dash_java_dash_string(other)))))&&((((org.plt.Kernel.equal_question_((binding_colon_constant_dash_name(this)),(binding_colon_constant_dash_name(other)))))&&(org.plt.types.Logic.TRUE))))));
              } else {
                return false;
              }
           } 
function make_dash_binding_colon_constant(id0,id1,id2) { return new binding_colon_constant(id0,id1,id2); }
function binding_colon_constant_dash_name(obj) { return obj.name; }
function binding_colon_constant_dash_java_dash_string(obj) { return obj.java_dash_string; }
function binding_colon_constant_dash_permissions(obj) { return obj.permissions; }
function binding_colon_constant_question_(obj) { 
              return obj instanceof binding_colon_constant ; 
            }
function binding_colon_function(name,module_dash_path,min_dash_arity,var_dash_arity_question_,java_dash_string,permissions,cps_question_) { this.name = name;
this.module_dash_path = module_dash_path;
this.min_dash_arity = min_dash_arity;
this.var_dash_arity_question_ = var_dash_arity_question_;
this.java_dash_string = java_dash_string;
this.permissions = permissions;
this.cps_question_ = cps_question_; }
            binding_colon_function.prototype = new org.plt.Kernel.Struct();
binding_colon_function.prototype.isEqual = function(other) {
              if (other instanceof binding_colon_function) {
                return (((org.plt.Kernel.equal_question_((binding_colon_function_dash_cps_question_(this)),(binding_colon_function_dash_cps_question_(other)))))&&((((org.plt.Kernel.equal_question_((binding_colon_function_dash_permissions(this)),(binding_colon_function_dash_permissions(other)))))&&((((org.plt.Kernel.equal_question_((binding_colon_function_dash_java_dash_string(this)),(binding_colon_function_dash_java_dash_string(other)))))&&((((org.plt.Kernel.equal_question_((binding_colon_function_dash_var_dash_arity_question_(this)),(binding_colon_function_dash_var_dash_arity_question_(other)))))&&((((org.plt.Kernel.equal_question_((binding_colon_function_dash_min_dash_arity(this)),(binding_colon_function_dash_min_dash_arity(other)))))&&((((org.plt.Kernel.equal_question_((binding_colon_function_dash_module_dash_path(this)),(binding_colon_function_dash_module_dash_path(other)))))&&((((org.plt.Kernel.equal_question_((binding_colon_function_dash_name(this)),(binding_colon_function_dash_name(other)))))&&(org.plt.types.Logic.TRUE))))))))))))));
              } else {
                return false;
              }
           } 
function make_dash_binding_colon_function(id0,id1,id2,id3,id4,id5,id6) { return new binding_colon_function(id0,id1,id2,id3,id4,id5,id6); }
function binding_colon_function_dash_name(obj) { return obj.name; }
function binding_colon_function_dash_module_dash_path(obj) { return obj.module_dash_path; }
function binding_colon_function_dash_min_dash_arity(obj) { return obj.min_dash_arity; }
function binding_colon_function_dash_var_dash_arity_question_(obj) { return obj.var_dash_arity_question_; }
function binding_colon_function_dash_java_dash_string(obj) { return obj.java_dash_string; }
function binding_colon_function_dash_permissions(obj) { return obj.permissions; }
function binding_colon_function_dash_cps_question_(obj) { return obj.cps_question_; }
function binding_colon_function_question_(obj) { 
              return obj instanceof binding_colon_function ; 
            }
function binding_dash_id(a_dash_binding) { return (((binding_colon_constant_question_(a_dash_binding))) ? ((binding_colon_constant_dash_name(a_dash_binding))) : ((((binding_colon_function_question_(a_dash_binding))) ? ((binding_colon_function_dash_name(a_dash_binding))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))); }
function env_dash_extend(an_dash_env, new_dash_binding) { return (make_dash_env((org.plt.Kernel._kernelHashSet((env_dash_bindings(an_dash_env)),(binding_dash_id(new_dash_binding)),new_dash_binding)))); }
function env_dash_lookup(an_dash_env, name) { return (org.plt.Kernel._kernelHashRef((env_dash_bindings(an_dash_env)),name,org.plt.types.Logic.FALSE)); }
function env_dash_contains_question_(an_dash_env, name) { return (binding_question_((org.plt.Kernel._kernelHashRef((env_dash_bindings(an_dash_env)),name,org.plt.types.Logic.FALSE)))); }
function env_dash_keys(an_dash_env) { return (org.plt.Kernel._kernelHashMap((env_dash_bindings(an_dash_env)),(function(args) { var k = args[0];
var v = args[1];
                             return k; }))); }
function env_dash_extend_dash_constant(an_dash_env, id, java_dash_string) { return (env_dash_extend(an_dash_env,(make_dash_binding_colon_constant(id,java_dash_string,org.plt.types.Empty.EMPTY)))); }
function env_dash_extend_dash_function(an_dash_env, id, module_dash_path, min_dash_arity, var_dash_arity_question_, java_dash_string) { return (env_dash_extend(an_dash_env,(make_dash_binding_colon_function(id,module_dash_path,min_dash_arity,var_dash_arity_question_,java_dash_string,org.plt.types.Empty.EMPTY,org.plt.types.Logic.FALSE)))); }
function list_question_(datum) { return (((org.plt.Kernel.empty_question_(datum)))||((((org.plt.Kernel.pair_question_(datum)))&&((list_question_((org.plt.Kernel.rest(datum)))))))); }
function program_question_(datum) { return (((list_question_(datum)))&&((org.plt.Kernel.andmap((function(args) { var x = args[0];
                             return (((defn_question_(x)))||((expression_question_(x)))||((test_dash_case_question_(x)))||((library_dash_require_question_(x)))); }),datum)))); }
function expression_question_(an_dash_expr) { return (((org.plt.Kernel.not((defn_question_(an_dash_expr)))))&&((org.plt.Kernel.not((test_dash_case_question_(an_dash_expr)))))&&((org.plt.Kernel.not((library_dash_require_question_(an_dash_expr)))))); }
function defn_question_(an_dash_sexp) { return (((list_dash_begins_dash_with_question_(an_dash_sexp,(org.plt.types.Symbol.makeInstance("define"))))) ? (org.plt.types.Logic.TRUE) : ((((list_dash_begins_dash_with_question_(an_dash_sexp,(org.plt.types.Symbol.makeInstance("define-struct"))))) ? (org.plt.types.Logic.TRUE) : (((org.plt.types.Logic.TRUE) ? (org.plt.types.Logic.FALSE) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))))); }
function string_dash_join(strs, delim) { return (((org.plt.Kernel.empty_question_(strs))) ? ((org.plt.types.String.makeInstance(""))) : ((((org.plt.Kernel.empty_question_((org.plt.Kernel.rest(strs))))) ? ((org.plt.Kernel.first(strs))) : (((org.plt.types.Logic.TRUE) ? (org.plt.Kernel.string_dash_append((org.plt.Kernel.first(strs)), [(string_dash_join((org.plt.Kernel.rest(strs)),delim))])) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))))); }
function list_dash_begins_dash_with_question_(an_dash_sexp, a_dash_label) { return (((list_question_(an_dash_sexp)))&&((org.plt.Kernel.not((org.plt.Kernel.empty_question_(an_dash_sexp)))))&&((org.plt.Kernel.symbol_question_((org.plt.Kernel.first(an_dash_sexp)))))&&((org.plt.Kernel.symbol_equal__question_((org.plt.Kernel.first(an_dash_sexp)),a_dash_label)))); }
function test_dash_case_question_(an_dash_sexp) { return (((list_dash_begins_dash_with_question_(an_dash_sexp,(org.plt.types.Symbol.makeInstance("check-expect")))))||((list_dash_begins_dash_with_question_(an_dash_sexp,(org.plt.types.Symbol.makeInstance("check-within")))))||((list_dash_begins_dash_with_question_(an_dash_sexp,(org.plt.types.Symbol.makeInstance("check-error")))))); }
function library_dash_require_question_(an_dash_sexp) { return (list_dash_begins_dash_with_question_(an_dash_sexp,(org.plt.types.Symbol.makeInstance("require")))); }
function identifier_dash__greaterthan_munged_dash_java_dash_identifier(an_dash_id) { return (function() {
               
function member_question_(an_dash_id, elts) { return (((org.plt.Kernel.empty_question_(elts))) ? (org.plt.types.Logic.FALSE) : ((((org.plt.Kernel.equal_question_((org.plt.Kernel.first(elts)),an_dash_id))) ? (org.plt.types.Logic.TRUE) : (((org.plt.types.Logic.TRUE) ? ((member_question_(an_dash_id,(org.plt.Kernel.rest(elts))))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))))); }
var java_dash_identifiers; 
function trans(ch) { return (((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance("-"))))) ? ((org.plt.types.String.makeInstance("_dash_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance("_"))))) ? ((org.plt.types.String.makeInstance("_underline_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance("?"))))) ? ((org.plt.types.String.makeInstance("_question_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance("!"))))) ? ((org.plt.types.String.makeInstance("_bang_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance("."))))) ? ((org.plt.types.String.makeInstance("_dot_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance(":"))))) ? ((org.plt.types.String.makeInstance("_colon_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance("="))))) ? ((org.plt.types.String.makeInstance("_equal_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance("#"))))) ? ((org.plt.types.String.makeInstance("_pound_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance("$"))))) ? ((org.plt.types.String.makeInstance("_dollar_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance("%"))))) ? ((org.plt.types.String.makeInstance("_percent_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance("^"))))) ? ((org.plt.types.String.makeInstance("_tilde_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance("&"))))) ? ((org.plt.types.String.makeInstance("_and_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance("*"))))) ? ((org.plt.types.String.makeInstance("_star_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance("+"))))) ? ((org.plt.types.String.makeInstance("_plus_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance("*"))))) ? ((org.plt.types.String.makeInstance("_star_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance("/"))))) ? ((org.plt.types.String.makeInstance("_slash_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance("<"))))) ? ((org.plt.types.String.makeInstance("_lessthan_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance(">"))))) ? ((org.plt.types.String.makeInstance("_greaterthan_"))) : ((((org.plt.Kernel.char_equal__question_(ch,(org.plt.types.Character.makeInstance("~"))))) ? ((org.plt.types.String.makeInstance("_tilde_"))) : (((org.plt.types.Logic.TRUE) ? (org.plt.Kernel.string(ch, [ch])) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))))))))))))))))))))))))))))))))))))))); }

               return java_dash_identifiers = (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("abstract")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("continue")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("for")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("new")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("switch")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("assert")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("default")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("goto")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("package")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("synchronized")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("boolean")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("do")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("if")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("private")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("break")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("double")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("implements")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("protected")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("throw")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("byte")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("else")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("import")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("public")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("throws")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("case")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("enum")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("instanceof")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("return")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("transient")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("catch")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("extends")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("int")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("short")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("try")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("final")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("interface")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("static")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("void")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("class")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("finally")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("long")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("strictfp")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("volatile")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("const")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("float")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("native")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("super")), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("while")), org.plt.types.Empty.EMPTY))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));

org.plt.Kernel.identity((((member_question_(an_dash_id,java_dash_identifiers))) ? ((org.plt.Kernel.string_dash__greaterthan_symbol(org.plt.Kernel.format((org.plt.types.String.makeInstance("_nonclashing_~a")), [an_dash_id])))) : (((org.plt.types.Logic.TRUE) ? ((function() {
               
var chars; 
var translated_dash_chunks; 
var translated_dash_id; 

               return chars = (org.plt.Kernel.string_dash__greaterthan_list((org.plt.Kernel.symbol_dash__greaterthan_string(an_dash_id))));
translated_dash_chunks = org.plt.Kernel.map((function(args) {
                    return trans(args[0]);
                 }), [chars]);
translated_dash_id = (org.plt.Kernel.string_dash__greaterthan_symbol((string_dash_join(translated_dash_chunks,(org.plt.types.String.makeInstance(""))))));
org.plt.Kernel.identity(translated_dash_id);
              })()) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))));
              })(); }
function desugar_dash_cond(an_dash_expr) { return (function() {
               
function loop(questions, answers, question_dash_last, answer_dash_last) { return (((org.plt.Kernel.empty_question_(questions))) ? (org.plt.Kernel.list([])) : (((org.plt.types.Logic.TRUE) ? (org.plt.Kernel.list([])) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))); }
function process_dash_clauses(clauses, questions_slash_rev, answers_slash_rev) { return (((list_dash_begins_dash_with_question_((org.plt.Kernel.first(clauses)),(org.plt.types.Symbol.makeInstance("else"))))) ? ((loop((org.plt.Kernel.reverse(questions_slash_rev)),(org.plt.Kernel.reverse(answers_slash_rev)),(org.plt.types.Symbol.makeInstance("true")),(org.plt.Kernel.second((org.plt.Kernel.first(clauses))))))) : ((((org.plt.Kernel.empty_question_((org.plt.Kernel.rest(clauses))))) ? ((loop((org.plt.Kernel.reverse(questions_slash_rev)),(org.plt.Kernel.reverse(answers_slash_rev)),(org.plt.Kernel.first((org.plt.Kernel.first(clauses)))),(org.plt.Kernel.second((org.plt.Kernel.first(clauses))))))) : (((org.plt.types.Logic.TRUE) ? ((process_dash_clauses((org.plt.Kernel.rest(clauses)),(org.plt.Kernel.cons((org.plt.Kernel.first((org.plt.Kernel.first(clauses)))),questions_slash_rev)),(org.plt.Kernel.cons((org.plt.Kernel.second((org.plt.Kernel.first(clauses)))),answers_slash_rev))))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))))); }

               return org.plt.Kernel.identity((((list_dash_begins_dash_with_question_(an_dash_expr,(org.plt.types.Symbol.makeInstance("cond"))))) ? ((process_dash_clauses((org.plt.Kernel.rest(an_dash_expr)),org.plt.types.Empty.EMPTY,org.plt.types.Empty.EMPTY))) : (((org.plt.types.Logic.TRUE) ? ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("desugar-cond")),org.plt.Kernel.format((org.plt.types.String.makeInstance("Not a cond clause: ~s")), [an_dash_expr])))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))));
              })(); }
function remove_dash_leading_dash_whitespace(a_dash_str) { return (function() {
               
function remove_dash_leading_dash_whitespace_slash_list(chars) { return (((org.plt.Kernel.empty_question_(chars))) ? (org.plt.types.Empty.EMPTY) : ((((org.plt.Kernel.char_dash_whitespace_question_((org.plt.Kernel.first(chars))))) ? ((remove_dash_leading_dash_whitespace_slash_list((org.plt.Kernel.rest(chars))))) : (((org.plt.types.Logic.TRUE) ? ((org.plt.Kernel.list_dash__greaterthan_string(chars))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))))); }

               return org.plt.Kernel.identity((remove_dash_leading_dash_whitespace_slash_list((org.plt.Kernel.string_dash__greaterthan_list(a_dash_str)))));
              })(); }
function take(a_dash_list, n) { return ((org.plt.Kernel._equal_(n,(org.plt.types.Rational.makeInstance(0, 1)), [n,(org.plt.types.Rational.makeInstance(0, 1))])) ? (org.plt.types.Empty.EMPTY) : (((org.plt.types.Logic.TRUE) ? ((org.plt.Kernel.cons((org.plt.Kernel.first(a_dash_list)),(take((org.plt.Kernel.rest(a_dash_list)),(org.plt.Kernel.sub1(n))))))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))); }
function list_dash_tail(a_dash_list, n) { return (org.plt.Kernel.reverse((take((org.plt.Kernel.reverse(a_dash_list)),n)))); }
function range(n) { return ((org.plt.Kernel._equal_(n,(org.plt.types.Rational.makeInstance(0, 1)), [n,(org.plt.types.Rational.makeInstance(0, 1))])) ? (org.plt.types.Empty.EMPTY) : (((org.plt.types.Logic.TRUE) ? (org.plt.Kernel.append((range((org.plt.Kernel.sub1(n)))), [org.plt.Kernel.list([])])) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))); }
function case_dash_analyze_dash_definition(a_dash_definition, f_dash_function, f_dash_regular_dash_definition, f_dash_define_dash_struct) { return (((((list_dash_begins_dash_with_question_(a_dash_definition,(org.plt.types.Symbol.makeInstance("define")))))&&(org.plt.Kernel._equal_((org.plt.Kernel.length(a_dash_definition)),(org.plt.types.Rational.makeInstance(3, 1)), [(org.plt.Kernel.length(a_dash_definition)),(org.plt.types.Rational.makeInstance(3, 1))]))&&((org.plt.Kernel.pair_question_((org.plt.Kernel.second(a_dash_definition))))))) ? ((function() {
               
var id; 
var args; 
var body; 

               return id = (org.plt.Kernel.first((org.plt.Kernel.second(a_dash_definition))));
args = (org.plt.Kernel.rest((org.plt.Kernel.second(a_dash_definition))));
body = (org.plt.Kernel.third(a_dash_definition));
org.plt.Kernel.identity(((f_dash_function).apply(null, [id, args, body])));
              })()) : ((((((list_dash_begins_dash_with_question_(a_dash_definition,(org.plt.types.Symbol.makeInstance("define")))))&&(org.plt.Kernel._equal_((org.plt.Kernel.length(a_dash_definition)),(org.plt.types.Rational.makeInstance(3, 1)), [(org.plt.Kernel.length(a_dash_definition)),(org.plt.types.Rational.makeInstance(3, 1))]))&&((org.plt.Kernel.symbol_question_((org.plt.Kernel.second(a_dash_definition)))))&&((list_dash_begins_dash_with_question_((org.plt.Kernel.third(a_dash_definition)),(org.plt.types.Symbol.makeInstance("lambda"))))))) ? ((function() {
               
var id; 
var args; 
var body; 

               return id = (org.plt.Kernel.second(a_dash_definition));
args = (org.plt.Kernel.second((org.plt.Kernel.third(a_dash_definition))));
body = (org.plt.Kernel.third((org.plt.Kernel.third(a_dash_definition))));
org.plt.Kernel.identity(((f_dash_function).apply(null, [id, args, body])));
              })()) : ((((((list_dash_begins_dash_with_question_(a_dash_definition,(org.plt.types.Symbol.makeInstance("define")))))&&(org.plt.Kernel._equal_((org.plt.Kernel.length(a_dash_definition)),(org.plt.types.Rational.makeInstance(3, 1)), [(org.plt.Kernel.length(a_dash_definition)),(org.plt.types.Rational.makeInstance(3, 1))]))&&((org.plt.Kernel.symbol_question_((org.plt.Kernel.second(a_dash_definition)))))&&((org.plt.Kernel.not((list_dash_begins_dash_with_question_((org.plt.Kernel.third(a_dash_definition)),(org.plt.types.Symbol.makeInstance("lambda"))))))))) ? ((function() {
               
var id; 
var body; 

               return id = (org.plt.Kernel.second(a_dash_definition));
body = (org.plt.Kernel.third(a_dash_definition));
org.plt.Kernel.identity(((f_dash_regular_dash_definition).apply(null, [id, body])));
              })()) : ((((list_dash_begins_dash_with_question_(a_dash_definition,(org.plt.types.Symbol.makeInstance("define-struct"))))) ? ((function() {
               
var id; 
var fields; 

               return id = (org.plt.Kernel.second(a_dash_definition));
fields = (org.plt.Kernel.third(a_dash_definition));
org.plt.Kernel.identity(((f_dash_define_dash_struct).apply(null, [id, fields])));
              })()) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))))))); }
function path_equal__question_(path_dash_1, path_dash_2) { return org.plt.Kernel.string_equal__question_((org.plt.Kernel._pathToString((org.plt.Kernel._normalizePath(path_dash_1)))),(org.plt.Kernel._pathToString((org.plt.Kernel._normalizePath(path_dash_2)))), [(org.plt.Kernel._pathToString((org.plt.Kernel._normalizePath(path_dash_1)))),(org.plt.Kernel._pathToString((org.plt.Kernel._normalizePath(path_dash_2))))]); }
var toplevel_dash_env; 
function pinfo(env,modules,used_dash_bindings) { this.env = env;
this.modules = modules;
this.used_dash_bindings = used_dash_bindings; }
            pinfo.prototype = new org.plt.Kernel.Struct();
pinfo.prototype.isEqual = function(other) {
              if (other instanceof pinfo) {
                return (((org.plt.Kernel.equal_question_((pinfo_dash_used_dash_bindings(this)),(pinfo_dash_used_dash_bindings(other)))))&&((((org.plt.Kernel.equal_question_((pinfo_dash_modules(this)),(pinfo_dash_modules(other)))))&&((((org.plt.Kernel.equal_question_((pinfo_dash_env(this)),(pinfo_dash_env(other)))))&&(org.plt.types.Logic.TRUE))))));
              } else {
                return false;
              }
           } 
function make_dash_pinfo(id0,id1,id2) { return new pinfo(id0,id1,id2); }
function pinfo_dash_env(obj) { return obj.env; }
function pinfo_dash_modules(obj) { return obj.modules; }
function pinfo_dash_used_dash_bindings(obj) { return obj.used_dash_bindings; }
function pinfo_question_(obj) { 
              return obj instanceof pinfo ; 
            }
var empty_dash_pinfo; 
function get_dash_base_dash_pinfo(_underline_) { return (make_dash_pinfo(toplevel_dash_env,org.plt.types.Empty.EMPTY,(org.plt.Kernel._kernelMakeImmutableHash(org.plt.types.Empty.EMPTY)))); }
function pinfo_dash_update_dash_env(a_dash_pinfo, an_dash_env) { return (make_dash_pinfo(an_dash_env,(pinfo_dash_modules(a_dash_pinfo)),(pinfo_dash_used_dash_bindings(a_dash_pinfo)))); }
function pinfo_dash_accumulate_dash_binding(a_dash_binding, a_dash_pinfo) { return (make_dash_pinfo((env_dash_extend((pinfo_dash_env(a_dash_pinfo)),a_dash_binding)),(pinfo_dash_modules(a_dash_pinfo)),(pinfo_dash_used_dash_bindings(a_dash_pinfo)))); }
function pinfo_dash_accumulate_dash_bindings(bindings, a_dash_pinfo) { return (org.plt.Kernel.foldl((function(args) {
                    return pinfo_dash_accumulate_dash_binding(args[0], args[1]);
                 }),a_dash_pinfo,bindings)); }
function pinfo_dash_accumulate_dash_module(a_dash_module, a_dash_pinfo) { return (make_dash_pinfo((pinfo_dash_env(a_dash_pinfo)),(org.plt.Kernel.cons(a_dash_module,(pinfo_dash_modules(a_dash_pinfo)))),(pinfo_dash_used_dash_bindings(a_dash_pinfo)))); }
function pinfo_dash_accumulate_dash_binding_dash_use(a_dash_binding, a_dash_pinfo) { return (make_dash_pinfo((pinfo_dash_env(a_dash_pinfo)),(pinfo_dash_modules(a_dash_pinfo)),(org.plt.Kernel._kernelHashSet((pinfo_dash_used_dash_bindings(a_dash_pinfo)),a_dash_binding,org.plt.types.Logic.TRUE)))); }
function program_dash_analyze(a_dash_program) { return (program_dash_analyze_slash_pinfo(a_dash_program,(get_dash_base_dash_pinfo((org.plt.types.Symbol.makeInstance("_")))))); }
function program_dash_analyze_slash_pinfo(a_dash_program, pinfo) { return (function() {
               
var pinfo_dash_1; 

               return pinfo_dash_1 = (program_dash_analyze_dash_collect_dash_definitions(a_dash_program,pinfo));
org.plt.Kernel.identity((program_dash_analyze_dash_uses(a_dash_program,pinfo_dash_1)));
              })(); }
function program_dash_analyze_dash_collect_dash_definitions(a_dash_program, pinfo) { return (((org.plt.Kernel.empty_question_(a_dash_program))) ? (pinfo) : (((org.plt.types.Logic.TRUE) ? ((function() {
               
var updated_dash_pinfo; 

               return updated_dash_pinfo = (((defn_question_((org.plt.Kernel.first(a_dash_program))))) ? ((definition_dash_analyze_dash_collect_dash_definitions((org.plt.Kernel.first(a_dash_program)),pinfo))) : ((((test_dash_case_question_((org.plt.Kernel.first(a_dash_program))))) ? (pinfo) : ((((library_dash_require_question_((org.plt.Kernel.first(a_dash_program))))) ? ((require_dash_analyze((org.plt.Kernel.second((org.plt.Kernel.first(a_dash_program)))),pinfo))) : ((((expression_question_((org.plt.Kernel.first(a_dash_program))))) ? (pinfo) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond"))))))))))));
org.plt.Kernel.identity((program_dash_analyze_dash_collect_dash_definitions((org.plt.Kernel.rest(a_dash_program)),updated_dash_pinfo)));
              })()) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))); }
function program_dash_analyze_dash_uses(a_dash_program, pinfo) { return (((org.plt.Kernel.empty_question_(a_dash_program))) ? (pinfo) : (((org.plt.types.Logic.TRUE) ? ((function() {
               
var updated_dash_pinfo; 

               return updated_dash_pinfo = (((defn_question_((org.plt.Kernel.first(a_dash_program))))) ? ((definition_dash_analyze_dash_uses((org.plt.Kernel.first(a_dash_program)),pinfo))) : ((((test_dash_case_question_((org.plt.Kernel.first(a_dash_program))))) ? (pinfo) : ((((library_dash_require_question_((org.plt.Kernel.first(a_dash_program))))) ? (pinfo) : ((((expression_question_((org.plt.Kernel.first(a_dash_program))))) ? ((expression_dash_analyze_dash_uses((org.plt.Kernel.first(a_dash_program)),pinfo,(pinfo_dash_env(pinfo))))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond"))))))))))));
org.plt.Kernel.identity((program_dash_analyze_dash_uses((org.plt.Kernel.rest(a_dash_program)),updated_dash_pinfo)));
              })()) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))); }
function bf(name, module_dash_path, arity, vararity_question_, java_dash_string) { return (make_dash_binding_colon_function(name,module_dash_path,arity,vararity_question_,java_dash_string,org.plt.types.Empty.EMPTY,org.plt.types.Logic.FALSE)); }
function definition_dash_analyze_dash_collect_dash_definitions(a_dash_definition, pinfo) { return (case_dash_analyze_dash_definition(a_dash_definition,(function(args) { var id = args[0];
var args = args[1];
var body = args[2];
                             return (pinfo_dash_accumulate_dash_binding((bf(id,org.plt.types.Logic.FALSE,(org.plt.Kernel.length(args)),org.plt.types.Logic.FALSE,(org.plt.Kernel.symbol_dash__greaterthan_string((identifier_dash__greaterthan_munged_dash_java_dash_identifier(id)))))),pinfo)); }),(function(args) { var id = args[0];
var expr = args[1];
                             return (pinfo_dash_accumulate_dash_binding((make_dash_binding_colon_constant(id,(org.plt.Kernel.symbol_dash__greaterthan_string((identifier_dash__greaterthan_munged_dash_java_dash_identifier(id)))),org.plt.types.Empty.EMPTY)),pinfo)); }),(function(args) { var id = args[0];
var fields = args[1];
                             return (pinfo_dash_update_dash_env(pinfo,(extend_dash_env_slash_struct_dash_defns((pinfo_dash_env(pinfo)),id,fields)))); }))); }
function extend_dash_env_slash_struct_dash_defns(an_dash_env, id, fields) { return (function() {
               
var constructor_dash_id; 
var constructor_dash_binding; 
var predicate_dash_id; 
var predicate_dash_binding; 
var selector_dash_ids; 
var selector_dash_bindings; 

               return constructor_dash_id = (org.plt.Kernel.string_dash__greaterthan_symbol(org.plt.Kernel.format((org.plt.types.String.makeInstance("make-~a")), [id])));
constructor_dash_binding = (bf(constructor_dash_id,org.plt.types.Logic.FALSE,(org.plt.Kernel.length(fields)),org.plt.types.Logic.FALSE,(org.plt.Kernel.symbol_dash__greaterthan_string((identifier_dash__greaterthan_munged_dash_java_dash_identifier(constructor_dash_id))))));
predicate_dash_id = (org.plt.Kernel.string_dash__greaterthan_symbol(org.plt.Kernel.format((org.plt.types.String.makeInstance("~a?")), [id])));
predicate_dash_binding = (bf(predicate_dash_id,org.plt.types.Logic.FALSE,(org.plt.types.Rational.makeInstance(1, 1)),org.plt.types.Logic.FALSE,(org.plt.Kernel.symbol_dash__greaterthan_string((identifier_dash__greaterthan_munged_dash_java_dash_identifier(predicate_dash_id))))));
selector_dash_ids = org.plt.Kernel.map((function(args) { var f = args[0];
                             return (org.plt.Kernel.string_dash__greaterthan_symbol(org.plt.Kernel.format((org.plt.types.String.makeInstance("~a-~a")), [f]))); }), [fields]);
selector_dash_bindings = org.plt.Kernel.map((function(args) { var sel_dash_id = args[0];
                             return (bf(sel_dash_id,org.plt.types.Logic.FALSE,(org.plt.types.Rational.makeInstance(1, 1)),org.plt.types.Logic.FALSE,(org.plt.Kernel.symbol_dash__greaterthan_string((identifier_dash__greaterthan_munged_dash_java_dash_identifier(sel_dash_id)))))); }), [selector_dash_ids]);
org.plt.Kernel.identity((org.plt.Kernel.foldl((function(args) { var a_dash_binding = args[0];
var an_dash_env = args[1];
                             return (env_dash_extend(an_dash_env,a_dash_binding)); }),an_dash_env,org.plt.Kernel.list_star_(constructor_dash_binding, [selector_dash_bindings]))));
              })(); }
function definition_dash_analyze_dash_uses(a_dash_definition, pinfo) { return (case_dash_analyze_dash_definition(a_dash_definition,(function(args) { var id = args[0];
var args = args[1];
var body = args[2];
                             return (function_dash_definition_dash_analyze_dash_uses(id,args,body,pinfo)); }),(function(args) { var id = args[0];
var expr = args[1];
                             return (expression_dash_analyze_dash_uses(expr,pinfo,(pinfo_dash_env(pinfo)))); }),(function(args) { var id = args[0];
var fields = args[1];
                             return pinfo; }))); }
function function_dash_definition_dash_analyze_dash_uses(fun, args, body, pinfo) { return (function() {
               
var env_dash_1; 
var env_dash_2; 

               return env_dash_1 = (pinfo_dash_env(pinfo));
env_dash_2 = (env_dash_extend(env_dash_1,(bf(fun,org.plt.types.Logic.FALSE,(org.plt.Kernel.length(args)),org.plt.types.Logic.FALSE,(org.plt.Kernel.symbol_dash__greaterthan_string((identifier_dash__greaterthan_munged_dash_java_dash_identifier(fun))))))));
org.plt.Kernel.identity((lambda_dash_expression_dash_analyze_dash_uses(args,body,(pinfo_dash_update_dash_env(pinfo,env_dash_2)))));
              })(); }
function lambda_dash_expression_dash_analyze_dash_uses(args, body, pinfo) { return (function() {
               
var env_dash_1; 
var env_dash_2; 

               return env_dash_1 = (pinfo_dash_env(pinfo));
env_dash_2 = (org.plt.Kernel.foldl((function(args) { var arg_dash_id = args[0];
var env = args[1];
                             return (env_dash_extend(env,(make_dash_binding_colon_constant(arg_dash_id,(org.plt.Kernel.symbol_dash__greaterthan_string(arg_dash_id)),org.plt.types.Empty.EMPTY)))); }),env_dash_1,args));
org.plt.Kernel.identity((expression_dash_analyze_dash_uses(body,pinfo,env_dash_2)));
              })(); }
function expression_dash_analyze_dash_uses(an_dash_expression, pinfo, env) { return (((list_dash_begins_dash_with_question_(an_dash_expression,(org.plt.types.Symbol.makeInstance("local"))))) ? ((local_dash_expression_dash_analyze_dash_uses(an_dash_expression,pinfo,env))) : ((((list_dash_begins_dash_with_question_(an_dash_expression,(org.plt.types.Symbol.makeInstance("cond"))))) ? ((expression_dash_analyze_dash_uses((desugar_dash_cond(an_dash_expression)),pinfo,env))) : ((((list_dash_begins_dash_with_question_(an_dash_expression,(org.plt.types.Symbol.makeInstance("if"))))) ? ((if_dash_expression_dash_analyze_dash_uses(an_dash_expression,pinfo,env))) : ((((list_dash_begins_dash_with_question_(an_dash_expression,(org.plt.types.Symbol.makeInstance("and"))))) ? ((function() {
               
var exprs; 

               return exprs = (org.plt.Kernel.rest(an_dash_expression));
org.plt.Kernel.identity((org.plt.Kernel.foldl((function(args) { var e = args[0];
var p = args[1];
                             return (expression_dash_analyze_dash_uses(e,p,env)); }),pinfo,exprs)));
              })()) : ((((list_dash_begins_dash_with_question_(an_dash_expression,(org.plt.types.Symbol.makeInstance("or"))))) ? ((function() {
               
var exprs; 

               return exprs = (org.plt.Kernel.rest(an_dash_expression));
org.plt.Kernel.identity((org.plt.Kernel.foldl((function(args) { var e = args[0];
var p = args[1];
                             return (expression_dash_analyze_dash_uses(e,p,env)); }),pinfo,exprs)));
              })()) : ((((list_dash_begins_dash_with_question_(an_dash_expression,(org.plt.types.Symbol.makeInstance("lambda"))))) ? ((function() {
               
var args; 
var body; 

               return args = (org.plt.Kernel.second(an_dash_expression));
body = (org.plt.Kernel.third(an_dash_expression));
org.plt.Kernel.identity((lambda_dash_expression_dash_analyze_dash_uses(args,body,pinfo)));
              })()) : ((((org.plt.Kernel.number_question_(an_dash_expression))) ? (pinfo) : ((((org.plt.Kernel.string_question_(an_dash_expression))) ? (pinfo) : ((((org.plt.Kernel.char_question_(an_dash_expression))) ? (pinfo) : ((((org.plt.Kernel.symbol_question_(an_dash_expression))) ? ((((env_dash_contains_question_(env,an_dash_expression))) ? ((pinfo_dash_accumulate_dash_binding_dash_use((env_dash_lookup(env,an_dash_expression)),pinfo))) : (((org.plt.types.Logic.TRUE) ? (pinfo) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond"))))))))) : ((((list_dash_begins_dash_with_question_(an_dash_expression,(org.plt.types.Symbol.makeInstance("quote"))))) ? (pinfo) : ((((org.plt.Kernel.pair_question_(an_dash_expression))) ? ((application_dash_expression_dash_analyze_dash_uses(an_dash_expression,pinfo,env))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))))))))))))))))))))))); }
function local_dash_expression_dash_analyze_dash_uses(an_dash_expression, pinfo, env) { return (function() {
               
var defns; 
var body; 
var nested_dash_pinfo; 

               return defns = (org.plt.Kernel.second(an_dash_expression));
body = (org.plt.Kernel.third(an_dash_expression));
nested_dash_pinfo = (org.plt.Kernel.foldl((function(args) { var a_dash_defn = args[0];
var a_dash_pinfo = args[1];
                             return (definition_dash_analyze_dash_uses(a_dash_defn,a_dash_pinfo)); }),pinfo,defns));
org.plt.Kernel.identity((expression_dash_analyze_dash_uses(body,nested_dash_pinfo,(pinfo_dash_env(nested_dash_pinfo)))));
              })(); }
function if_dash_expression_dash_analyze_dash_uses(an_dash_expression, pinfo, env) { return (function() {
               
var test; 
var consequent; 
var alternative; 

               return test = (org.plt.Kernel.second(an_dash_expression));
consequent = (org.plt.Kernel.third(an_dash_expression));
alternative = (org.plt.Kernel.fourth(an_dash_expression));
org.plt.Kernel.identity((org.plt.Kernel.foldl((function(args) { var e = args[0];
var p = args[1];
                             return (expression_dash_analyze_dash_uses(e,p,env)); }),pinfo,org.plt.Kernel.list([]))));
              })(); }
function application_dash_expression_dash_analyze_dash_uses(an_dash_expression, pinfo, env) { return (function() {
               
var updated_dash_pinfo; 

               return updated_dash_pinfo = (org.plt.Kernel.foldl((function(args) { var e = args[0];
var p = args[1];
                             return (expression_dash_analyze_dash_uses(e,p,env)); }),pinfo,an_dash_expression));
org.plt.Kernel.identity(updated_dash_pinfo);
              })(); }
function module_dash_binding(name,path,bindings) { this.name = name;
this.path = path;
this.bindings = bindings; }
            module_dash_binding.prototype = new org.plt.Kernel.Struct();
module_dash_binding.prototype.isEqual = function(other) {
              if (other instanceof module_dash_binding) {
                return (((org.plt.Kernel.equal_question_((module_dash_binding_dash_bindings(this)),(module_dash_binding_dash_bindings(other)))))&&((((org.plt.Kernel.equal_question_((module_dash_binding_dash_path(this)),(module_dash_binding_dash_path(other)))))&&((((org.plt.Kernel.equal_question_((module_dash_binding_dash_name(this)),(module_dash_binding_dash_name(other)))))&&(org.plt.types.Logic.TRUE))))));
              } else {
                return false;
              }
           } 
function make_dash_module_dash_binding(id0,id1,id2) { return new module_dash_binding(id0,id1,id2); }
function module_dash_binding_dash_name(obj) { return obj.name; }
function module_dash_binding_dash_path(obj) { return obj.path; }
function module_dash_binding_dash_bindings(obj) { return obj.bindings; }
function module_dash_binding_question_(obj) { 
              return obj instanceof module_dash_binding ; 
            }
var world_dash_config_dash_module; 
function make_dash_world_dash_module(module_dash_path) { return (make_dash_module_dash_binding((org.plt.types.Symbol.makeInstance("world")),module_dash_path,org.plt.Kernel.append((module_dash_binding_dash_bindings(world_dash_config_dash_module)), [org.plt.Kernel.list([])]))); }
var world_dash_module; 
var world_dash_stub_dash_module; 
var bootstrap_dash_module; 
var location_dash_module; 
var tilt_dash_module; 
var gui_dash_world_dash_module; 
var sms_dash_module; 
var net_dash_module; 
var parser_dash_module; 
function extend_dash_env_slash_module_dash_binding(an_dash_env, a_dash_module_dash_binding) { return (function() {
               
function loop(an_dash_env, contents) { return (((org.plt.Kernel.empty_question_(contents))) ? (an_dash_env) : (((org.plt.types.Logic.TRUE) ? ((loop((env_dash_extend(an_dash_env,(org.plt.Kernel.first(contents)))),(org.plt.Kernel.rest(contents))))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))); }

               return org.plt.Kernel.identity((loop(an_dash_env,(module_dash_binding_dash_bindings(a_dash_module_dash_binding)))));
              })(); }
var known_dash_modules; 
function require_dash_analyze(require_dash_path, pinfo) { return (function() {
               
function loop(modules) { return (((org.plt.Kernel.empty_question_(modules))) ? ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("require-analyze")),org.plt.Kernel.format((org.plt.types.String.makeInstance("Moby doesn't know about module ~s yet")), [require_dash_path])))) : ((((path_equal__question_((org.plt.Kernel._resolveModulePath(require_dash_path,org.plt.types.Logic.FALSE)),(module_dash_binding_dash_path((org.plt.Kernel.first(modules))))))) ? ((pinfo_dash_accumulate_dash_module((org.plt.Kernel.first(modules)),(pinfo_dash_accumulate_dash_bindings((module_dash_binding_dash_bindings((org.plt.Kernel.first(modules)))),pinfo))))) : (((org.plt.types.Logic.TRUE) ? ((loop((org.plt.Kernel.rest(modules))))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))))); }

               return org.plt.Kernel.identity((loop(known_dash_modules)));
              })(); }
function compiled_dash_program(defns,toplevel_dash_exprs,pinfo) { this.defns = defns;
this.toplevel_dash_exprs = toplevel_dash_exprs;
this.pinfo = pinfo; }
            compiled_dash_program.prototype = new org.plt.Kernel.Struct();
compiled_dash_program.prototype.isEqual = function(other) {
              if (other instanceof compiled_dash_program) {
                return (((org.plt.Kernel.equal_question_((compiled_dash_program_dash_pinfo(this)),(compiled_dash_program_dash_pinfo(other)))))&&((((org.plt.Kernel.equal_question_((compiled_dash_program_dash_toplevel_dash_exprs(this)),(compiled_dash_program_dash_toplevel_dash_exprs(other)))))&&((((org.plt.Kernel.equal_question_((compiled_dash_program_dash_defns(this)),(compiled_dash_program_dash_defns(other)))))&&(org.plt.types.Logic.TRUE))))));
              } else {
                return false;
              }
           } 
function make_dash_compiled_dash_program(id0,id1,id2) { return new compiled_dash_program(id0,id1,id2); }
function compiled_dash_program_dash_defns(obj) { return obj.defns; }
function compiled_dash_program_dash_toplevel_dash_exprs(obj) { return obj.toplevel_dash_exprs; }
function compiled_dash_program_dash_pinfo(obj) { return obj.pinfo; }
function compiled_dash_program_question_(obj) { 
              return obj instanceof compiled_dash_program ; 
            }
function compiled_dash_program_dash_main(a_dash_compiled_dash_program) { return org.plt.Kernel.string_dash_append((compiled_dash_program_dash_defns(a_dash_compiled_dash_program)), [(org.plt.types.String.makeInstance("\n}"))]); }
function program_dash__greaterthan_compiled_dash_program(program) { return (_dash_program_dash__greaterthan_compiled_dash_program(program,(get_dash_base_dash_pinfo((org.plt.types.Symbol.makeInstance("js")))))); }
function _dash_program_dash__greaterthan_compiled_dash_program(program, input_dash_pinfo) { return (function() {
               
var a_dash_pinfo; 
var toplevel_dash_env; 
function loop(program, defns, tops) { return (((org.plt.Kernel.empty_question_(program))) ? ((make_dash_compiled_dash_program(defns,tops,a_dash_pinfo))) : (((org.plt.types.Logic.TRUE) ? ((((defn_question_((org.plt.Kernel.first(program))))) ? ((function() {
               
var defn_dash_string_plus_expr_dash_string; 

               return defn_dash_string_plus_expr_dash_string = (definition_dash__greaterthan_javascript_dash_strings((org.plt.Kernel.first(program)),toplevel_dash_env,a_dash_pinfo));
org.plt.Kernel.identity((loop((org.plt.Kernel.rest(program)),org.plt.Kernel.string_dash_append(defns, [(org.plt.Kernel.first(defn_dash_string_plus_expr_dash_string))]),org.plt.Kernel.string_dash_append(tops, [(org.plt.Kernel.second(defn_dash_string_plus_expr_dash_string))]))));
              })()) : ((((test_dash_case_question_((org.plt.Kernel.first(program))))) ? ((loop((org.plt.Kernel.rest(program)),org.plt.Kernel.string_dash_append(defns, [(org.plt.types.String.makeInstance("// Test case erased\n"))]),tops))) : ((((library_dash_require_question_((org.plt.Kernel.first(program))))) ? ((loop((org.plt.Kernel.rest(program)),org.plt.Kernel.string_dash_append(defns, [(org.plt.types.String.makeInstance("// Module require erased\n"))]),tops))) : ((((expression_question_((org.plt.Kernel.first(program))))) ? ((loop((org.plt.Kernel.rest(program)),defns,org.plt.Kernel.string_dash_append(tops, [(org.plt.types.String.makeInstance(");"))])))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond"))))))))))))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))); }

               return a_dash_pinfo = (program_dash_analyze_slash_pinfo(program,input_dash_pinfo));
toplevel_dash_env = (pinfo_dash_env(a_dash_pinfo));

org.plt.Kernel.identity((loop(program,(org.plt.types.String.makeInstance("")),(org.plt.types.String.makeInstance("")))));
              })(); }
function definition_dash__greaterthan_javascript_dash_strings(defn, env, a_dash_pinfo) { return (case_dash_analyze_dash_definition(defn,(function(args) { var fun = args[0];
var args = args[1];
var body = args[2];
                             return org.plt.Kernel.list([]); }),(function(args) { var id = args[0];
var body = args[1];
                             return (variable_dash_definition_dash__greaterthan_javascript_dash_strings(id,body,env,a_dash_pinfo)); }),(function(args) { var id = args[0];
var fields = args[1];
                             return org.plt.Kernel.list([]); }))); }
function function_dash_definition_dash__greaterthan_java_dash_string(fun, args, body, env, a_dash_pinfo) { return (function() {
               
var munged_dash_fun_dash_id; 
var munged_dash_arg_dash_ids; 
var new_dash_env; 
var env_dash_with_dash_arg_dash_bindings; 

               return munged_dash_fun_dash_id = (identifier_dash__greaterthan_munged_dash_java_dash_identifier(fun));
munged_dash_arg_dash_ids = org.plt.Kernel.map((function(args) {
                    return identifier_dash__greaterthan_munged_dash_java_dash_identifier(args[0]);
                 }), [args]);
new_dash_env = (env_dash_extend_dash_function(env,fun,org.plt.types.Logic.FALSE,(org.plt.Kernel.length(args)),org.plt.types.Logic.FALSE,(org.plt.Kernel.symbol_dash__greaterthan_string(munged_dash_fun_dash_id))));
env_dash_with_dash_arg_dash_bindings = (org.plt.Kernel.foldl((function(args) { var arg_dash_id = args[0];
var env = args[1];
                             return (env_dash_extend(env,(make_dash_binding_colon_constant(arg_dash_id,(org.plt.Kernel.symbol_dash__greaterthan_string((identifier_dash__greaterthan_munged_dash_java_dash_identifier(arg_dash_id)))),org.plt.types.Empty.EMPTY)))); }),new_dash_env,args));
org.plt.Kernel.identity(org.plt.Kernel.format((org.plt.types.String.makeInstance("function ~a(~a) { return ~a; }")), [(expression_dash__greaterthan_javascript_dash_string(body,env_dash_with_dash_arg_dash_bindings,a_dash_pinfo))]));
              })(); }
function variable_dash_definition_dash__greaterthan_javascript_dash_strings(id, body, env, a_dash_pinfo) { return (function() {
               
var munged_dash_id; 
var new_dash_env; 

               return munged_dash_id = (identifier_dash__greaterthan_munged_dash_java_dash_identifier(id));
new_dash_env = (env_dash_extend(env,(make_dash_binding_colon_constant(id,(org.plt.Kernel.symbol_dash__greaterthan_string(munged_dash_id)),org.plt.types.Empty.EMPTY))));
org.plt.Kernel.identity(org.plt.Kernel.list([]));
              })(); }
function struct_dash_definition_dash__greaterthan_javascript_dash_string(id, fields, env, a_dash_pinfo) { return (function() {
               
function field_dash__greaterthan_accessor_dash_name(struct_dash_name, field_dash_name) { return (org.plt.Kernel.string_dash__greaterthan_symbol(org.plt.Kernel.string_dash_append((org.plt.Kernel.symbol_dash__greaterthan_string(struct_dash_name)), [(org.plt.Kernel.symbol_dash__greaterthan_string(field_dash_name))]))); }

               return org.plt.Kernel.identity(org.plt.Kernel.string_dash_append(org.plt.Kernel.format((org.plt.types.String.makeInstance("function ~a(~a) { ~a }\n            ~a.prototype = new org.plt.Kernel.Struct();")), [(identifier_dash__greaterthan_munged_dash_java_dash_identifier(id))]), [org.plt.Kernel.format((org.plt.types.String.makeInstance("function ~a(obj) { \n              return obj instanceof ~a ; \n            }")), [(identifier_dash__greaterthan_munged_dash_java_dash_identifier(id))])]));
              })(); }
function expression_dash__greaterthan_javascript_dash_string(expr, env, a_dash_pinfo) { return (((list_dash_begins_dash_with_question_(expr,(org.plt.types.Symbol.makeInstance("local"))))) ? ((function() {
               
var defns; 
var body; 

               return defns = (org.plt.Kernel.second(expr));
body = (org.plt.Kernel.third(expr));
org.plt.Kernel.identity((local_dash_expression_dash__greaterthan_javascript_dash_string(defns,body,env,a_dash_pinfo)));
              })()) : ((((list_dash_begins_dash_with_question_(expr,(org.plt.types.Symbol.makeInstance("cond"))))) ? ((expression_dash__greaterthan_javascript_dash_string((desugar_dash_cond(expr)),env,a_dash_pinfo))) : ((((list_dash_begins_dash_with_question_(expr,(org.plt.types.Symbol.makeInstance("if"))))) ? ((function() {
               
var test; 
var consequent; 
var alternative; 

               return test = (org.plt.Kernel.second(expr));
consequent = (org.plt.Kernel.third(expr));
alternative = (org.plt.Kernel.fourth(expr));
org.plt.Kernel.identity(org.plt.Kernel.format((org.plt.types.String.makeInstance("((~a) ? (~a) : (~a))")), [(expression_dash__greaterthan_javascript_dash_string(alternative,env,a_dash_pinfo))]));
              })()) : ((((list_dash_begins_dash_with_question_(expr,(org.plt.types.Symbol.makeInstance("and"))))) ? ((function() {
               
var exprs; 

               return exprs = (org.plt.Kernel.rest(expr));
org.plt.Kernel.identity(org.plt.Kernel.string_dash_append((org.plt.types.String.makeInstance("(")), [(org.plt.types.String.makeInstance(")"))]));
              })()) : ((((list_dash_begins_dash_with_question_(expr,(org.plt.types.Symbol.makeInstance("or"))))) ? ((function() {
               
var exprs; 

               return exprs = (org.plt.Kernel.rest(expr));
org.plt.Kernel.identity(org.plt.Kernel.string_dash_append((org.plt.types.String.makeInstance("(")), [(org.plt.types.String.makeInstance(")"))]));
              })()) : ((((list_dash_begins_dash_with_question_(expr,(org.plt.types.Symbol.makeInstance("lambda"))))) ? ((function() {
               
var args; 
var body; 

               return args = (org.plt.Kernel.second(expr));
body = (org.plt.Kernel.third(expr));
org.plt.Kernel.identity((lambda_dash_expression_dash__greaterthan_javascript_dash_string(args,body,env,a_dash_pinfo)));
              })()) : ((((org.plt.Kernel.number_question_(expr))) ? ((number_dash__greaterthan_javascript_dash_string(expr))) : ((((org.plt.Kernel.string_question_(expr))) ? ((string_dash__greaterthan_javascript_dash_string(expr))) : ((((org.plt.Kernel.char_question_(expr))) ? ((char_dash__greaterthan_javascript_dash_string(expr))) : ((((org.plt.Kernel.symbol_question_(expr))) ? ((identifier_dash_expression_dash__greaterthan_javascript_dash_string(expr,env,a_dash_pinfo))) : ((((list_dash_begins_dash_with_question_(expr,(org.plt.types.Symbol.makeInstance("quote"))))) ? ((quote_dash_expression_dash__greaterthan_javascript_dash_string((org.plt.Kernel.second(expr))))) : ((((org.plt.Kernel.pair_question_(expr))) ? ((function() {
               
var operator; 
var operands; 

               return operator = (org.plt.Kernel.first(expr));
operands = (org.plt.Kernel.rest(expr));
org.plt.Kernel.identity((application_dash_expression_dash__greaterthan_javascript_dash_string(operator,operands,env,a_dash_pinfo)));
              })()) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))))))))))))))))))))))); }
function quote_dash_expression_dash__greaterthan_javascript_dash_string(expr) { return (((org.plt.Kernel.empty_question_(expr))) ? ((org.plt.types.String.makeInstance("org.plt.types.Empty.EMPTY"))) : ((((org.plt.Kernel.pair_question_(expr))) ? (org.plt.Kernel.format((org.plt.types.String.makeInstance("(org.plt.Kernel.cons(~a, ~a))")), [(quote_dash_expression_dash__greaterthan_javascript_dash_string((org.plt.Kernel.rest(expr))))])) : ((((org.plt.Kernel.symbol_question_(expr))) ? (org.plt.Kernel.format((org.plt.types.String.makeInstance("(org.plt.types.Symbol.makeInstance(\"~a\"))")), [expr])) : ((((org.plt.Kernel.number_question_(expr))) ? ((number_dash__greaterthan_javascript_dash_string(expr))) : ((((org.plt.Kernel.string_question_(expr))) ? ((string_dash__greaterthan_javascript_dash_string(expr))) : ((((org.plt.Kernel.char_question_(expr))) ? ((char_dash__greaterthan_javascript_dash_string(expr))) : (((org.plt.types.Logic.TRUE) ? ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("quote-expression->javascript-string")),org.plt.Kernel.format((org.plt.types.String.makeInstance("I don't know how to deal with ~s")), [expr])))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))))))))))))); }
function local_dash_expression_dash__greaterthan_javascript_dash_string(defns, body, env, a_dash_pinfo) { return (function() {
               
var inner_dash_compiled_dash_program; 

               return inner_dash_compiled_dash_program = (_dash_program_dash__greaterthan_compiled_dash_program(org.plt.Kernel.append(defns, [org.plt.Kernel.list([])]),(pinfo_dash_update_dash_env(a_dash_pinfo,env))));
org.plt.Kernel.identity(org.plt.Kernel.format((org.plt.types.String.makeInstance("(function() {\n               ~a\n\n               return ~a\n              })()")), [(remove_dash_leading_dash_whitespace((compiled_dash_program_dash_toplevel_dash_exprs(inner_dash_compiled_dash_program))))]));
              })(); }
function application_dash_expression_dash__greaterthan_javascript_dash_string(operator, operands, env, a_dash_pinfo) { return (((((org.plt.Kernel.symbol_question_(operator)))&&((org.plt.Kernel.not((env_dash_contains_question_(env,operator))))))) ? ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("application-expression->java-string")),org.plt.Kernel.format((org.plt.types.String.makeInstance("Moby doesn't know about ~s")), [operator])))) : ((((org.plt.Kernel.symbol_question_(operator))) ? ((function() {
               
var operator_dash_binding; 
var operand_dash_strings; 

               return operator_dash_binding = (env_dash_lookup(env,operator));
operand_dash_strings = org.plt.Kernel.map((function(args) { var e = args[0];
                             return (expression_dash__greaterthan_javascript_dash_string(e,env,a_dash_pinfo)); }), [operands]);
org.plt.Kernel.identity((((binding_colon_constant_question_(operator_dash_binding))) ? (org.plt.Kernel.format((org.plt.types.String.makeInstance("((~a).apply(null, [~a]))")), [(string_dash_join(operand_dash_strings,(org.plt.types.String.makeInstance(", "))))])) : ((((binding_colon_function_question_(operator_dash_binding))) ? (((org.plt.Kernel._lessthan_((org.plt.Kernel.length(operands)),(binding_colon_function_dash_min_dash_arity(operator_dash_binding)), [(org.plt.Kernel.length(operands)),(binding_colon_function_dash_min_dash_arity(operator_dash_binding))])) ? ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("application-expression->java-string")),org.plt.Kernel.format((org.plt.types.String.makeInstance("Minimal arity of ~s not met.  Operands were ~s")), [operands])))) : ((((binding_colon_function_dash_var_dash_arity_question_(operator_dash_binding))) ? (((org.plt.Kernel._greaterthan_((binding_colon_function_dash_min_dash_arity(operator_dash_binding)),(org.plt.types.Rational.makeInstance(0, 1)), [(binding_colon_function_dash_min_dash_arity(operator_dash_binding)),(org.plt.types.Rational.makeInstance(0, 1))])) ? (org.plt.Kernel.format((org.plt.types.String.makeInstance("~a(~a, [~a])")), [(string_dash_join((list_dash_tail(operand_dash_strings,(binding_colon_function_dash_min_dash_arity(operator_dash_binding)))),(org.plt.types.String.makeInstance(","))))])) : (((org.plt.types.Logic.TRUE) ? (org.plt.Kernel.format((org.plt.types.String.makeInstance("~a([~a])")), [(string_dash_join((list_dash_tail(operand_dash_strings,(binding_colon_function_dash_min_dash_arity(operator_dash_binding)))),(org.plt.types.String.makeInstance(","))))])) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond"))))))))) : (((org.plt.types.Logic.TRUE) ? (org.plt.Kernel.format((org.plt.types.String.makeInstance("(~a(~a))")), [(string_dash_join(operand_dash_strings,(org.plt.types.String.makeInstance(","))))])) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond"))))))))))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))));
              })()) : (((org.plt.types.Logic.TRUE) ? ((function() {
               
var operator_dash_string; 
var operand_dash_strings; 

               return operator_dash_string = (expression_dash__greaterthan_javascript_dash_string(operator,env,a_dash_pinfo));
operand_dash_strings = org.plt.Kernel.map((function(args) { var e = args[0];
                             return (expression_dash__greaterthan_javascript_dash_string(e,env,a_dash_pinfo)); }), [operands]);
org.plt.Kernel.identity(org.plt.Kernel.format((org.plt.types.String.makeInstance("((~a).apply(null, [~a]))")), [(string_dash_join(operand_dash_strings,(org.plt.types.String.makeInstance(", "))))]));
              })()) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))))); }
function identifier_dash_expression_dash__greaterthan_javascript_dash_string(an_dash_id, an_dash_env, a_dash_pinfo) { return (((org.plt.Kernel.not((env_dash_contains_question_(an_dash_env,an_dash_id))))) ? ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("translate-toplevel-id")),org.plt.Kernel.format((org.plt.types.String.makeInstance("Moby doesn't know about ~s.")), [an_dash_id])))) : (((org.plt.types.Logic.TRUE) ? ((function() {
               
var binding; 

               return binding = (env_dash_lookup(an_dash_env,an_dash_id));
org.plt.Kernel.identity((((binding_colon_constant_question_(binding))) ? ((binding_colon_constant_dash_java_dash_string(binding))) : ((((binding_colon_function_question_(binding))) ? ((((binding_colon_function_dash_var_dash_arity_question_(binding))) ? (org.plt.Kernel.format((org.plt.types.String.makeInstance("(function(args) {\n                    return ~a.apply(null, args);\n                  })")), [(binding_colon_function_dash_java_dash_string(binding))])) : (((org.plt.types.Logic.TRUE) ? (org.plt.Kernel.format((org.plt.types.String.makeInstance("(function(args) {\n                    return ~a(~a);\n                 })")), [(string_dash_join(org.plt.Kernel.map((function(args) { var i = args[0];
                             return org.plt.Kernel.format((org.plt.types.String.makeInstance("args[~a]")), [i]); }), [(range((binding_colon_function_dash_min_dash_arity(binding))))]),(org.plt.types.String.makeInstance(", "))))])) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond"))))))))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))));
              })()) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))); }
function mapi(f, elts) { return (function() {
               
function loop(i, elts) { return (((org.plt.Kernel.empty_question_(elts))) ? (org.plt.types.Empty.EMPTY) : (((org.plt.types.Logic.TRUE) ? ((org.plt.Kernel.cons(((f).apply(null, [(org.plt.Kernel.first(elts)), i])),(loop((org.plt.Kernel.add1(i)),(org.plt.Kernel.rest(elts))))))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))); }

               return org.plt.Kernel.identity((loop((org.plt.types.Rational.makeInstance(0, 1)),elts)));
              })(); }
function lambda_dash_expression_dash__greaterthan_javascript_dash_string(args, body, env, a_dash_pinfo) { return (function() {
               
var munged_dash_arg_dash_ids; 
var new_dash_env; 

               return munged_dash_arg_dash_ids = org.plt.Kernel.map((function(args) {
                    return identifier_dash__greaterthan_munged_dash_java_dash_identifier(args[0]);
                 }), [args]);
new_dash_env = (org.plt.Kernel.foldl((function(args) { var arg_dash_id = args[0];
var env = args[1];
                             return (env_dash_extend(env,(make_dash_binding_colon_constant(arg_dash_id,(org.plt.Kernel.symbol_dash__greaterthan_string((identifier_dash__greaterthan_munged_dash_java_dash_identifier(arg_dash_id)))),org.plt.types.Empty.EMPTY)))); }),env,args));
org.plt.Kernel.identity(org.plt.Kernel.format((org.plt.types.String.makeInstance("(function(args) { ~a\n                             return ~a; })")), [(expression_dash__greaterthan_javascript_dash_string(body,new_dash_env,a_dash_pinfo))]));
              })(); }
function number_dash__greaterthan_javascript_dash_string(a_dash_num) { return (((org.plt.Kernel.integer_question_(a_dash_num))) ? (org.plt.Kernel.format((org.plt.types.String.makeInstance("(org.plt.types.Rational.makeInstance(~a, 1))")), [(org.plt.Kernel.inexact_dash__greaterthan_exact(a_dash_num))])) : ((((((org.plt.Kernel.inexact_question_(a_dash_num)))&&((org.plt.Kernel.real_question_(a_dash_num))))) ? (org.plt.Kernel.format((org.plt.types.String.makeInstance("(org.plt.types.FloatPoint.makeInstance(\"~a\"))")), [a_dash_num])) : ((((org.plt.Kernel.rational_question_(a_dash_num))) ? (org.plt.Kernel.format((org.plt.types.String.makeInstance("(org.plt.types.Rational.makeInstance(~a, ~a))")), [(org.plt.Kernel.denominator(a_dash_num))])) : ((((org.plt.Kernel.complex_question_(a_dash_num))) ? (org.plt.Kernel.format((org.plt.types.String.makeInstance("(org.plt.types.Complex.makeInstance(~a, ~a))")), [(number_dash__greaterthan_javascript_dash_string((org.plt.Kernel.imag_dash_part(a_dash_num))))])) : (((org.plt.types.Logic.TRUE) ? ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("number->java-string")),(org.plt.types.String.makeInstance("Don't know how to handle ~s yet")),a_dash_num))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))))))))); }
function string_dash__greaterthan_javascript_dash_string(a_dash_str) { return org.plt.Kernel.format((org.plt.types.String.makeInstance("(org.plt.types.String.makeInstance(~s))")), [a_dash_str]); }
function char_dash__greaterthan_javascript_dash_string(a_dash_char) { return org.plt.Kernel.string_dash_append((org.plt.types.String.makeInstance("(org.plt.types.Character.makeInstance(\"")), [(org.plt.types.String.makeInstance("\"))"))]); }
function toplevel() {






PERMISSION_colon_LOCATION = (make_dash_permission_colon_location());
PERMISSION_colon_SMS = (make_dash_permission_colon_sms());
PERMISSION_colon_TILT = (make_dash_permission_colon_tilt());
PERMISSION_colon_INTERNET = (make_dash_permission_colon_internet());





empty_dash_env = (make_dash_env((org.plt.Kernel._kernelMakeImmutableHashEq(org.plt.types.Empty.EMPTY))));


























toplevel_dash_env = (function() {
               
var top_dash_env_dash_1; 
function r(env, a_dash_name, arity, vararity_question_) { return (env_dash_extend_dash_function(env,a_dash_name,(org.plt.Kernel._resolveModulePath((org.plt.types.Symbol.makeInstance("lang/htdp-beginner")),org.plt.types.Logic.FALSE)),arity,vararity_question_,org.plt.Kernel.format((org.plt.types.String.makeInstance("org.plt.Kernel.~a")), [(identifier_dash__greaterthan_munged_dash_java_dash_identifier(a_dash_name))]))); }
function r_star_(env, a_dash_name, arity, java_dash_string) { return (env_dash_extend_dash_function(env,a_dash_name,(org.plt.Kernel._resolveModulePath((org.plt.types.Symbol.makeInstance("lang/htdp-beginner")),org.plt.types.Logic.FALSE)),arity,org.plt.types.Logic.FALSE,java_dash_string)); }
var top_dash_env_dash_2; 
var top_dash_env_dash_3; 

               return top_dash_env_dash_1 = (org.plt.Kernel.foldl((function(args) { var id_plus_name = args[0];
var env = args[1];
                             return (env_dash_extend_dash_constant(env,(org.plt.Kernel.first(id_plus_name)),(org.plt.Kernel.second(id_plus_name)))); }),empty_dash_env,(org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("null")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("org.plt.types.Empty.EMPTY")), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("empty")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("org.plt.types.Empty.EMPTY")), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("org.plt.types.Logic.TRUE")), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("false")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("org.plt.types.Logic.FALSE")), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("eof")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("org.plt.types.EofObject.EOF")), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("pi")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("org.plt.Kernel.pi")), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("e")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("org.plt.Kernel.pi")), org.plt.types.Empty.EMPTY)))), org.plt.types.Empty.EMPTY))))))))))))))));


top_dash_env_dash_2 = (org.plt.Kernel.foldl((function(args) { var name_plus_arity = args[0];
var env = args[1];
                             return ((org.plt.Kernel._equal_((org.plt.Kernel.length(name_plus_arity)),(org.plt.types.Rational.makeInstance(2, 1)), [(org.plt.Kernel.length(name_plus_arity)),(org.plt.types.Rational.makeInstance(2, 1))])) ? ((r(env,(org.plt.Kernel.first(name_plus_arity)),(org.plt.Kernel.second(name_plus_arity)),org.plt.types.Logic.FALSE))) : (((org.plt.Kernel._equal_((org.plt.Kernel.length(name_plus_arity)),(org.plt.types.Rational.makeInstance(3, 1)), [(org.plt.Kernel.length(name_plus_arity)),(org.plt.types.Rational.makeInstance(3, 1))])) ? ((r(env,(org.plt.Kernel.first(name_plus_arity)),(org.plt.Kernel.second(name_plus_arity)),(((org.plt.Kernel.symbol_equal__question_((org.plt.Kernel.third(name_plus_arity)),(org.plt.types.Symbol.makeInstance("true"))))) ? (org.plt.types.Logic.TRUE) : (org.plt.types.Logic.FALSE))))) : ((org.plt.Kernel.error((org.plt.types.Symbol.makeInstance("cond")),(org.plt.types.String.makeInstance("Fell out of cond")))))))); }),top_dash_env_dash_1,(org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("+")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(0, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("-")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("*")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(0, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("/")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance(">=")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance(">")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("<=")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("<")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("=")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("=~")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(3, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("number->string")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("even?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("odd?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("positive?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("negative?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("number?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("rational?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("quotient")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("remainder")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("numerator")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("denominator")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("integer?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("real?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("abs")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("acos")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("asin")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("atan")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("random")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("max")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("min")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("sqr")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("sqrt")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("modulo")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("add1")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("sub1")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("zero?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("exp")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("expt")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("sgn")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("log")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("gcd")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("lcm")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("round")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("floor")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("ceiling")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("sin")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("cos")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("tan")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("sinh")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("cosh")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("angle")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("conjugate")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("magnitude")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("exact->inexact")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("inexact->exact")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("inexact?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("complex?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("real-part")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("imag-part")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("not")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("false?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("boolean?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("boolean=?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char=?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char<?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char<=?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char>?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char>=?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char-ci<=?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char-ci<?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char-ci=?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char-ci>=?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char-ci>?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char-downcase")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char-lower-case?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char-numeric?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char-upcase")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char-upper-case?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char-whitespace?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char-alphabetic?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("char->integer")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("integer->char")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("symbol=?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("symbol->string")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string=?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("symbol?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string>?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string>=?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string<?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string<=?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string-ci<=?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string-ci<?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string-ci=?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string-ci>=?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string-ci>?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("substring")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(3, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string-length")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string-ref")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string-copy")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string->number")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string->list")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string->symbol")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string-append")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("list->string")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("make-string")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("string")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("empty?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("first")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("second")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("third")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("fourth")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("fifth")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("sixth")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("seventh")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("eighth")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("rest")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("cons")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("pair?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("cons?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("null?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("length")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("list")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(0, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("list*")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("list-ref")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("append")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("member")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("memq")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("memv")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("reverse")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("caaar")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("caadr")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("caar")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("cadar")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("cadddr")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("caddr")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("cadr")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("car")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("cdaar")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("cdadr")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("cdar")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("cddar")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("cdddr")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("cddr")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("cdr")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("make-posn")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("posn-x")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("posn-y")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("posn?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("eof-object?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("equal?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("eq?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("eqv?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("equal~?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(3, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("error")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("struct?")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("identity")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("current-seconds")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(0, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("andmap")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("foldl")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(3, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("build-list")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), org.plt.types.Empty.EMPTY)))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("map")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("format")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), (org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("true")), org.plt.types.Empty.EMPTY)))))), org.plt.types.Empty.EMPTY))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
top_dash_env_dash_3 = (org.plt.Kernel.foldl((function(args) { var id_plus_arity_plus_name = args[0];
var env = args[1];
                             return (r_star_(env,(org.plt.Kernel.first(id_plus_arity_plus_name)),(org.plt.Kernel.second(id_plus_arity_plus_name)),(org.plt.Kernel.third(id_plus_arity_plus_name)))); }),top_dash_env_dash_2,(org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("make-immutable-hasheq")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("org.plt.Kernel._kernelMakeImmutableHashEq")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("make-immutable-hash")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("org.plt.Kernel._kernelMakeImmutableHash")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("hash-set!")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(3, 1)), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("org.plt.Kernel._kernelHashSetBang")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("hash-set")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(3, 1)), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("org.plt.Kernel._kernelHashSet")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("hash-ref")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(3, 1)), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("org.plt.Kernel._kernelHashRef")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("hash-map")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("org.plt.Kernel._kernelHashMap")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("path->string")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("org.plt.Kernel._pathToString")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("normalize-path")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(1, 1)), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("org.plt.Kernel._normalizePath")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("resolve-module-path")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("org.plt.Kernel._resolveModulePath")), org.plt.types.Empty.EMPTY)))))), (org.plt.Kernel.cons((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("build-path")), (org.plt.Kernel.cons((org.plt.types.Rational.makeInstance(2, 1)), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("org.plt.Kernel._buildPath")), org.plt.types.Empty.EMPTY)))))), org.plt.types.Empty.EMPTY))))))))))))))))))))));
org.plt.Kernel.identity(top_dash_env_dash_3);
              })();

empty_dash_pinfo = (make_dash_pinfo(empty_dash_env,org.plt.types.Empty.EMPTY,(org.plt.Kernel._kernelMakeImmutableHash(org.plt.types.Empty.EMPTY))));





















world_dash_config_dash_module = (function() {
               
var module_dash_path; 

               return module_dash_path = (org.plt.Kernel._resolveModulePath((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("lib")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("world-config.ss")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("moby")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("stub")), org.plt.types.Empty.EMPTY)))))))),org.plt.types.Logic.FALSE));
org.plt.Kernel.identity((make_dash_module_dash_binding((org.plt.types.Symbol.makeInstance("world-config")),module_dash_path,org.plt.Kernel.list([]))));
              })();

world_dash_module = (function() {
               
var module_dash_path; 

               return module_dash_path = (org.plt.Kernel._resolveModulePath((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("lib")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("world.ss")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("teachpack")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("htdp")), org.plt.types.Empty.EMPTY)))))))),org.plt.types.Logic.FALSE));
org.plt.Kernel.identity((make_dash_world_dash_module(module_dash_path)));
              })();
world_dash_stub_dash_module = (function() {
               
var module_dash_path; 

               return module_dash_path = (org.plt.Kernel._resolveModulePath((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("lib")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("world.ss")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("moby")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("stub")), org.plt.types.Empty.EMPTY)))))))),org.plt.types.Logic.FALSE));
org.plt.Kernel.identity((make_dash_world_dash_module(module_dash_path)));
              })();
bootstrap_dash_module = (function() {
               
var module_dash_path; 

               return module_dash_path = (org.plt.Kernel._resolveModulePath((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("lib")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("bootstrap.ss")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("moby")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("stub")), org.plt.types.Empty.EMPTY)))))))),org.plt.types.Logic.FALSE));
org.plt.Kernel.identity((make_dash_module_dash_binding((org.plt.types.Symbol.makeInstance("world")),module_dash_path,org.plt.Kernel.append(org.plt.Kernel.list([]), [(module_dash_binding_dash_bindings(world_dash_stub_dash_module))]))));
              })();
location_dash_module = (function() {
               
var module_dash_path; 
function bf(name, module_dash_path, arity, vararity_question_, java_dash_string) { return (make_dash_binding_colon_function(name,module_dash_path,arity,vararity_question_,java_dash_string,org.plt.Kernel.list([]),org.plt.types.Logic.FALSE)); }

               return module_dash_path = (org.plt.Kernel._resolveModulePath((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("lib")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("location.ss")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("moby")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("stub")), org.plt.types.Empty.EMPTY)))))))),org.plt.types.Logic.FALSE));

org.plt.Kernel.identity((make_dash_module_dash_binding((org.plt.types.Symbol.makeInstance("location")),module_dash_path,org.plt.Kernel.list([]))));
              })();
tilt_dash_module = (function() {
               
var module_dash_path; 
function bf(name, module_dash_path, arity, vararity_question_, java_dash_string) { return (make_dash_binding_colon_function(name,module_dash_path,arity,vararity_question_,java_dash_string,org.plt.Kernel.list([]),org.plt.types.Logic.TRUE)); }

               return module_dash_path = (org.plt.Kernel._resolveModulePath((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("lib")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("tilt.ss")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("moby")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("stub")), org.plt.types.Empty.EMPTY)))))))),org.plt.types.Logic.FALSE));

org.plt.Kernel.identity((make_dash_module_dash_binding((org.plt.types.Symbol.makeInstance("tilt")),module_dash_path,org.plt.Kernel.list([]))));
              })();
gui_dash_world_dash_module = (function() {
               
var module_dash_path; 

               return module_dash_path = (org.plt.Kernel._resolveModulePath((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("lib")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("gui-world.ss")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("gui-world")), org.plt.types.Empty.EMPTY)))))),org.plt.types.Logic.FALSE));
org.plt.Kernel.identity((make_dash_module_dash_binding((org.plt.types.Symbol.makeInstance("gui-world")),module_dash_path,org.plt.Kernel.append((module_dash_binding_dash_bindings(world_dash_config_dash_module)), [org.plt.Kernel.list([])]))));
              })();
sms_dash_module = (function() {
               
var module_dash_path; 

               return module_dash_path = (org.plt.Kernel._resolveModulePath((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("lib")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("sms.ss")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("moby")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("stub")), org.plt.types.Empty.EMPTY)))))))),org.plt.types.Logic.FALSE));
org.plt.Kernel.identity((make_dash_module_dash_binding((org.plt.types.Symbol.makeInstance("sms")),module_dash_path,org.plt.Kernel.list([]))));
              })();
net_dash_module = (function() {
               
var module_dash_path; 

               return module_dash_path = (org.plt.Kernel._resolveModulePath((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("lib")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("net.ss")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("moby")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("stub")), org.plt.types.Empty.EMPTY)))))))),org.plt.types.Logic.FALSE));
org.plt.Kernel.identity((make_dash_module_dash_binding((org.plt.types.Symbol.makeInstance("net")),module_dash_path,org.plt.Kernel.list([]))));
              })();
parser_dash_module = (function() {
               
var module_dash_path; 

               return module_dash_path = (org.plt.Kernel._resolveModulePath((org.plt.Kernel.cons((org.plt.types.Symbol.makeInstance("lib")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("parser.ss")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("moby")), (org.plt.Kernel.cons((org.plt.types.String.makeInstance("stub")), org.plt.types.Empty.EMPTY)))))))),org.plt.types.Logic.FALSE));
org.plt.Kernel.identity((make_dash_module_dash_binding((org.plt.types.Symbol.makeInstance("parser")),module_dash_path,org.plt.Kernel.list([]))));
              })();

known_dash_modules = org.plt.Kernel.list([]);



















}