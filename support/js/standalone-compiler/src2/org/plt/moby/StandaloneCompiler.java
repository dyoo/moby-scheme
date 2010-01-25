package org.plt.moby;

import org.mozilla.javascript.*;
import java.util.Set;
import java.util.HashSet;
import java.io.InputStream;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.IOException;

import javax.script.ScriptEngineManager;
import javax.script.ScriptEngine;
import javax.script.ScriptException;


public class StandaloneCompiler {
    private _StandaloneCompiler impl;

    public StandaloneCompiler() throws IOException {

	long startTime = System.currentTimeMillis();
	
// 	Reader reader = new BufferedReader(new InputStreamReader
// 	    (this.getClass().getResourceAsStream
// 	     ("/org/plt/moby/compiler.js")));
// 	Context ctx = Context.enter();
// 	ctx.setOptimizationLevel(-1);
// 	try {
// 	    ScriptableObject scope = ctx.initStandardObjects();
// 	    ctx.evaluateReader(scope, reader, "compiler.js", 0, null);
// 	} finally {
// 	    ctx.exit();
// 	}


//   ScriptEngineManager engineMgr = new ScriptEngineManager();
//   ScriptEngine engine = engineMgr.getEngineByName("JavaScript");
//   InputStream is = 
//       this.getClass().getResourceAsStream("/org/plt/moby/compiler.js");
//   try {
//     Reader reader = new InputStreamReader(is);
//     engine.eval(reader);
//   } catch (ScriptException ex) {
//     ex.printStackTrace();
//   }
	Context ctx = Context.enter();
	try {
	    ScriptableObject scope = ctx.initStandardObjects();
	    ((Script)(new base())).exec(ctx, scope);
	    ((Script)(new jshashtable())).exec(ctx, scope);
	    ((Script)(new types())).exec(ctx, scope);
	    ((Script)(new kernel())).exec(ctx, scope);
	    ((Script)(new stx())).exec(ctx, scope);
	    ((Script)(new read())).exec(ctx, scope);
	    ((Script)(new compiler())).exec(ctx, scope);


	    ((Script)(new _StandaloneCompiler())).exec(ctx, scope);
	    //this.impl = new _StandaloneCompiler(ctx, scope);


	} finally {
	    ctx.exit();
	}

	long endTime = System.currentTimeMillis();
	System.out.println(endTime - startTime);
    }


    public CompilationResult compile(String src) {
	return null;
// 	Object[] results = this.impl.compileScheme(src);
// 	String compiledText = results[0].toString();
// 	Scriptable permArray = (Scriptable) results[1];
// 	double length = (Double) permArray.get("length", permArray);
// 	Set<String> permissions = new HashSet<String>();
// 	for (int i = 0; i < length; i++) {
// 	    permissions.add(permArray.get(i, permArray).toString());
// 	}
// 	return new CompilationResult(compiledText, permissions);
    }
}