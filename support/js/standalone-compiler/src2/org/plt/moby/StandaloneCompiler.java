package org.plt.moby;

import org.mozilla.javascript.*;
import java.util.Set;
import java.util.HashSet;
import java.io.InputStream;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.IOException;

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


	this.impl = new _StandaloneCompiler();

	long endTime = System.currentTimeMillis();
	System.out.println(endTime - startTime);
    }


    public CompilationResult compile(String src) {
	Object[] results = this.impl.compileScheme(src);
	String compiledText = results[0].toString();
	Scriptable permArray = (Scriptable) results[1];
	double length = (Double) permArray.get("length", permArray);
	Set<String> permissions = new HashSet<String>();
	for (int i = 0; i < length; i++) {
	    permissions.add(permArray.get(i, permArray).toString());
	}
	return new CompilationResult(compiledText, permissions);
    }
}