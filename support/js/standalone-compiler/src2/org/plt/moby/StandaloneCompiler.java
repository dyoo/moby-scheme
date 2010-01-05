package org.plt.moby;

import org.mozilla.javascript.*;
import java.util.Set;
import java.util.HashSet;

public class StandaloneCompiler {
    private _StandaloneCompiler impl;

    public StandaloneCompiler() {
	this.impl = new _StandaloneCompiler();
    }

    public CompilationResult compile(String src) {
	Object[] results = this.impl.compile(src);
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