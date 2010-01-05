package org.plt.moby;

import org.mozilla.javascript.*;


public class StandaloneCompiler {
    private _StandaloneCompiler impl;

    public StandaloneCompiler() {
	this.impl = new _StandaloneCompiler();
    }

    public CompilationResult compile(String src) {
	Object[] results = this.impl.compile(src);
	return null;
    }
}