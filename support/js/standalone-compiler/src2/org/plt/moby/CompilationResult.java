package org.plt.moby;

import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;

public class CompilationResult {
    private String js;
    private Set<String> perms;

    public CompilationResult(String js, Set<String> perms) {
	this.js = js;
	this.perms = new HashSet<String>(perms);
    }

    public String getJs() {
	return this.js;
    }

    public Set<String> getPerms() {
	return this.perms;
    }
}