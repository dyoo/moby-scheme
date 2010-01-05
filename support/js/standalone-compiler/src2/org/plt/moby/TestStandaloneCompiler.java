package org.plt.moby;

// FIXME: make these real unit tests.
public class TestStandaloneCompiler {
    public static void main(String[] args) {
	StandaloneCompiler c = new StandaloneCompiler();
	CompilationResult r = c.compile("(define (f x a b) (* x x)) (f 42 0 1)  (js-big-bang 0 (on-location-change f))");
	System.out.println(r.getJs());
	System.out.println(r.getPerms());


	r = c.compile("(define (g y) (+ y 42)) (g 17)");
	System.out.println(r.getJs());
	System.out.println(r.getPerms());
    }
}